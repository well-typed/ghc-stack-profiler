module GHC.Stack.Profiler (
  -- * Run sample profiler
  withStackProfiler,
  withStackProfilerForMyThread,
  withStackProfilerForThread,
  setupRootStackProfiler,

  -- * Configuration of sample profiler
  StackProfilerManager (..),
  ProfilerSamplingInterval (..),

  -- * Basic thread sampler
  sampleThread,

  -- * Low level helpers for setting up custom

  -- sample profilers threads
  runWithStackProfiler,
  setupStackProfilerThread,
  stopStackProfilerManager,
  sampleToEventlog,

  -- * Thread filtering
  isProfilerThread,
  isRtsThread,
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import GHC.Conc
import GHC.Conc.Sync (fromThreadId, threadLabel)

import GHC.Stack.CloneStack (cloneThreadStack)

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM
import Data.Foldable (traverse_)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace.Binary.Compat as Compat
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.Util
import GHC.Stack.Profiler.Decode
import qualified GHC.Stack.Profiler.FFI as FFI
import GHC.Stack.Profiler.Sampler

-- | Sampling intervals for the stack profiler.
data ProfilerSamplingInterval
  = -- | Sample every @n@ milliseconds.
    --
    -- Recommended value: @'SampleIntervalMs' 10@ or @'SampleIntervalMs' 20@.
    SampleIntervalMs Int
  deriving (Show, Eq, Ord)

profilerSamplingIntervalToThreadDelayTime :: ProfilerSamplingInterval -> Int
profilerSamplingIntervalToThreadDelayTime = \case
  SampleIntervalMs n -> n * 1000

-- ----------------------------------------------------------------------------
-- High-Level user API
-- ----------------------------------------------------------------------------

-- | Sample the all non-rts threads every 'ProfilerSamplingInterval' for the duration of
-- the wrapped action.
-- Once the wrapped action terminates, the stack profiling stops.
--
-- RTS threads such as the 'TimerManager' and 'IOManager' are not sampled as these
-- are usually not interesting for user code.
withStackProfiler :: StackProfilerManager -> ProfilerSamplingInterval -> IO a -> IO a
withStackProfiler manager delay act = do
  runWithStackProfiler manager sampleAction delay act
 where
  sampleAction = do
    tids <- listThreads
    userThreads <- filterM (isThreadOfInterest manager) tids
    forM_ userThreads $ \tid ->
      sampleToEventlog manager tid

  isThreadOfInterest :: StackProfilerManager -> ThreadId -> IO Bool
  isThreadOfInterest config tid = do
    lbl <- threadLabel tid
    threads <- readTVarIO (profilerThreads config)
    pure $
      not $
        or
          [ isProfilerThread threads tid
          , isRtsThread tid lbl
          ]

-- | Sample the current thread every 'ProfilerSamplingInterval' for the duration of
-- the wrapped action.
-- Once the wrapped action terminates, the stack profiling stops.
withStackProfilerForMyThread :: StackProfilerManager -> ProfilerSamplingInterval -> IO a -> IO a
withStackProfilerForMyThread manager delay act = do
  tid <- myThreadId
  withStackProfilerForThread manager tid delay act

-- | Sample a specific 'ThreadId' every 'ProfilerSamplingInterval' for the duration of
-- the wrapped action.
-- Once the wrapped action terminates, the stack profiling stops.
withStackProfilerForThread :: StackProfilerManager -> ThreadId -> ProfilerSamplingInterval -> IO a -> IO a
withStackProfilerForThread manager tid delay act =
  runWithStackProfiler manager (sampleToEventlog manager tid) delay act

setupRootStackProfiler :: Bool -> (StackProfilerManager -> IO a) -> IO ()
setupRootStackProfiler shouldRun act =
  bracket
    ( do
        manager <- newStackProfilerManager shouldRun
        FFI.installEventlogSocketHandlers manager
        pure manager
    )
    act
    stopStackProfilerManager

-- ----------------------------------------------------------------------------
-- Low-level user API
-- ----------------------------------------------------------------------------

runWithStackProfiler :: StackProfilerManager -> IO () -> ProfilerSamplingInterval -> IO a -> IO a
runWithStackProfiler manager sampleAction delay act = do
  if Compat.userTracingEnabled
    then bracket (setupStackProfilerThread manager sampleAction delay) (uncurry stopStackProfilerThread) (const act)
    else act

stopStackProfilerManager :: StackProfilerManager -> IO ()
stopStackProfilerManager MkStackProfilerManager{isShuttingDown, profilerThreads} = do
  threads <- atomically $ do
    writeTVar isShuttingDown True
    readTVar profilerThreads
  traverse_ killThread threads

stopStackProfilerThread :: ThreadId -> StackProfilerManager -> IO ()
stopStackProfilerThread profilerId MkStackProfilerManager{profilerThreads} = do
  killThread profilerId
    `finally` atomically
      ( do
          STM.modifyTVar' profilerThreads (Set.delete profilerId)
      )

setupStackProfilerThread :: StackProfilerManager -> IO () -> ProfilerSamplingInterval -> IO (ThreadId, StackProfilerManager)
setupStackProfilerThread manager sampleAction delay = do
  barrier <- newEmptyMVar
  sid <- forkIO $ do
    () <- takeMVar barrier
    sampleThreadId <- myThreadId
    labelThread sampleThreadId ("Sample Profiler Thread " <> show (fromThreadId sampleThreadId))
    forever $ do
      atomically $ do
        STM.check =<< readTVar (isRunning manager)
      sampleAction
      -- TODO: this is wrong, we don't sample every delay time as sampling takes time as well
      threadDelay (profilerSamplingIntervalToThreadDelayTime delay)

  atomically $ do
    STM.modifyTVar' (profilerThreads manager) $ \threads ->
      Set.insert sid threads
  putMVar barrier ()
  pure (sid, manager)

-- | We don't want to sample the stack profiler threads themselves.
isProfilerThread :: Set ThreadId -> ThreadId -> Bool
isProfilerThread threads tid =
  Set.member tid threads

-- | RTS threads are often not that interesting, we much rather want to focus on
-- the user code.
isRtsThread :: ThreadId -> Maybe String -> Bool
isRtsThread _ Nothing = False
isRtsThread _tid (Just lbl) =
  lbl == "TimerManager" || "IOManager on cap" `List.isPrefixOf` lbl

-- | Sample the stack of the 'ThreadId' if the thread is currently running.
-- If the thread is not running (e.g., because it is dead), then we return 'Nothing'.
sampleThread :: ThreadId -> IO (Maybe ThreadSample)
sampleThread tid = do
  tidStatus <- threadStatus tid
  (cap, _lockedToCap) <- threadCapability tid
  case canCloneStack tidStatus of
    True -> do
      stack <- cloneThreadStack tid
      pure $
        Just $
          ThreadSample
            { threadSampleId = tid
            , threadSampleCapability = MkCapabilityId $ intToWord64 cap
            , threadSampleStackSnapshot = stack
            }
    False -> do
      -- Only running threads need to be sampled
      pure Nothing
 where
  canCloneStack :: ThreadStatus -> Bool
  canCloneStack = \case
    ThreadRunning -> True
    ThreadBlocked BlockedOnMVar -> True
    _ -> False

-- | If the thread's callstack can be sampled, we serialise the sample
-- and write into the eventlog for later processing.
sampleToEventlog :: StackProfilerManager -> ThreadId -> IO ()
sampleToEventlog manager tid = do
  sampleThread tid >>= \case
    Nothing -> pure ()
    Just threadSample -> do
      msgs <- serializeThreadSample (symbolTableRef manager) threadSample
      mapM_ (Compat.traceBinaryEventIO . LBS.toStrict) msgs
