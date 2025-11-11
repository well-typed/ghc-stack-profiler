module GHC.Stack.Profiler.Sampler (
  -- * Run sample profiler
  withStackProfiler,
  withSampleProfilerForMyThread,
  withStackProfilerForThread,
  -- * Configuration of sample profiler
  StackProfilerManager(..),
  ProfilerSamplingInterval(..),
  -- * Basic thread sampler
  sampleThread,
  -- * Low level helpers for setting up custom
  -- sample profilers threads
  runWithStackProfiler,
  setupStackProfilerManager,
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
import Data.IORef
import GHC.Conc
import GHC.Conc.Sync

import GHC.Stack.CloneStack (cloneThreadStack)

import qualified Debug.Trace.Binary.Compat as Compat
import GHC.Stack.Profiler.Decode
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.Util

-- | A 'StackProfilerManager' records all the relevant information
-- to manage the ghc stack profiler run-time.
data StackProfilerManager = MkStackProfilerManager
  { profilerThreadId :: ThreadId
  -- ^ 'ThreadId' of the stack sampling thread.
  , symbolTableRef :: IORef SymbolTable
  } deriving (Eq)

-- | Sampling intervals for the stack profiler.
data ProfilerSamplingInterval
  = SampleIntervalMs Int
  -- ^ Sample every @n@ milliseconds.
  --
  -- Recommended value: @'SampleIntervalMs' 10@ or @'SampleIntervalMs' 20@.
  deriving (Show, Eq, Ord)

profilerSamplingIntervalToThreadDelayTime :: ProfilerSamplingInterval -> Int
profilerSamplingIntervalToThreadDelayTime = \ case
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
withStackProfiler :: ProfilerSamplingInterval -> IO a -> IO a
withStackProfiler delay act = do
  runWithStackProfiler sampleAction delay act
  where
    sampleAction config = do
      tids <- listThreads
      userThreads <- filterM (isThreadOfInterest config) tids
      forM_ userThreads $ \tid ->
        sampleToEventlog (symbolTableRef config) tid

    isThreadOfInterest :: StackProfilerManager -> ThreadId -> IO Bool
    isThreadOfInterest config tid = do
      lbl <- threadLabel tid
      pure $ not $ or
        [ isProfilerThread config tid lbl
        , isRtsThread tid lbl
        ]

-- | Sample the current thread every 'ProfilerSamplingInterval' for the duration of
-- the wrapped action.
-- Once the wrapped action terminates, the stack profiling stops.
withSampleProfilerForMyThread :: ProfilerSamplingInterval -> IO a -> IO a
withSampleProfilerForMyThread delay act = do
  tid <- myThreadId
  withStackProfilerForThread tid delay act

-- | Sample a specific 'ThreadId' every 'ProfilerSamplingInterval' for the duration of
-- the wrapped action.
-- Once the wrapped action terminates, the stack profiling stops.
withStackProfilerForThread :: ThreadId -> ProfilerSamplingInterval -> IO a -> IO a
withStackProfilerForThread tid delay act =
  runWithStackProfiler (\ config -> (sampleToEventlog (symbolTableRef config) tid)) delay act

-- ----------------------------------------------------------------------------
-- Low-level user API
-- ----------------------------------------------------------------------------

runWithStackProfiler :: (StackProfilerManager -> IO ()) -> ProfilerSamplingInterval -> IO a -> IO a
runWithStackProfiler sampleAction delay act = do
  if Compat.userTracingEnabled
    then bracket (setupStackProfilerManager sampleAction delay) stopStackProfilerManager (const act)
    else act

stopStackProfilerManager :: StackProfilerManager -> IO ()
stopStackProfilerManager MkStackProfilerManager{profilerThreadId} =
  killThread profilerThreadId

setupStackProfilerManager :: (StackProfilerManager -> IO ()) -> ProfilerSamplingInterval -> IO StackProfilerManager
setupStackProfilerManager sampleAction delay = do
  samplerThreadConfigMVar <- newEmptyMVar
  sid <- forkIO $ do
    config <- takeMVar samplerThreadConfigMVar
    sampleThreadId <- myThreadId
    labelThread sampleThreadId "Sample Profiler Thread"
    forever $ do
      sampleAction config
      -- TODO: this is wrong, we don't sample every delay time as sampling takes time as well
      threadDelay (profilerSamplingIntervalToThreadDelayTime delay)

  tableRef <- newIORef emptyMapSymbolTableWriter
  let sampleThreadConf = MkStackProfilerManager
        { profilerThreadId = sid
        , symbolTableRef = tableRef
        }
  putMVar samplerThreadConfigMVar sampleThreadConf
  pure sampleThreadConf

-- | We don't want to sample the stack profiler itself.
isProfilerThread :: StackProfilerManager -> ThreadId -> Maybe String -> Bool
isProfilerThread MkStackProfilerManager {profilerThreadId} tid _lbl = tid == profilerThreadId

-- | RTS threads are often not that interesting, we much rather want to focus on
-- the user code.
isRtsThread :: ThreadId -> Maybe String -> Bool
isRtsThread _    Nothing    = False
isRtsThread _tid (Just lbl) =
  lbl `elem` ["TimerManager"] || "IOManager on cap" `List.isPrefixOf` lbl

-- | Sample the stack of the 'ThreadId' if the thread is currently running.
-- If the thread is not running (e.g., because it is dead), then we return 'Nothing'.
sampleThread :: ThreadId -> IO (Maybe ThreadSample)
sampleThread tid = do
  tidStatus <- threadStatus tid
  (cap, _lockedToCap) <- threadCapability tid
  case tidStatus of
    ThreadRunning -> do
      stack <- cloneThreadStack tid
      pure $ Just $ ThreadSample
        { threadSampleId = tid
        , threadSampleCapability = MkCapabilityId $ intToWord64 cap
        , threadSampleStackSnapshot = stack
        }
    _ -> do
      -- Only running threads need to be sampled
      pure Nothing

-- | If the thread's callstack can be sampled, we serialise the sample
-- and write into the eventlog for later processing.
sampleToEventlog :: IORef SymbolTable -> ThreadId -> IO ()
sampleToEventlog tableRef tid = do
  sampleThread tid >>= \ case
    Nothing -> pure ()
    Just threadSample -> do
      msgs <- serializeThreadSample tableRef threadSample
      mapM_ (Compat.traceBinaryEventIO . LBS.toStrict) msgs
