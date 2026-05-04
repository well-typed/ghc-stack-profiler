module GHC.Stack.Profiler (
  -- * Run sample profiler
  withStackProfiler,
  withStackProfilerForMyThread,
  withStackProfilerForThread,
  withRootStackProfiler,
  shutdownStackProfilerManager,

  -- * Configuration of sample profiler
  StackProfilerManager (..),
  ProfilerSamplingInterval (..),

  -- * Basic thread sampler
  sampleThread,

  -- * Low level helpers for setting up custom sample profilers threads
  runWithStackProfiler,
  setupStackProfilerThread,
  stopStackProfilerThread,

  -- * Thread filtering
  isProfilerThread,
  isRtsThread,
) where

import GHC.Conc
import GHC.Conc.Sync (fromThreadId, threadLabel)
import GHC.Stack.CloneStack (cloneThreadStack)

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.STM.TVar as STM
import Control.Exception
import Control.Monad
import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace
import qualified Debug.Trace.Binary.Compat as Compat

import GHC.Stack.Profiler.Commands (sendStopProfilingMessage)
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.Util
import GHC.Stack.Profiler.Decode
import qualified GHC.Stack.Profiler.Decode as Decode
import qualified GHC.Stack.Profiler.Eventlog.Socket as EventlogSocket
import GHC.Stack.Profiler.Manager
import GHC.Stack.Profiler.SymbolTable (readSymbolTable)

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
  runWithStackProfiler
    manager
    (allThreadSampler manager delay)
    (defaultCallStackSerialiser manager)
    act

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
  runWithStackProfiler
    manager
    (singleThreadSampler manager delay tid)
    (defaultCallStackSerialiser manager)
    act

withRootStackProfiler :: Bool -> (StackProfilerManager -> IO a) -> IO a
withRootStackProfiler shouldRun act =
  bracket
    (runNewStackProfilerManager shouldRun)
    shutdownStackProfilerManager
    act

-- ----------------------------------------------------------------------------
-- Low-level user API
-- ----------------------------------------------------------------------------

runNewStackProfilerManager :: Bool -> IO StackProfilerManager
runNewStackProfilerManager shouldRun = do
  manager <- newStackProfilerManager shouldRun
  startEventLoopThread manager
  EventlogSocket.registerWithEventlogSocket manager
  pure manager

shutdownStackProfilerManager :: StackProfilerManager -> IO ()
shutdownStackProfilerManager manager = do
  shutdownAllSamplerThreads manager
  -- TODO: we could also send a stop command instead
  shutdownEventLoop manager

runWithStackProfiler :: StackProfilerManager -> ThreadSampler -> CallStackSerialiser -> IO a -> IO a
runWithStackProfiler manager sampler serializer act = do
  bracket
    (setupStackProfilerThread manager sampler serializer)
    (stopStackProfilerThread manager)
    (const act)

stopStackProfilerThread :: StackProfilerManager -> Async () -> IO ()
stopStackProfilerThread MkStackProfilerManager{profilerThreads} profilerThread = do
  cancel profilerThread
    `finally` atomically
      ( do
          STM.modifyTVar'
            profilerThreads
            ( \threadMap ->
                (Map.delete (asyncThreadId profilerThread) threadMap)
            )
      )

setupStackProfilerThread ::
  StackProfilerManager ->
  ThreadSampler ->
  CallStackSerialiser ->
  IO (Async ())
setupStackProfilerThread manager sampler serialiser = do
  barrier <- newEmptyMVar
  workerThread <- async $ do
    () <- takeMVar barrier
    sampleThreadId <- myThreadId
    labelThread sampleThreadId ("Sample Profiler Thread " <> show (fromThreadId sampleThreadId))
    forever $ do
      runStackProfilerSample sampler serialiser

  -- Add this thread to the list of known worker threads to make sure it isn't accidentally sampled
  addSamplerThread manager workerThread
  putMVar barrier ()
  pure workerThread

-- ----------------------------------------------------------------------------
-- Sample the RTS CallStack of one or more threads
-- ----------------------------------------------------------------------------

data ThreadSampler = MkThreadSampler
  { listThreadsToSample :: IO [ThreadId]
  , delaySamplerThread :: IO ()
  , waitForProfilingStart :: IO ()
  }

defaultThreadSampler :: StackProfilerManager -> ProfilerSamplingInterval -> ThreadSampler
defaultThreadSampler manager delay =
  MkThreadSampler
    { listThreadsToSample = do
        pure []
    , delaySamplerThread =
        threadDelay (profilerSamplingIntervalToThreadDelayTime delay)
    , waitForProfilingStart =
        atomically $ do
          STM.check =<< shouldProfile manager
    }

singleThreadSampler :: StackProfilerManager -> ProfilerSamplingInterval -> ThreadId -> ThreadSampler
singleThreadSampler manager delay tid =
  (defaultThreadSampler manager delay)
    { listThreadsToSample = do
        pure [tid]
    }

allThreadSampler :: StackProfilerManager -> ProfilerSamplingInterval -> ThreadSampler
allThreadSampler manager delay =
  (defaultThreadSampler manager delay)
    { listThreadsToSample = do
        tids <- listThreads
        userThreads <- filterM (isThreadOfInterest manager) tids
        pure userThreads
    }

runStackProfilerSample :: ThreadSampler -> CallStackSerialiser -> IO ()
runStackProfilerSample sampler serialiser = do
  waitForProfilingStart sampler
  tids <- listThreadsToSample sampler
  mapM_ (runCallStackSerialiser serialiser) tids
  -- TODO: this is wrong, we don't sample every delay time as sampling takes time as well
  delaySamplerThread sampler

-- ----------------------------------------------------------------------------
-- Serialise the RTS CallStack for the eventlog
-- ----------------------------------------------------------------------------

data CallStackSerialiser = MkCallStackSerialiser
  { sampleCallStack :: ThreadId -> IO (Maybe ThreadSample)
  , decodeThreadSample :: ThreadSample -> IO CallStackMessage
  , serialiseCallStackMessage :: CallStackMessage -> IO ()
  }

-- | If the thread's callstack can be sampled, we serialise the sample
-- and write into the eventlog for later processing.
runCallStackSerialiser :: CallStackSerialiser -> ThreadId -> IO ()
runCallStackSerialiser serialiser tid = do
  sampleCallStack serialiser tid >>= \case
    Nothing -> pure ()
    Just threadSample -> do
      callStackSample <- decodeThreadSample serialiser threadSample
      serialiseCallStackMessage serialiser callStackSample

defaultCallStackSerialiser :: StackProfilerManager -> CallStackSerialiser
defaultCallStackSerialiser manager =
  MkCallStackSerialiser
    { sampleCallStack = sampleThread
    , decodeThreadSample = threadSampleToCallStackMessage
    , serialiseCallStackMessage = \callStackSample -> do
        lbss <- atomically $ do
          eventlogMessages <- serializeCallStackMessage (symbolTableRef manager) callStackSample
          let
            lbss = serializeBinaryEventlogMessages eventlogMessages
          -- Only write this message if we are still profiling
          STM.check =<< shouldProfile manager
          pure lbss

        writeChan (messageChan manager) (WriteProfileSample $ fmap LBS.toStrict lbss)
    }

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

-- ----------------------------------------------------------------------------
-- Main Event Loop handler
-- ----------------------------------------------------------------------------

startEventLoopThread :: StackProfilerManager -> IO ()
startEventLoopThread manager = do
  !sinkAsync <- do
    sinkAsync <- async (forever mainEventHandler)

    -- if the main eventloop crashes for any reason, we want to know
    link sinkAsync

    pure
      MkEventThread
        { eventThread = sinkAsync
        }

  atomically $ do
    writeTVar (mainEventLoopThread manager) (Just sinkAsync)
 where
  mainEventHandler = do
    msg <- Chan.readChan (messageChan manager)
    run <- STM.atomically $ shouldProfile manager
    case msg of
      WriteProfileSample msgs ->
        case run of
          True ->
            mapM_ Compat.traceBinaryEventIO msgs
          False ->
            -- If we received a sample but the eventlog is currently locked
            -- discard the message.
            pure ()
      StartProfiling barrier -> do
        STM.atomically $ enableSampling manager
        putMVar barrier ()
      StopProfiling barrier -> do
        STM.atomically $ disableSampling manager
        putMVar barrier ()
      StartEventlog barrier -> do
        STM.atomically $ enableEventLogging manager
        putMVar barrier ()
      StopEventlog barrier -> do
        STM.atomically $ disableEventLogging manager
        putMVar barrier ()
      PublishInitEvents barrier -> do
        symbolTable <- STM.atomically $ readSymbolTable (symbolTableRef manager)
        let
          msgs = Decode.initMessages symbolTable

        mapM_
          Compat.traceBinaryEventIO
          (fmap LBS.toStrict msgs)

        Debug.Trace.flushEventLog
        putMVar barrier ()

---------------------------------------------------------------------
-- Coordination utils
-- ----------------------------------------------------------------------------

shutdownEventLoop :: StackProfilerManager -> IO ()
shutdownEventLoop manager = do
  sinkAsync <- atomically $ do
    thread <- readTVar (mainEventLoopThread manager)
    writeTVar (mainEventLoopThread manager) Nothing
    pure thread

  sendStopProfilingMessage manager
  traverse_ (cancel . eventThread) sinkAsync

addSamplerThread :: StackProfilerManager -> Async () -> IO ()
addSamplerThread manager worker = do
  -- if the worker crashes for any reason, we want to know
  link worker

  atomically $ do
    STM.modifyTVar' (profilerThreads manager) $ \threadMap ->
      (Map.insert (asyncThreadId worker) worker threadMap)

shutdownAllSamplerThreads :: StackProfilerManager -> IO ()
shutdownAllSamplerThreads MkStackProfilerManager{profilerThreads} = do
  threads <- atomically $ do
    threadsMap <- readTVar profilerThreads
    writeTVar profilerThreads Map.empty
    pure $ Map.elems threadsMap

  traverse_ cancel threads

-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

-- | We don't want to sample the stack profiler threads themselves.
isProfilerThread :: Maybe EventThread -> Set ThreadId -> ThreadId -> Bool
isProfilerThread writerThread profilerThreadIds tid =
  Set.member tid profilerThreadIds
    || maybe False (== tid) (asyncThreadId . eventThread <$> writerThread)

-- | RTS threads are often not that interesting, we much rather want to focus on
-- the user code.
isRtsThread :: ThreadId -> Maybe String -> Bool
isRtsThread _ Nothing = False
isRtsThread _tid (Just lbl) =
  lbl == "TimerManager" || "IOManager on cap" `List.isPrefixOf` lbl

isThreadOfInterest :: StackProfilerManager -> ThreadId -> IO Bool
isThreadOfInterest manager tid = do
  lbl <- threadLabel tid
  (profilerThreadIds, eventlogWriter) <- STM.atomically $ do
    threadMap <- readTVar (profilerThreads manager)
    sink <- readTVar (mainEventLoopThread manager)
    pure (Map.keysSet threadMap, sink)
  pure $
    not $
      or
        [ isProfilerThread eventlogWriter profilerThreadIds tid
        , isRtsThread tid lbl
        ]
