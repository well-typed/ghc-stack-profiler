module GHC.Stack.Profiler.Manager (
  Manager (..),
  newManager,
  stopManager,
  shouldProfile,
  enableEventLogging,
  disableEventLogging,
  enableSampling,
  disableSampling,
  registerSamplerThread,
  unregisterSamplerThread,
  stopAllSamplerThreads,

  -- * Sampler Threads
  Sampler (..),
  cancelSampler,

  -- * Event Loop
  EventLoop (..),
  startEventLoop,
  stopEventLoop,

  -- * Control Messages
  ControlMessage (..),
  startProfiling,
  stopProfiling,
  sendPublishInitEventMessages,
  sendStartProfilingMessage,
  sendStopProfilingMessage,
  sendEnableEventlogMessage,
  sendDisableEventlogMessage,
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async (..), async, cancel, link)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Debug.Trace
import qualified Debug.Trace.Binary.Compat as Compat
import GHC.Generics (Generic)
import qualified GHC.Stack.Profiler.Decode as Decode
import GHC.Stack.Profiler.SymbolTable

-- NOTE: Part of the public API.

-- | A `Manager` handle, which can be used to stop the manager with `stopManager`.
data Manager = MkManager
  { samplerThreadMapVar :: !(TVar (Map ThreadId Sampler))
  -- ^ 'Async' of the stack sampling thread.
  , eventLoopThreadVar :: !(TVar (Maybe EventLoop))
  -- ^ Main event loop thread responsible for processing profiler messages, etc...
  , symbolTableRef :: !StackSymbolTable
  -- ^ Global table for common symbols.
  , shouldSampleVar :: !(TVar Bool)
  -- ^ Is the profiler currently running?
  --
  -- Can be controlled via 'startProfiler' and 'stopProfiler'.
  -- This variable describes whether the user wants to profile, regardless
  -- of the eventlog state.
  , eventLoggingStartedVar :: !(TVar Bool)
  -- ^ Is there an eventlog?
  --
  -- It is fully possible that we start profiling but no eventlog-writer
  -- being connected/configured. The eventlog can be enabled at a later point,
  -- or stopped/started via @eventlog-socket@.
  -- This variable tracks the state of the eventlog-writer.
  , messageChan :: Chan ControlMessage
  }
  deriving (Generic, Eq)

newManager :: Bool -> IO Manager
newManager running = do
  tracingEnabled <- Compat.userTracingEnabledIO
  MkManager
    <$> newTVarIO Map.empty
    <*> newTVarIO Nothing
    <*> emptySymbolTableIO
    <*> newTVarIO running
    <*> newTVarIO tracingEnabled
    <*> newChan

-- NOTE: Part of the public API.

-- | Stop a `Manager`.
--
--   This also stops every `Sampler` started by this manager.
stopManager :: Manager -> IO ()
stopManager manager = do
  stopAllSamplerThreads manager
  stopEventLoop manager

-- | Can we profile right now?
--
-- We only sample a stack if the profiler is instructed to run and the eventlog is enabled.
shouldProfile :: Manager -> STM Bool
shouldProfile manager =
  liftA2
    (&&)
    (readTVar $ shouldSampleVar manager)
    (readTVar $ eventLoggingStartedVar manager)

enableEventLogging :: Manager -> STM ()
enableEventLogging manager = do
  TVar.writeTVar (eventLoggingStartedVar manager) True

disableEventLogging :: Manager -> STM ()
disableEventLogging manager = do
  TVar.writeTVar (eventLoggingStartedVar manager) False

enableSampling :: Manager -> STM ()
enableSampling manager = do
  TVar.writeTVar (shouldSampleVar manager) True

disableSampling :: Manager -> STM ()
disableSampling manager = do
  TVar.writeTVar (shouldSampleVar manager) False

registerSamplerThread :: Manager -> Sampler -> IO ()
registerSamplerThread manager samplerThread@MkSampler{samplerAsync} = do
  link samplerAsync -- If the sampler crashes, we want to know.
  atomically $ do
    STM.modifyTVar' (samplerThreadMapVar manager) $ \threadMap ->
      (Map.insert (asyncThreadId samplerAsync) samplerThread threadMap)

unregisterSamplerThread :: Manager -> Sampler -> IO ()
unregisterSamplerThread manager MkSampler{samplerAsync} =
  atomically $ do
    STM.modifyTVar'
      (samplerThreadMapVar manager)
      (Map.delete (asyncThreadId samplerAsync))

stopAllSamplerThreads :: Manager -> IO ()
stopAllSamplerThreads manager = do
  samplerThreads <-
    atomically $ do
      samplerThreadMap <- readTVar (samplerThreadMapVar manager)
      writeTVar (samplerThreadMapVar manager) Map.empty
      pure $ Map.elems samplerThreadMap
  for_ samplerThreads cancelSampler

-------------------------------------------------------------------------------
-- Sampler Threads
-------------------------------------------------------------------------------

newtype Sampler = MkSampler
  { samplerAsync :: Async ()
  }

cancelSampler :: Sampler -> IO ()
cancelSampler MkSampler{samplerAsync} =
  cancel samplerAsync

-------------------------------------------------------------------------------
-- Event Loop
-------------------------------------------------------------------------------

newtype EventLoop = MkEventLoop
  { eventLoopAsync :: Async ()
  }

data ControlMessage
  = WriteProfileSample [ByteString]
  | PublishInitEvents (MVar ())
  | StartProfiling (MVar ())
  | StopProfiling (MVar ())
  | StartEventlog (MVar ())
  | StopEventlog (MVar ())

startEventLoop :: Manager -> IO ()
startEventLoop manager = do
  !eventLoopThread <- do
    eventLoopAsync <- async $ forever $ eventHandler manager
    link eventLoopAsync -- If the event loop crashes, we want to know.
    pure $ MkEventLoop{eventLoopAsync}
  atomically $ do
    writeTVar (eventLoopThreadVar manager) (Just eventLoopThread)

eventHandler :: Manager -> IO ()
eventHandler manager = do
  msg <- readChan (messageChan manager)
  run <- atomically $ shouldProfile manager
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
      atomically $ enableSampling manager
      putMVar barrier ()
    StopProfiling barrier -> do
      atomically $ disableSampling manager
      putMVar barrier ()
    StartEventlog barrier -> do
      atomically $ enableEventLogging manager
      putMVar barrier ()
    StopEventlog barrier -> do
      atomically $ disableEventLogging manager
      putMVar barrier ()
    PublishInitEvents barrier -> do
      symbolTable <- atomically $ readSymbolTable (symbolTableRef manager)
      let
        binaryMessages = Decode.initMessages symbolTable

      for_ binaryMessages $ \binaryMessage ->
        Compat.traceBinaryEventIO (BSL.toStrict binaryMessage)

      Debug.Trace.flushEventLog
      putMVar barrier ()

stopEventLoop :: Manager -> IO ()
stopEventLoop manager = do
  maybeEventThread <- atomically $ stateTVar (eventLoopThreadVar manager) (,Nothing)
  sendStopProfilingMessage manager
  for_ maybeEventThread $ \(MkEventLoop eventThread) ->
    cancel eventThread

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | Start the profiler threads.
--
-- Blocks until all threads started running.
startProfiling :: Manager -> IO ()
startProfiling manager = do
  -- TODO: this atomically is redundant, the main loop thread
  -- sets it anyway
  atomically $
    writeTVar (shouldSampleVar manager) True
  sendStartProfilingMessage manager

-- | Stop the running profiler threads.
--
-- Blocks until all threads stopped running.
stopProfiling :: Manager -> IO ()
stopProfiling manager = do
  -- TODO: this atomically is *not* redundant, it makes sure no new
  -- samples can be created.
  -- Otherwise, new samples could be created and queued while we are waiting
  -- for the event loop to process this message.
  -- It is important, that once this message is processed, that no sampler thread is sampling
  -- at all. Otherwise, there will be new init events that are not published.
  atomically $
    writeTVar (shouldSampleVar manager) False
  sendStopProfilingMessage manager

-- | Start profiling.
--
-- Blocks until the message has been processed by the main event loop.
sendStartProfilingMessage :: Manager -> IO ()
sendStartProfilingMessage manager = do
  barrier <- newEmptyMVar
  writeChan
    (messageChan manager)
    (StartProfiling barrier)
  takeMVar barrier

-- | Stop profiling.
--
-- Blocks until the message has been processed by the main event loop.
sendStopProfilingMessage :: Manager -> IO ()
sendStopProfilingMessage manager = do
  barrier <- newEmptyMVar
  writeChan
    (messageChan manager)
    (StopProfiling barrier)
  takeMVar barrier

-- | Start EventLogging now.
--
-- Blocks until the message has been processed by the main event loop.
sendEnableEventlogMessage :: Manager -> IO ()
sendEnableEventlogMessage manager = do
  barrier <- newEmptyMVar
  writeChan
    (messageChan manager)
    (StartEventlog barrier)
  takeMVar barrier

-- | Stop EventLogging now.
--
-- Blocks until the message has been processed by the main event loop.
sendDisableEventlogMessage :: Manager -> IO ()
sendDisableEventlogMessage manager = do
  barrier <- newEmptyMVar
  writeChan
    (messageChan manager)
    (StopEventlog barrier)
  takeMVar barrier

-- | Publish all init messages so far.
--
-- Blocks until the init events have been written to the eventlog and
-- eventlog was flushed.
sendPublishInitEventMessages :: Manager -> IO ()
sendPublishInitEventMessages manager = do
  barrier <- newEmptyMVar
  writeChan
    (messageChan manager)
    (PublishInitEvents barrier)
  takeMVar barrier
