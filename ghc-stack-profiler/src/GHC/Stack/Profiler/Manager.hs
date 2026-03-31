module GHC.Stack.Profiler.Manager (
  StackProfilerManager (..),
  newStackProfilerManager,
  shouldProfile,
  EventThread (..),
  ProfilerMessage (..),
  enableEventLogging,
  disableEventLogging,
  enableSampling,
  disableSampling,
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar
import qualified Control.Concurrent.STM.TVar as TVar
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Debug.Trace.Binary.Compat as Compat
import GHC.Generics (Generic)
import GHC.Stack.Profiler.SymbolTable

-- | A 'StackProfilerManager' records all the relevant information
-- to manage the ghc stack profiler run-time.
data StackProfilerManager = MkStackProfilerManager
  { profilerThreads :: !(TVar (Map ThreadId (Async ())))
  -- ^ 'Async' of the stack sampling thread.
  , mainEventLoopThread :: !(TVar (Maybe EventThread))
  -- ^ Main event loop thread responsible for processing profiler messages, etc...
  , symbolTableRef :: !StackSymbolTable
  -- ^ Global table for common symbols.
  , isThreadSamplerRunning :: !(TVar Bool)
  -- ^ Is the profiler currently running?
  --
  -- Can be controlled via 'startProfiler' and 'stopProfiler'.
  -- This variable describes whether the user wants to profile, regardless
  -- of the eventlog state.
  , isEventlogStarted :: !(TVar Bool)
  -- ^ Is there an eventlog?
  --
  -- It is fully possible that we start profiling but no eventlog-writer
  -- being connected/configured. The eventlog can be enabled at a later point,
  -- or stopped/started via @eventlog-socket@.
  -- This variable tracks the state of the eventlog-writer.
  , messageChan :: Chan ProfilerMessage
  }
  deriving (Generic, Eq)

data ProfilerMessage
  = WriteProfileSample [ByteString]
  | PublishInitEvents (MVar ())
  | StartProfiling (MVar ())
  | StopProfiling (MVar ())
  | StartEventlog (MVar ())
  | StopEventlog (MVar ())

newStackProfilerManager :: Bool -> IO StackProfilerManager
newStackProfilerManager running = do
  tracingEnabled <- Compat.userTracingEnabledIO
  MkStackProfilerManager
    <$> newTVarIO Map.empty
    <*> newTVarIO Nothing
    <*> emptySymbolTableIO
    <*> newTVarIO running
    <*> newTVarIO tracingEnabled
    <*> newChan

data EventThread = MkEventThread
  { eventThread :: !(Async ())
  }

-- | Can we profile right now?
--
-- We only sample a stack if the profiler is instructed to run and the eventlog is enabled.
shouldProfile :: StackProfilerManager -> STM Bool
shouldProfile manager =
  liftA2
    (&&)
    (readTVar $ isThreadSamplerRunning manager)
    (readTVar $ isEventlogStarted manager)

enableEventLogging :: StackProfilerManager -> STM ()
enableEventLogging manager = do
  TVar.writeTVar (isEventlogStarted manager) True

disableEventLogging :: StackProfilerManager -> STM ()
disableEventLogging manager = do
  TVar.writeTVar (isEventlogStarted manager) False

enableSampling :: StackProfilerManager -> STM ()
enableSampling manager = do
  TVar.writeTVar (isThreadSamplerRunning manager) True

disableSampling :: StackProfilerManager -> STM ()
disableSampling manager = do
  TVar.writeTVar (isThreadSamplerRunning manager) False
