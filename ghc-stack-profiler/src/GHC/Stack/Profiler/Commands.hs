module GHC.Stack.Profiler.Commands (
  startProfiling,
  stopProfiling,
  sendPublishInitEventMessages,
  sendStartProfilingMessage,
  sendStopProfilingMessage,
  sendEnableEventlogMessage,
  sendDisableEventlogMessage,
) where

import Control.Concurrent.Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import GHC.Stack.Profiler.Manager

-- | Start the profiler threads.
--
-- Blocks until all threads started running.
startProfiling :: StackProfilerManager -> IO ()
startProfiling manager = do
  -- TODO: this atomically is redundant, the main loop thread
  -- sets it anyway
  STM.atomically $
    STM.writeTVar (isThreadSamplerRunning manager) True
  sendStartProfilingMessage manager

-- | Stop the running profiler threads.
--
-- Blocks until all threads stopped running.
stopProfiling :: StackProfilerManager -> IO ()
stopProfiling manager = do
  -- TODO: this atomically is *not* redundant, it makes sure no new
  -- samples can be created.
  -- Otherwise, new samples could be created and queued while we are waiting
  -- for the event loop to process this message.
  -- It is important, that once this message is processed, that no sampler thread is sampling
  -- at all. Otherwise, there will be new init events that are not published.
  STM.atomically $
    STM.writeTVar (isThreadSamplerRunning manager) False
  sendStopProfilingMessage manager

-- | Start profiling.
--
-- Blocks until the message has been processed by the main event loop.
sendStartProfilingMessage :: StackProfilerManager -> IO ()
sendStartProfilingMessage manager = do
  barrier <- MVar.newEmptyMVar
  writeChan
    (messageChan manager)
    (StartProfiling barrier)
  MVar.takeMVar barrier

-- | Stop profiling.
--
-- Blocks until the message has been processed by the main event loop.
sendStopProfilingMessage :: StackProfilerManager -> IO ()
sendStopProfilingMessage manager = do
  barrier <- MVar.newEmptyMVar
  writeChan
    (messageChan manager)
    (StopProfiling barrier)
  MVar.takeMVar barrier

-- | Start EventLogging now.
--
-- Blocks until the message has been processed by the main event loop.
sendEnableEventlogMessage :: StackProfilerManager -> IO ()
sendEnableEventlogMessage manager = do
  barrier <- MVar.newEmptyMVar
  writeChan
    (messageChan manager)
    (StartEventlog barrier)
  MVar.takeMVar barrier

-- | Stop EventLogging now.
--
-- Blocks until the message has been processed by the main event loop.
sendDisableEventlogMessage :: StackProfilerManager -> IO ()
sendDisableEventlogMessage manager = do
  barrier <- MVar.newEmptyMVar
  writeChan
    (messageChan manager)
    (StopEventlog barrier)
  MVar.takeMVar barrier

-- | Publish all init messages so far.
--
-- Blocks until the init events have been written to the eventlog and
-- eventlog was flushed.
sendPublishInitEventMessages :: StackProfilerManager -> IO ()
sendPublishInitEventMessages manager = do
  barrier <- MVar.newEmptyMVar
  writeChan
    (messageChan manager)
    (PublishInitEvents barrier)
  MVar.takeMVar barrier
