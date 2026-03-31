{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GHC.Stack.Profiler.FFI (
  installEventlogSocketHandlers,
  defaultErrorHandlers,
) where

#if defined(EVENTLOG_SOCKET_SUPPORT)
import GHC.Eventlog.Socket
import System.IO (hPutStrLn, stderr)
#endif

import Control.Exception
import qualified Control.Monad.STM as STM
import GHC.Stack.Profiler.Manager
import GHC.Stack.Profiler.Commands

#if defined(EVENTLOG_SOCKET_SUPPORT)
startProfilerCommandId :: CommandId
startProfilerCommandId = CommandId 0x1

stopProfilerCommandId :: CommandId
stopProfilerCommandId = CommandId 0x2
#endif

-- | Install the @eventlog-socket@ custom command handlers and lifecycle hooks.
--
-- The supported custom commands are:
-- * Start the profiler
-- * Stop the profiler
--
-- We implement the lifecycle hooks for stopping the eventlog and starting
-- writing to the eventlog.
-- When we start eventlogging, we post the definitions of the existing callstack
-- definitions, e.g., string and source locations.
--
-- May throw 'EventlogSocketControlError' when registering @eventlog-socket@
-- hooks fails.
installEventlogSocketHandlers :: StackProfilerManager -> IO ()
installEventlogSocketHandlers =
#if defined(EVENTLOG_SOCKET_SUPPORT)
  \ manager -> do
    registerEventlogSocketHooks manager
  where
    registerEventlogSocketHooks manager = do
      registerHook HookPostStartEventLogging $
        startEventLoggingHook manager
      registerHook HookPreEndEventLogging $
        endEventLoggingHook manager
      ns <- registerNamespace "ghc-stack-profiler"
      registerCommand ns startProfilerCommandId (startProfilerCommand manager)
      registerCommand ns stopProfilerCommandId (stopProfilerCommand manager)
#else
  \ _manager ->
    pure ()
#endif

defaultErrorHandlers :: [Handler ()]
defaultErrorHandlers =
  [
#if defined(EVENTLOG_SOCKET_SUPPORT)
    Handler $ \ (e :: EventlogSocketControlError) -> do
      hPutStrLn stderr "Failed to register eventlog-socket commands"
      hPutStrLn stderr (displayException e)
#endif
  ]

-- | Post-start EventLogging hook.
--
-- 1. Publish init events and flush the eventlog.
-- 2. Inform the main loop that the eventlog is ready for messages now.
startEventLoggingHook :: StackProfilerManager -> IO ()
startEventLoggingHook manager = do
  sendPublishInitEventMessages manager
  -- Block until start message has been processed
  sendEnableEventlogMessage manager

-- | Pre-end EventLogging hook.
endEventLoggingHook :: StackProfilerManager -> IO ()
endEventLoggingHook manager = do
  -- Disallow logging any more messages to the eventlog.
  -- Stops the profiler sampling threads.
  STM.atomically $ disableEventLogging manager

  -- Block until all messages have been processed
  sendDisableEventlogMessage manager

startProfilerCommand :: StackProfilerManager -> IO ()
startProfilerCommand manager = do
  startProfiling manager

stopProfilerCommand :: StackProfilerManager -> IO ()
stopProfilerCommand manager = do
  stopProfiling manager
