{-# LANGUAGE CPP #-}

module GHC.Stack.Profiler.Eventlog.Socket (
  registerWithEventlogSocket,
) where

import GHC.Stack.Profiler.Manager (StackProfilerManager)

#ifdef EVENTLOG_SOCKET_SUPPORT
import qualified Control.Monad.STM as STM
import GHC.Eventlog.Socket (CommandId (..), Hook (..), registerCommand, registerHook, registerNamespace)
import GHC.Stack.Profiler.Commands (startProfiling, stopProfiling, sendEnableEventlogMessage, sendDisableEventlogMessage, sendPublishInitEventMessages)
import GHC.Stack.Profiler.Manager (disableEventLogging)
import Debug.Trace (traceMarkerIO)
#endif

-- | Register the @eventlog-socket@ custom command handlers and lifecycle hooks.
--
-- This adds support for the following @eventlog-socket@ custom commands:
--
-- * @0x01@: Start profiling.
-- * @0x02@: Stop profiling.
--
-- If built with @+control@, this may throw an [@EventlogSocketControlError@](https://hackage-content.haskell.org/package/eventlog-socket/docs/GHC-Eventlog-Socket.html#t:EventlogSocketControlError).
registerWithEventlogSocket :: StackProfilerManager -> IO ()
#ifdef EVENTLOG_SOCKET_SUPPORT
registerWithEventlogSocket = registerWithEventlogSocketIfSupported
#else
registerWithEventlogSocket = const $ pure ()
#endif

#ifdef EVENTLOG_SOCKET_SUPPORT
-- The real implementation of @registerWithEventlogSocket@.
registerWithEventlogSocketIfSupported :: StackProfilerManager -> IO ()
registerWithEventlogSocketIfSupported manager = do
  -- Register the PostStartEventLogging and PreEndEventLogging hooks.
  registerHook HookPostStartEventLogging $ startEventLoggingHook manager
  registerHook HookPreEndEventLogging $ endEventLoggingHook manager

  -- Register the custom commands under the ghc-stack-profiler namespace.
  ns <- registerNamespace "ghc-stack-profiler"
  registerCommand ns startProfilerCommandId (startProfilerCommand manager)
  registerCommand ns stopProfilerCommandId (stopProfilerCommand manager)

-- The @startProfiler@ command ID.
startProfilerCommandId :: CommandId
startProfilerCommandId = CommandId 0x1

-- The @stopProfiler@ command ID.
stopProfilerCommandId :: CommandId
stopProfilerCommandId = CommandId 0x2

-- | The handler for @eventlog-socket@'s @PostStartEventLogging@ hook.
--
-- This publishes the init events, flushes the eventlog, informs the manager
-- that the eventlog is enabled, and blocks until this message is processed.
startEventLoggingHook :: StackProfilerManager -> IO ()
startEventLoggingHook manager = do
  sendPublishInitEventMessages manager
  sendEnableEventlogMessage manager

-- | The handler for @eventlog-socket@'s @PreEndEventLogging@ hook.
--
-- This stops all profiler threads from writing to the eventlog, which stops
-- all sampler threads, informs the manager that the eventlog is disabled, and
-- blocks until this message is processed.
endEventLoggingHook :: StackProfilerManager -> IO ()
endEventLoggingHook manager = do
  STM.atomically $ disableEventLogging manager
  sendDisableEventlogMessage manager

-- | The handler for the @StartProfiling@ custom command.
startProfilerCommand :: StackProfilerManager -> IO ()
startProfilerCommand manager = do
  traceMarkerIO "ghc-stack-profiler: Start profiling"
  startProfiling manager

-- | The handler for the @StopProfiling@ custom command.
stopProfilerCommand :: StackProfilerManager -> IO ()
stopProfilerCommand manager = do
  stopProfiling manager
  traceMarkerIO "ghc-stack-profiler: Stop profiling"
#endif
