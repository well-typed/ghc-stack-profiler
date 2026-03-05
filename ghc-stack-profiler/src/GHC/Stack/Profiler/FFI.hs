{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GHC.Stack.Profiler.FFI (
  installEventlogSocketHandlers,
  startProfiler,
  stopProfiler,
) where

import GHC.Stack.Profiler.Manager
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
#if defined(EVENTLOG_SOCKET_SUPPORT)
import Control.Exception
import GHC.Eventlog.Socket
import System.IO
#endif

#if defined(EVENTLOG_SOCKET_SUPPORT)
startProfilerCommandId :: CommandId
startProfilerCommandId = CommandId 0x1

stopProfilerCommandId :: CommandId
stopProfilerCommandId = CommandId 0x2
#endif

installEventlogSocketHandlers :: StackProfilerManager -> IO ()
installEventlogSocketHandlers = do
#if defined(EVENTLOG_SOCKET_SUPPORT)
  \ manager -> do
    success <- try $ do
      ns <- registerNamespace "ghc-stack-profiler"
      registerCommand ns startProfilerCommandId (startProfiler manager)
      registerCommand ns stopProfilerCommandId (stopProfiler manager)
    case success of
      Right () -> pure ()
      Left (esce :: EventlogSocketControlError) -> do
        hPutStrLn stderr "Failed to register eventlog-socket commands"
        hPutStrLn stderr (displayException esce)
#else
  \ _manager ->
    pure ()
#endif

startProfiler :: StackProfilerManager -> IO ()
startProfiler manager = do
  STM.atomically $ TVar.writeTVar (isRunning manager) True

stopProfiler :: StackProfilerManager -> IO ()
stopProfiler manager = do
  STM.atomically $ TVar.writeTVar (isRunning manager) False
