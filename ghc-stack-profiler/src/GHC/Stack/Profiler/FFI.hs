{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GHC.Stack.Profiler.FFI (
  installEventlogSocketHandlers,
  startProfiler,
  stopProfiler,
) where

import Foreign.StablePtr
import GHC.Stack.Profiler.Sampler
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

installEventlogSocketHandlers :: StackProfilerManager -> IO ()
installEventlogSocketHandlers manager = do
  stackProfilerPtr <- newStablePtr manager
  registerStackProfilerCommands stackProfilerPtr

startProfiler :: StablePtr StackProfilerManager -> IO ()
startProfiler managerPtr = do
  manager <- deRefStablePtr managerPtr
  STM.atomically $ TVar.writeTVar (isRunning manager) True

stopProfiler :: StablePtr StackProfilerManager -> IO ()
stopProfiler managerPtr = do
  manager <- deRefStablePtr managerPtr
  STM.atomically $ TVar.writeTVar (isRunning manager) False

foreign import capi "custom-command-handler.h custom_ghc_stack_profiler_command_register"
  registerStackProfilerCommands :: StablePtr StackProfilerManager -> IO ()

foreign export ccall startProfiler :: StablePtr StackProfilerManager -> IO ()

foreign export ccall stopProfiler :: StablePtr StackProfilerManager -> IO ()
