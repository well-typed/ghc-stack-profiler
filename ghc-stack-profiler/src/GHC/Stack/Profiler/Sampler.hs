module GHC.Stack.Profiler.Sampler (
  StackProfilerManager (..),
  newStackProfilerManager,
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack.Profiler.Core.SymbolTable (emptyMapSymbolTableWriter)
import GHC.Stack.Profiler.Decode

-- | A 'StackProfilerManager' records all the relevant information
-- to manage the ghc stack profiler run-time.
data StackProfilerManager = MkStackProfilerManager
  { profilerThreads :: !(TVar (Set ThreadId))
  -- ^ 'ThreadId' of the stack sampling thread.
  , symbolTableRef :: !(TVar SymbolTable)
  , isRunning :: !(TVar Bool)
  , isShuttingDown :: !(TVar Bool)
  }
  deriving (Eq)

newStackProfilerManager :: Bool -> IO StackProfilerManager
newStackProfilerManager running = do
  MkStackProfilerManager
    <$> newTVarIO Set.empty
    <*> newTVarIO emptyMapSymbolTableWriter
    <*> newTVarIO running
    <*> newTVarIO False
