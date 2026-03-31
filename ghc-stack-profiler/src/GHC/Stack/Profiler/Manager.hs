module GHC.Stack.Profiler.Manager (
  StackProfilerManager (..),
  newStackProfilerManager,
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TVar
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Stack.Profiler.SymbolTable

-- | A 'StackProfilerManager' records all the relevant information
-- to manage the ghc stack profiler run-time.
data StackProfilerManager = MkStackProfilerManager
  { profilerThreads :: !(TVar (Set (Async ()), Set ThreadId))
  -- ^ 'Async' of the stack sampling thread.
  , symbolTableRef :: !StackSymbolTable
  , isRunning :: !(TVar Bool)
  , isShuttingDown :: !(TVar Bool)
  }
  deriving (Generic, Eq)

newStackProfilerManager :: Bool -> IO StackProfilerManager
newStackProfilerManager running = do
  MkStackProfilerManager
    <$> newTVarIO (Set.empty, Set.empty)
    <*> emptySymbolTable
    <*> newTVarIO running
    <*> newTVarIO False
