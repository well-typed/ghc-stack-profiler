module GHC.Stack.Profiler.SymbolTable (
  -- * 'StackSymbolTable' type
  StackSymbolTable (..),
  emptySymbolTable,
  withSymbolWriter,
  modifySymbolWriter,
  modifySymbolWriter_,
) where

import Control.Concurrent
import qualified Control.Concurrent.MVar as MVar
import GHC.Generics (Generic)
import GHC.Stack.Profiler.Core.SymbolTable

-- | A @'SymbolTableWriter' 'MapTable'@ guarded by a lock for mutable, concurrent access.
--
-- The lock is an 'MVar', but this is considered an implementation detail that may change without warning.
newtype StackSymbolTable
  = MkStackSymbolTable
  { writerSymbolTable :: MVar (SymbolTableWriter MapTable)
  }
  deriving (Generic, Eq)

-- | Create an empty 'StackSymbolTable'
emptySymbolTable :: IO StackSymbolTable
emptySymbolTable =
  MkStackSymbolTable
    <$> newMVar emptyMapSymbolTableWriter

-- | Read-only access to the underlying 'SymbolTableWriter'.
--
-- Locks the resource until the wrapped computation finishes.
withSymbolWriter :: StackSymbolTable -> (SymbolTableWriter MapTable -> IO a) -> IO a
withSymbolWriter stackSymTable act =
  MVar.withMVar
    (writerSymbolTable stackSymTable)
    act

-- | Modify the underlying 'SymbolTableWriter' that allows to return an additional
-- computation.
--
-- Locks the resource until the wrapped computation finishes.
modifySymbolWriter :: StackSymbolTable -> (SymbolTableWriter MapTable -> IO (SymbolTableWriter MapTable, a)) -> IO a
modifySymbolWriter stackSymTable act = do
  MVar.modifyMVar
    (writerSymbolTable stackSymTable)
    act

-- | Same as 'modifySymbolWriter' without other results.
modifySymbolWriter_ :: StackSymbolTable -> (SymbolTableWriter MapTable -> IO (SymbolTableWriter MapTable)) -> IO ()
modifySymbolWriter_ stackSymTable act = do
  MVar.modifyMVar_
    (writerSymbolTable stackSymTable)
    act
