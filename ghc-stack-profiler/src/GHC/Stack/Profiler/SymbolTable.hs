module GHC.Stack.Profiler.SymbolTable (
  -- * 'StackSymbolTable' type
  StackSymbolTable,
  emptySymbolTable,
  emptySymbolTableIO,
  readSymbolTable,
  writeSymbolTable,
) where

import Control.Concurrent.STM
import GHC.Generics (Generic)
import GHC.Stack.Profiler.Core.SymbolTable

-- | A @'SymbolTableWriter' 'MapTable'@ guarded by a lock for mutable, concurrent access.
--
-- The lock is an 'MVar', but this is considered an implementation detail that may change without warning.
newtype StackSymbolTable = MkStackSymbolTable
  { writerSymbolTable :: TVar (SymbolTableWriter MapTable)
  }
  deriving (Generic, Eq)

-- | Create an empty 'StackSymbolTable'
emptySymbolTableIO :: IO StackSymbolTable
emptySymbolTableIO = atomically emptySymbolTable

emptySymbolTable :: STM StackSymbolTable
emptySymbolTable =
  MkStackSymbolTable
    <$> newTVar emptyMapSymbolTableWriter

readSymbolTable :: StackSymbolTable -> STM (SymbolTableWriter MapTable)
readSymbolTable =
  readTVar . writerSymbolTable

writeSymbolTable :: SymbolTableWriter MapTable -> StackSymbolTable -> STM ()
writeSymbolTable newWriterTbl symTbl =
  writeTVar (writerSymbolTable symTbl) newWriterTbl
