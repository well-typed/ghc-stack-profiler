module GHC.Stack.Profiler.Core.SymbolTable (
  -- * Abstract interfaces for transforming 'CallStackMessage's and
  -- 'BinaryEventlogMessage' into each other.
  SymbolTableWriter(..),
  SymbolTableReader(..),
  -- * A 'Map' implementation for the 'SymbolTableWriter' interface.
  MapTable,
  emptyMapSymbolTableWriter,
  -- * An 'IntMap' implementation for the 'SymbolTableReader' interface.
  IntMapTable,
  mkIntMapSymbolTableReader,
  emptyIntMapTable,
  insertSourceLocationMessage,
  insertTextMessage,
) where

import Data.Text (Text)
import Data.Word (Word64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.SourceLocation
import GHC.Stack.Profiler.Core.Util
import GHC.Stack (HasCallStack)

-- ----------------------------------------------------------------------------
-- Abstract interfaces for writing and reading to the symbol tables for deduplicating
-- the symbols for 'Text' and 'SourceLocation'.
-- ----------------------------------------------------------------------------

-- | Implementation agnostic symbol table supposed to be used to deduplicate symbols
-- in 'CallStackMessage'.
--
-- When transforming 'CallStackMessage' to '[BinaryEventlogMessage]' we replace some
-- symbols with identifiers.
-- In particular arbitrary length symbols, such as 'Text's and 'SourceLocation's.
-- As these symbols are discovered while encoding the callstack, the 'SymbolTableWriter'
-- needs to be extended, which is why we thread the 'tbl' parameter through the
-- lookup or insertion operations.
data SymbolTableWriter tbl =
  MkSymbolTableWriter
    { writerTable :: tbl
    -- ^ Symbol table for symbols we replace with unique identifiers.
    , lookupOrInsertText :: tbl -> Text -> (StringId, Bool, tbl)
    -- ^ Lookup up the given 'Text' in the 'tbl' Symbol table.
    -- If the 'Text' can't be found, we insert it into the table and generate a
    -- new 'StringId.
    -- Returns 'True', if the given 'Text' was inserted and 'False' otherwise.
    , lookupOrInsertSourceLocation :: tbl -> SourceLocation -> (SourceLocationId, Bool, tbl)
    -- ^ Lookup up the given 'SourceLocation' in the 'tbl' Symbol table.
    -- If the 'SourceLocation' can't be found, we insert it into the table and generate a
    -- new 'SourceLocationId.
    -- Returns 'True', if the given 'Text' was inserted and 'False' otherwise.
    }
  deriving (Generic)

-- | Implementation agnostic symbol table reader helping consumers to decode
-- 'BinaryEventlogMessage's into a 'CallStackMessage'.
--
-- As during deserialisation, we do not discover new Messages, the abstract 'SymbolTableReader'
-- doesn't need to thread the implementation through the lookup operations.
data SymbolTableReader =
  MkSymbolTableReader
    { lookupStringId :: StringId -> Text
    -- ^ Lookup the 'StringId' in the symbol table.
    -- This operation throws an exception if the 'StringId' is unknown.
    , lookupSourceLocationId :: SourceLocationId -> SourceLocation
    -- ^ Lookup the 'SourceLocationId' in the symbol table.
    -- This operation throws an exception if the 'SourceLocationId' is unknown.
    }
  deriving (Generic)

-- ----------------------------------------------------------------------------
-- Implementation backend for 'SymbolTableWriter'
-- ----------------------------------------------------------------------------

data MapTable = MkMapTable
  { stringTable :: !(Map Text StringId)
  , srcLocTable :: !(Map SourceLocation SourceLocationId)
  , uniqueSupply :: !Word64
  } deriving (Show, Eq, Ord, Generic)

{-# INLINABLE emptyMapSymbolTableWriter #-}
emptyMapSymbolTableWriter :: SymbolTableWriter MapTable
emptyMapSymbolTableWriter = MkSymbolTableWriter
  { writerTable =
      MkMapTable
        { stringTable = Map.empty
        , srcLocTable = Map.empty
        , uniqueSupply = 0
        }
  , lookupOrInsertText = alterStringMap
  , lookupOrInsertSourceLocation = alterSrcLocTable
  }

  where
    nextUnique tbl = (uniqueSupply tbl, tbl { uniqueSupply = uniqueSupply tbl + 1 })

    updateEntry tbl0 mkKey Nothing =
      let
        (uniq, tbl) = nextUnique tbl0
        sid = mkKey uniq
      in
        ((sid, True, tbl), Just sid)

    updateEntry tbl _ (Just val) =
      ((val, False, tbl), Just val)

    swapAround set ((sid, new, tbl), hm) =
      (sid, new, set tbl hm)

    alterStringMap = \ tbl str ->
      swapAround setStringTable $
        Map.alterF (updateEntry tbl MkStringId) str (stringTable tbl)

    alterSrcLocTable = \ tbl srcLoc ->
      swapAround setSourceLocationTable $
        Map.alterF (updateEntry tbl MkSourceLocationId) srcLoc (srcLocTable tbl)

setSourceLocationTable :: MapTable -> Map SourceLocation SourceLocationId -> MapTable
setSourceLocationTable tbl hm = tbl { srcLocTable = hm }

setStringTable :: MapTable -> Map Text StringId -> MapTable
setStringTable tbl hm = tbl { stringTable = hm }

-- ----------------------------------------------------------------------------
-- Implementation backend for 'SymbolTableReader'
-- ----------------------------------------------------------------------------

data IntMapTable = MkIntMapTable
  { stringLookupTable :: !(IntMap Text)
  , srcLocLookupTable :: !(IntMap SourceLocation)
  } deriving (Eq, Ord, Show, Generic)

emptyIntMapTable :: IntMapTable
emptyIntMapTable = MkIntMapTable
  { stringLookupTable = IntMap.empty
  , srcLocLookupTable = IntMap.empty
  }

mkIntMapSymbolTableReader :: IntMapTable -> SymbolTableReader
mkIntMapSymbolTableReader tbl =
  MkSymbolTableReader
    { lookupStringId = flip lookupTextMessage tbl
    , lookupSourceLocationId = flip lookupSourceLocationMessage tbl
    }

{-# INLINABLE insertTextMessage #-}
insertTextMessage :: BinaryStringMessage -> IntMapTable -> IntMapTable
insertTextMessage msg tbl =
  tbl
    { stringLookupTable =
        IntMap.insert
          (idToInt $ binaryStringMessageId msg)
          (binaryStringMessage msg)
          (stringLookupTable tbl)
    }

{-# INLINABLE insertSourceLocationMessage #-}
insertSourceLocationMessage :: BinarySourceLocationMessage -> IntMapTable -> IntMapTable
insertSourceLocationMessage msg tbl =
  tbl
    { srcLocLookupTable =
        IntMap.insert
          (idToInt $ binarySourceLocationMessageId msg)
          sourceLocation
          (srcLocLookupTable tbl)
    }
  where
    sourceLocation = MkSourceLocation
      { line = binarySourceLocationRow msg
      , column = binarySourceLocationColumn msg
      , functionName = lookupTextMessage (binarySourceLocationFunctionId msg) tbl
      , fileName = lookupTextMessage (binarySourceLocationFilename msg) tbl
      }

{-# INLINABLE lookupTextMessage #-}
lookupTextMessage :: StringId -> IntMapTable -> Text
lookupTextMessage sid tbl = stringLookupTable tbl `unsafeIntMapLookup` idToInt sid

{-# INLINABLE lookupSourceLocationMessage #-}
lookupSourceLocationMessage :: SourceLocationId -> IntMapTable -> SourceLocation
lookupSourceLocationMessage sid tbl = srcLocLookupTable tbl `unsafeIntMapLookup` idToInt sid

unsafeIntMapLookup :: HasCallStack => IntMap a -> Int -> a
unsafeIntMapLookup tbl k = case IntMap.lookup k tbl of
  Just v -> v
  Nothing ->
    error $ "Failed to find key: " ++ showAsHex k
