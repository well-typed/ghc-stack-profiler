module GHC.Stack.Profiler.Core.SymbolTable (
  -- * Abstract interfaces for transforming 'CallStackMessage's and

  -- 'BinaryEventlogMessage' into each other.
  SymbolTableWriter (..),
  SymbolTableReader (..),

  -- * A 'Map' implementation for the 'SymbolTableWriter' interface.
  MapTable,
  emptyMapSymbolTableWriter,
  getKnownStrings,
  getKnownSourceLocations,

  -- * An 'IntMap' implementation for the 'SymbolTableReader' interface.
  IntMapTable,
  MissingKeyError (..),
  mkIntMapSymbolTableReader,
  emptyIntMapTable,
  insertSourceLocationMessage,
  insertTextMessage,
) where

import Control.Exception
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Tuple as Tuple
import GHC.Generics (Generic)
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.SourceLocation
import GHC.Stack.Profiler.Core.Util

-- ----------------------------------------------------------------------------
-- Abstract interfaces for writing and reading to the symbol tables for deduplicating
-- the symbols for 'Text' and 'SourceLocation'.
-- ----------------------------------------------------------------------------

-- | Implementation agnostic symbol table supposed to be used to deduplicate symbols
-- in 'CallStackMessage'.
--
-- When transforming 'CallStackMessage' to ['BinaryEventlogMessage'] we replace some
-- symbols with identifiers.
-- In particular arbitrary length symbols, such as 'Text's and 'SourceLocation's.
-- As these symbols are discovered while encoding the callstack, the 'SymbolTableWriter'
-- needs to be extended, which is why we thread the 'tbl' parameter through the
-- lookup or insertion operations.
data SymbolTableWriter tbl = MkSymbolTableWriter
  { writerTable :: !tbl
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
data SymbolTableReader = MkSymbolTableReader
  { lookupStringId :: StringId -> Maybe Text
  -- ^ Lookup the 'StringId' in the symbol table.
  -- This operation throws an exception if the 'StringId' is unknown.
  , lookupSourceLocationId :: SourceLocationId -> Maybe SourceLocation
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
  , stringUniqueSupply :: {-# UNPACK #-} !StringId
  , srcLocUniqueSupply :: {-# UNPACK #-} !SourceLocationId
  }
  deriving (Show, Eq, Ord, Generic)

{-# INLINEABLE emptyMapSymbolTableWriter #-}
emptyMapSymbolTableWriter :: SymbolTableWriter MapTable
emptyMapSymbolTableWriter =
  MkSymbolTableWriter
    { writerTable =
        MkMapTable
          { stringTable = Map.empty
          , srcLocTable = Map.empty
          , stringUniqueSupply = MkStringId 0
          , srcLocUniqueSupply = MkSourceLocationId 0
          }
    , lookupOrInsertText = alterStringMap
    , lookupOrInsertSourceLocation = alterSrcLocTable
    }
 where
  nextSrcLocUnique tbl =
    ( srcLocUniqueSupply tbl
    , tbl
        { srcLocUniqueSupply =
            incrementSourceLocationId $ srcLocUniqueSupply tbl
        }
    )

  nextStringUnique tbl =
    ( stringUniqueSupply tbl
    , tbl
        { stringUniqueSupply =
            incrementStringLocationId $ stringUniqueSupply tbl
        }
    )

  updateEntry tbl0 nextKey Nothing =
    let
      (sid, tbl) = nextKey tbl0
    in
      ((sid, True, tbl), Just sid)
  updateEntry tbl _ (Just val) =
    ((val, False, tbl), Just val)

  swapAround set ((sid, new, tbl), hm) =
    (sid, new, set tbl hm)

  alterStringMap = \tbl str ->
    swapAround setStringTable $
      Map.alterF (updateEntry tbl nextStringUnique) str (stringTable tbl)

  alterSrcLocTable = \tbl srcLoc ->
    swapAround setSourceLocationTable $
      Map.alterF (updateEntry tbl nextSrcLocUnique) srcLoc (srcLocTable tbl)

setSourceLocationTable :: MapTable -> Map SourceLocation SourceLocationId -> MapTable
setSourceLocationTable tbl hm =
  tbl
    { srcLocTable = hm
    }

setStringTable :: MapTable -> Map Text StringId -> MapTable
setStringTable tbl hm =
  tbl
    { stringTable = hm
    }

getKnownStrings :: MapTable -> [(StringId, Text)]
{-# INLINEABLE getKnownStrings #-}
getKnownStrings table =
  List.map Tuple.swap $ Map.assocs (stringTable table)

getKnownSourceLocations :: MapTable -> [(SourceLocationId, SourceLocation)]
{-# INLINEABLE getKnownSourceLocations #-}
getKnownSourceLocations table =
  List.map Tuple.swap $ Map.assocs (srcLocTable table)

-- ----------------------------------------------------------------------------
-- Implementation backend for 'SymbolTableReader'
-- ----------------------------------------------------------------------------

data MissingKeyError
  = -- | We failed to find the 'StringId' to fully decode the 'SourceLocationId'.
    KeyStringIdNotFound SourceLocationId StringId
  deriving (Show)

instance Exception MissingKeyError where
  displayException = \case
    KeyStringIdNotFound srcLocId stringId ->
      "While decoding the Source Location ("
        ++ show (getSourceLocationId srcLocId)
        ++ "), "
        ++ "the String ("
        ++ show (getStringId stringId)
        ++ ") couldn't be found"

data IntMapTable = MkIntMapTable
  { stringLookupTable :: !(IntMap Text)
  , srcLocLookupTable :: !(IntMap SourceLocation)
  }
  deriving (Eq, Ord, Show, Generic)

emptyIntMapTable :: IntMapTable
emptyIntMapTable =
  MkIntMapTable
    { stringLookupTable = IntMap.empty
    , srcLocLookupTable = IntMap.empty
    }

mkIntMapSymbolTableReader :: IntMapTable -> SymbolTableReader
mkIntMapSymbolTableReader tbl =
  MkSymbolTableReader
    { lookupStringId = flip lookupTextMessage tbl
    , lookupSourceLocationId = flip lookupSourceLocationMessage tbl
    }

{-# INLINEABLE insertTextMessage #-}
insertTextMessage :: BinaryStringMessage -> IntMapTable -> IntMapTable
insertTextMessage msg tbl =
  tbl
    { stringLookupTable =
        IntMap.insert
          (idToInt $ binaryStringMessageId msg)
          (binaryStringMessage msg)
          (stringLookupTable tbl)
    }

{-# INLINEABLE insertSourceLocationMessage #-}
insertSourceLocationMessage :: BinarySourceLocationMessage -> IntMapTable -> Either MissingKeyError IntMapTable
insertSourceLocationMessage msg tbl = do
  let
    srcLocId = binarySourceLocationMessageId msg
    funcId = binarySourceLocationFunctionId msg
    fileId = binarySourceLocationFilename msg

  funcName <-
    maybe (Left $ KeyStringIdNotFound srcLocId funcId) Right $ lookupTextMessage funcId tbl
  fileName <-
    maybe (Left $ KeyStringIdNotFound srcLocId fileId) Right $ lookupTextMessage fileId tbl

  pure
    tbl
      { srcLocLookupTable =
          IntMap.insert
            (idToInt srcLocId)
            (mkSourceLocation funcName fileName)
            (srcLocLookupTable tbl)
      }
 where
  mkSourceLocation funcName fileName =
    MkSourceLocation
      { line = binarySourceLocationRow msg
      , column = binarySourceLocationColumn msg
      , functionName = funcName
      , fileName = fileName
      }

{-# INLINEABLE lookupTextMessage #-}
lookupTextMessage :: StringId -> IntMapTable -> Maybe Text
lookupTextMessage sid tbl = IntMap.lookup (idToInt sid) (stringLookupTable tbl)

{-# INLINEABLE lookupSourceLocationMessage #-}
lookupSourceLocationMessage :: SourceLocationId -> IntMapTable -> Maybe SourceLocation
lookupSourceLocationMessage sid tbl = IntMap.lookup (idToInt sid) (srcLocLookupTable tbl)
