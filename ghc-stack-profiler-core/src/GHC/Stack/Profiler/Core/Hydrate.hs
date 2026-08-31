module GHC.Stack.Profiler.Core.Hydrate where

import Control.Exception
import Data.Either (partitionEithers)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import GHC.Stack.Profiler.Core.CallStack
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.Util

data BinaryCallStackDecodeError
  = StringIdNotFound StringId
  | SourceLocationIdNotFound SourceLocationId
  deriving (Show)

instance Exception BinaryCallStackDecodeError where
  displayException = \case
    StringIdNotFound sid ->
      "Failed to decode a BinaryCallStackMessage. Failed to find a String with the key: " ++ show (getStringId sid)
    SourceLocationIdNotFound sid ->
      "Failed to decode a BinaryCallStackMessage. Failed to find a SourceLocation with the key: " ++ show (getSourceLocationId sid)

-- | Generic implementation to turn 'BinaryCallStackMessage' into the much richer
-- 'CallStack'.
hydrateEventlogCallStackMessage :: SymbolTableReader -> BinaryCallStackMessage -> (CallStack, [BinaryCallStackDecodeError])
hydrateEventlogCallStackMessage decodeTable msg =
  let
    decodeItem :: BinaryStackItem -> Either BinaryCallStackDecodeError StackItem
    decodeItem = \case
      BinaryIpe ipeId ->
        Right $ IpeId ipeId
      BinaryMessage stringId mSrcLocId -> do
        str <-
          maybe
            (Left $ StringIdNotFound stringId)
            (Right . Text.unpack)
            (lookupStringId decodeTable stringId)
        srcLoc <- case mSrcLocId of
          Nothing -> pure Nothing
          Just srcLocId ->
            maybe
              (Left $ SourceLocationIdNotFound srcLocId)
              (Right . Just)
              (lookupSourceLocationId decodeTable srcLocId)
        pure $ UserAnnotation str srcLoc

    itemsOrErros = map decodeItem (binaryCallStack msg)
    (errors, items) = partitionEithers itemsOrErros
  in
    ( MkCallStack
        { callCapabilityId = binaryCallCapabilityId msg
        , callThreadId = binaryCallThreadId msg
        , callStack = items
        }
    , errors
    )

-- | Implementation agnostic symbol table reader helping consumers to decode
-- 'Message's into a 'CallStack'.
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
    fileId = binarySourceLocationFilename msg

  fileName <-
    maybe (Left $ KeyStringIdNotFound srcLocId fileId) Right $ lookupTextMessage fileId tbl

  pure
    tbl
      { srcLocLookupTable =
          IntMap.insert
            (idToInt srcLocId)
            (mkSourceLocation fileName)
            (srcLocLookupTable tbl)
      }
 where
  mkSourceLocation fileName =
    MkSourceLocation
      { line = binarySourceLocationRow msg
      , column = binarySourceLocationColumn msg
      , fileName = fileName
      }

{-# INLINEABLE lookupTextMessage #-}
lookupTextMessage :: StringId -> IntMapTable -> Maybe Text
lookupTextMessage sid tbl = IntMap.lookup (idToInt sid) (stringLookupTable tbl)

{-# INLINEABLE lookupSourceLocationMessage #-}
lookupSourceLocationMessage :: SourceLocationId -> IntMapTable -> Maybe SourceLocation
lookupSourceLocationMessage sid tbl = IntMap.lookup (idToInt sid) (srcLocLookupTable tbl)
