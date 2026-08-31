module GHC.Stack.Profiler.Core.Dehydrate where

import Control.Monad (when)
import Control.Monad.Trans.State.Strict (State, runState)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import Data.Word (Word16)
import GHC.Generics
import GHC.Stack.Profiler.Core.CallStack
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.Util

-- | Generic implementation to turn 'CallStack' into '[Message]'.
--
-- Replaces already encountered text or source location information with unique ids.
-- If new text or source location messages are encountered, they are inserted into
-- the 'SymbolTableWriter'.
--
-- All new string values and source location messages are before 'CallStackChunk' and
-- 'CallStackFinal' messages.
-- For the result list @r :: ['Message']@, the following holds:
--
-- * 'StringDef' messages are the first elements in @r@. There might not be any.
-- * 'SourceLocationDef' are after 'StringDef' messages and before any 'CallStackChunk' or
--    'CallStackFinal' messages. There might not be any such messages.
-- * Then 'CallStackChunk' follow if there are any.
-- * The last message is always a 'CallStackFinal' message and it occurs exactly once in @r@.
dehydrateCallStackMessage ::
  forall table.
  SymbolTableWriter table ->
  CallStack ->
  ([Message], SymbolTableWriter table)
dehydrateCallStackMessage msgTbl0 msg =
  let
    (stackItems, finalState) =
      runWithEncodingState
        (newEncodingState msgTbl0)
        (mapM go (callStack msg))

    stringDefs =
      map StringDef $ stringMessages finalState

    sourceLocDefs =
      map SourceLocationDef $ sourceLocMessages finalState

    stackMsgChunks =
      chunkCallStackMessage
        MkCallStackChunk
          { callStackChunkThreadId = callThreadId msg
          , callStackChunkCapabilityId = callCapabilityId msg
          , callStackChunk = stackItems
          }
  in
    ( stringDefs ++ sourceLocDefs ++ stackMsgChunks
    , symbolTableWriter finalState
    )
 where
  go :: StackItem -> State (EncodingState tbl) CallStackFrame
  go = \case
    IpeId ipeId ->
      pure $ CallStackFrameIpe ipeId
    UserAnnotation s mSrcLoc -> do
      srcLocId <- case mSrcLoc of
        Nothing -> pure Nothing
        Just srcLoc -> Just <$> lookupSourceLocationMessage srcLoc
      CallStackFrameAnn <$> lookupTextMessage (Text.pack s) <*> pure srcLocId

-- | Chunk the 'callStackChunk' of the 'CallStackChunk' by the given 'Word16'.
-- If there are no items in 'CallStackChunk', then a singleton list is returned containing
-- the original element.
--
-- Post-condition for the result @r@:
--
-- * all elements in @init r @ are 'CallStackChunk's
-- * the element returned by @last r@ is a 'CallStackFinal' Message.
--
-- The resulting 'CallStackChunk' are in reverse order and so are the chunks themselves.
--
-- This means, for a stack @[1,2,3,4,5,6]@ and an assumed chunk size of 2,
-- we produce @[[6,5],[4,3],[2,1]]@.
chunkCallStackMessage :: CallStackChunk -> [Message]
chunkCallStackMessage = chunkCallStackMessage_ callStackSizeLimit

-- | Same as 'chunkCallStackMessage', but allows to set the chunking size in bytes.
chunkCallStackMessage_ :: Word16 -> CallStackChunk -> [Message]
chunkCallStackMessage_ chunkLimit16 msg0 =
  let
    chunkLimitInt = word16ToInt chunkLimit16
    items = callStackChunk msg0
    chunked =
      let
        go (!size, curChunk, restChunk) item =
          let
            !bytes = word16ToInt $ byteSizeOf item
          in
            if (size + bytes) < chunkLimitInt
              then (size + bytes, item : curChunk, restChunk)
              else (bytes, [item], curChunk : restChunk)
        (_, lastChunk, initChunk) = List.foldl' go (0, [], []) items
      in
        lastChunk : initChunk
  in
    mkEventlogMessages chunked
 where
  mkCallStack chunk =
    MkCallStackChunk
      { callStackChunkThreadId = callStackChunkThreadId msg0
      , callStackChunkCapabilityId = callStackChunkCapabilityId msg0
      , callStackChunk = chunk
      }

  mkEventlogMessages :: [[CallStackFrame]] -> [Message]
  mkEventlogMessages [] =
    -- If there are no chunks, we simply return the original message
    [ CallStackFinal msg0
    ]
  mkEventlogMessages [chunk] =
    [ CallStackFinal $ mkCallStack chunk
    ]
  mkEventlogMessages (chunk : chunks) =
    CallStackChunk (mkCallStack chunk) : mkEventlogMessages chunks

-- ----------------------------------------------------------------------------
-- Helper types and functions to implement the conversion to the binary
-- representation.
-- ----------------------------------------------------------------------------

data EncodingState tbl = MkEncodingState
  { symbolTableWriter :: !(SymbolTableWriter tbl)
  , stringMessages :: ![StringDef]
  , sourceLocMessages :: ![SourceLocationDef]
  }
  deriving (Generic)

runWithEncodingState :: EncodingState tbl -> State (EncodingState tbl) a -> (a, EncodingState tbl)
runWithEncodingState encodingState encoder =
  runState encoder encodingState

newEncodingState :: SymbolTableWriter tbl -> EncodingState tbl
newEncodingState msgTbl0 =
  MkEncodingState
    { symbolTableWriter = msgTbl0
    , stringMessages = []
    , sourceLocMessages = []
    }

setSymbolTableWriter :: tbl -> State.State (EncodingState tbl) ()
setSymbolTableWriter tbl = State.modify' (\st -> st{symbolTableWriter = (symbolTableWriter st){writerTable = tbl}})

addStringMessage :: StringDef -> State.State (EncodingState tbl) ()
addStringMessage msg = State.modify' (\st -> st{stringMessages = msg : stringMessages st})

addSourceLocationMessage :: SourceLocationDef -> State.State (EncodingState tbl) ()
addSourceLocationMessage msg = State.modify' (\st -> st{sourceLocMessages = msg : sourceLocMessages st})

lookupOrInsertTextMessage :: forall tbl. Text -> State (EncodingState tbl) (StringId, Bool)
lookupOrInsertTextMessage s = do
  tbl <- State.gets symbolTableWriter
  let
    (sid, new, tbl1) = lookupOrInsertText tbl (writerTable tbl) s
  setSymbolTableWriter tbl1
  pure (sid, new)

lookupOrInsertSrcLocMessage :: forall tbl. SourceLocation -> State (EncodingState tbl) (SourceLocationId, Bool)
lookupOrInsertSrcLocMessage s = do
  tbl <- State.gets symbolTableWriter
  let
    (sid, new, tbl1) = lookupOrInsertSourceLocation tbl (writerTable tbl) s
  setSymbolTableWriter tbl1
  pure (sid, new)

lookupTextMessage :: forall tbl. Text -> State (EncodingState tbl) StringId
lookupTextMessage s = do
  (sid, new) <- lookupOrInsertTextMessage s
  when new $
    addStringMessage $
      MkStringDef sid s
  pure sid

lookupSourceLocationMessage :: forall tbl. SourceLocation -> State (EncodingState tbl) SourceLocationId
lookupSourceLocationMessage s = do
  (sid, new) <- lookupOrInsertSrcLocMessage s
  when new $ do
    fileId <- lookupTextMessage $ fileName s
    addSourceLocationMessage $
      MkSourceLocationDef
        { sourceLocationDefId = sid
        , sourceLocationDefRow = line s
        , sourceLocationDefColumn = column s
        , sourceLocationDefFilename = fileId
        }
  pure sid

-- | Implementation agnostic symbol table supposed to be used to deduplicate symbols
-- in 'CallStack'.
--
-- When transforming 'CallStack' to ['Message'] we replace some
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
            nextSourceLocationId $ srcLocUniqueSupply tbl
        }
    )

  nextStringUnique tbl =
    ( stringUniqueSupply tbl
    , tbl
        { stringUniqueSupply =
            nextStringId $ stringUniqueSupply tbl
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
