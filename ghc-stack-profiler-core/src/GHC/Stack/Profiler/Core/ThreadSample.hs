{-# LANGUAGE OverloadedStrings #-}

module GHC.Stack.Profiler.Core.ThreadSample (
  -- * High-level API
  ThreadSample (..),
  deserializeEventlogMessage,

  -- * Serialisable 'ThreadSample'
  CallStackMessage (..),
  StackItem (..),
  SourceLocation (..),

  -- * Serialisation of 'CallStackMessage'
  SymbolTableWriter (..),
  SymbolTableReader (..),
  dehydrateCallStackMessage,
  BinaryCallStackDecodeError (..),
  hydrateEventlogCallStackMessage,
  catCallStackMessage,
  chunkCallStackMessage,

  -- * Message dehydration helpers
  EncodingState,
  runWithEncodingState,
  newEncodingState,
  lookupSourceLocationMessage,
  lookupTextMessage,
) where

import Control.Concurrent
import Control.Exception (Exception (..))
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (State, runState)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Data.Either (partitionEithers)
import GHC.Stack.CloneStack (StackSnapshot)
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.SourceLocation
import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.Util (chunksOf, word16ToInt)

-- ----------------------------------------------------------------------------
-- Thread Sample
-- ----------------------------------------------------------------------------

-- | A 'ThreadSample' is a snapshot of a threads RTS callstack.
-- This callstack is a copy of the original callstack, so can be traversed and
-- decoded without affecting the running thread.
--
-- The 'StackSnapshot' is a boxed value and needs to be garbage collected.
-- Note, as long as 'StackSnapshot' is alive, you keep the full callstack
-- alive, which might be quite expensive.
data ThreadSample = ThreadSample
  { threadSampleId :: !ThreadId
  , threadSampleCapability :: !CapabilityId
  , threadSampleStackSnapshot :: !StackSnapshot
  }
  deriving (Generic)

deserializeEventlogMessage :: LBS.ByteString -> Either String BinaryEventlogMessage
deserializeEventlogMessage msg = case runGetOrFail get msg of
  Left (_, _, errMsg) -> Left errMsg
  Right (_, _, callStackMessage) -> Right callStackMessage

-- ----------------------------------------------------------------------------
-- Decoded RTS CallStack
-- ----------------------------------------------------------------------------

-- | A decoded rts callstack that can be serialised to the EventLog.
data CallStackMessage = MkCallStackMessage
  { callThreadId :: !Word64
  , callCapabilityId :: !CapabilityId
  , callStack :: [StackItem]
  }
  deriving (Eq, Ord, Show, Generic)

data StackItem
  = IpeId !IpeId
  | UserMessage !String
  | SourceLocation !SourceLocation
  deriving (Eq, Ord, Show, Generic)

-- ----------------------------------------------------------------------------
-- Turning a 'CallStackMessage' into '[BinaryEventlogMessage]'
-- ----------------------------------------------------------------------------

-- | Generic implementation to turn 'CallStackMessage' into '[BinaryEventlogMessage]'.
--
-- Replaces already encountered text or source location information with unique ids.
-- If new text or source location messages are encountered, they are inserted into
-- the 'SymbolTableWriter'.
--
-- All new string values and source location messages are before 'CallStackChunk' and
-- 'CallStackFinal' messages.
-- For the result list @r :: ['BinaryEventlogMessage']@, the following holds:
--
-- * 'StringDef' messages are the first elements in @r@. There might not be any.
-- * 'SourceLocationDef' are after 'StringDef' messages and before any 'CallStackChunk' or
--    'CallStackFinal' messages. There might not be any such messages.
-- * Then 'CallStackChunk' follow if there are any.
-- * The last message is always a 'CallStackFinal' message and it occurs exactly once in @r@.
dehydrateCallStackMessage ::
  forall table.
  SymbolTableWriter table ->
  CallStackMessage ->
  ([BinaryEventlogMessage], SymbolTableWriter table)
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
      chunkCallStackMessage callStackItemLimit $
        MkBinaryCallStackMessage
          { binaryCallThreadId = callThreadId msg
          , binaryCallCapabilityId = callCapabilityId msg
          , binaryCallStack = stackItems
          }
  in
    ( stringDefs ++ sourceLocDefs ++ stackMsgChunks
    , symbolTableWriter finalState
    )
 where
  go :: StackItem -> State (EncodingState tbl) BinaryStackItem
  go = \case
    IpeId ipeId ->
      pure $ BinaryIpe ipeId
    UserMessage s ->
      BinaryString <$> lookupTextMessage (Text.pack s)
    SourceLocation srcLoc ->
      BinarySourceLocation <$> lookupSourceLocationMessage srcLoc

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
-- 'CallStackMessage'.
hydrateEventlogCallStackMessage :: SymbolTableReader -> BinaryCallStackMessage -> (CallStackMessage, [BinaryCallStackDecodeError])
hydrateEventlogCallStackMessage decodeTable msg =
  let
    decodeItem :: BinaryStackItem -> Either BinaryCallStackDecodeError StackItem
    decodeItem = \case
      BinaryIpe ipeId ->
        Right $ IpeId ipeId
      BinaryString stringId ->
        maybe
          (Left $ StringIdNotFound stringId)
          (Right . UserMessage . Text.unpack)
          (lookupStringId decodeTable stringId)
      BinarySourceLocation srcLocId ->
        maybe
          (Left $ SourceLocationIdNotFound srcLocId)
          (Right . SourceLocation)
          (lookupSourceLocationId decodeTable srcLocId)

    itemsOrErros = map decodeItem (binaryCallStack msg)
    (errors, items) = partitionEithers itemsOrErros
  in
    ( MkCallStackMessage
        { callCapabilityId = binaryCallCapabilityId msg
        , callThreadId = binaryCallThreadId msg
        , callStack = items
        }
    , errors
    )

-- | Combine all 'BinaryCallStackMessage's into a single 'BinaryCallStackMessage'.
-- We assume that all 'BinaryCallStackMessage' only differ in their 'binaryCallStack' values.
--
-- 'catCallStackMessage' is the inverse of 'chunkCallStackMessage'.
catCallStackMessage :: NonEmpty BinaryCallStackMessage -> BinaryCallStackMessage
catCallStackMessage msgs =
  MkBinaryCallStackMessage
    { binaryCallThreadId = binaryCallThreadId $ NonEmpty.head msgs
    , binaryCallCapabilityId = binaryCallCapabilityId $ NonEmpty.head msgs
    , binaryCallStack = concatMap binaryCallStack $ NonEmpty.toList msgs
    }

-- | Chunk the 'binaryCallStack' of the 'BinaryCallStackMessage' by the given 'Word16'.
-- If there are no items in 'BinaryCallStackMessage', then a singleton list is returned containing
-- the original element.
--
-- Post-condition for the result @r@:
--
-- * all elements in @init r @ are 'CallStackChunk's
-- * the element returned by @last r@ is a 'CallStackFinal' BinaryEventlogMessage.
chunkCallStackMessage :: Word16 -> BinaryCallStackMessage -> [BinaryEventlogMessage]
chunkCallStackMessage chunkSize msg0 =
  let
    items = binaryCallStack msg0
    chunked = chunksOf (word16ToInt chunkSize) items
  in
    go chunked
 where
  mkCallStack chunk =
    MkBinaryCallStackMessage
      { binaryCallThreadId = binaryCallThreadId msg0
      , binaryCallCapabilityId = binaryCallCapabilityId msg0
      , binaryCallStack = chunk
      }

  go :: [[BinaryStackItem]] -> [BinaryEventlogMessage]
  go [] =
    -- If there are no chunks, we simply return the original message
    [ CallStackFinal msg0
    ]
  go [chunk] =
    [ CallStackFinal $ mkCallStack chunk
    ]
  go (chunk : chunks) =
    CallStackChunk (mkCallStack chunk) : go chunks

-- ----------------------------------------------------------------------------
-- Helper types and functions to implement the conversion to the binary
-- representation.
-- ----------------------------------------------------------------------------

data EncodingState tbl = MkEncodingState
  { symbolTableWriter :: !(SymbolTableWriter tbl)
  , stringMessages :: ![BinaryStringMessage]
  , sourceLocMessages :: ![BinarySourceLocationMessage]
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

addStringMessage :: BinaryStringMessage -> State.State (EncodingState tbl) ()
addStringMessage msg = State.modify' (\st -> st{stringMessages = msg : stringMessages st})

addSourceLocationMessage :: BinarySourceLocationMessage -> State.State (EncodingState tbl) ()
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
      MkBinaryStringMessage sid s
  pure sid

lookupSourceLocationMessage :: forall tbl. SourceLocation -> State (EncodingState tbl) SourceLocationId
lookupSourceLocationMessage s = do
  (sid, new) <- lookupOrInsertSrcLocMessage s
  when new $ do
    nameId <- lookupTextMessage $ functionName s
    fileId <- lookupTextMessage $ fileName s
    addSourceLocationMessage $
      MkBinarySourceLocationMessage
        { binarySourceLocationMessageId = sid
        , binarySourceLocationRow = line s
        , binarySourceLocationColumn = column s
        , binarySourceLocationFunctionId = nameId
        , binarySourceLocationFilename = fileId
        }
  pure sid
