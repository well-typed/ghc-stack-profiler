module GHC.Stack.Profiler.Core.Eventlog (
  -- * Eventlgog Message types
  Message (..),
  CallStackChunk (..),
  BinaryStringMessage (..),
  BinarySourceLocationMessage (..),
  BinaryStackItem (..),
  ThreadId (..),
  CapabilityId (..),
  StringId (..),
  incrementStringLocationId,
  SourceLocationId (..),
  incrementSourceLocationId,
  IpeId (..),
  deserializeEventlogMessage,
  joinCallStackChunks,

  -- * Eventlog constants
  callStackFinalMessageTag,
  callStackPartialMessageTag,
  callStackStringMessageTag,
  callStackSourceLocationMessageTag,
  callStackMessageTags,
  callStackSizeLimit,
  callStackSizeLimit_,
  byteSizeOf,
  eventlogBufferSize,
  stringLengthLimit,
) where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import GHC.Generics
import GHC.Stack.Profiler.Core.Util

-- ----------------------------------------------------------------------------
-- Eventlog Messages
-- ----------------------------------------------------------------------------

-- | Efficient serialisation format of the GHC RTS callstack.
--
-- Message format:
--
-- @
-- MESSAGE
--  := FF CA (stackFinal: CALL_STACK)
--   | FF CB (stackChunk: CALL_STACK)
--   | FF CC (strDef: STR_DEF)
--   | FF CD (srcLocDef: SRC_LOC_DEF)
--
-- CALL_STACK
--  := (capability: 'Word32') (threadId: 'Word32') (callStackLen: 'Word16') (callStack: CALL_STACK_FRAME{callStackLen})
--  -- NOTE: callStackLen must be smaller than (2^16 - 8) / 9
--
-- CALL_STACK_FRAME
--  := 01 (ipe: 'Word64')
--   | 02 (strId: 'Word64')
--   | 03 (strId: 'Word64') (srcLocId: 'Word64')
--
-- STR_DEF
--  := (strId: 'Word64') (strLen: 'Word16') (str: 'Char'{strLen})
--  -- NOTE: strLen must be smaller than 2^16 - 8
--
-- SRC_LOC_DEF
--  := (srcLocId: 'Word64') (row: 'Word32') (col: 'Word32') (functionId: 'Word64') (filename: 'Word64')
-- @
data Message
  = -- | A chunk of the call-stack, indicated by the prefix @FF CA@.
    --
    --   This variant indicates that no further 'CallStackChunk' or 'CallStackFinal' will follow.
    CallStackFinal !CallStackChunk
  | -- | A chunk of the call-stack, indicated by the prefix @FF CB@.
    --
    --   This variant indicates that another 'CallStackChunk' or 'CallStackFinal' will follow.
    CallStackChunk !CallStackChunk
  | -- | A string definition, indicated by the prefix @FF CC@.
    --
    --   This messages associates the string ID @strId@ with the string
    --   @strLen@, for future use in call-stack messages and source location
    --   definitions.
    StringDef !BinaryStringMessage
  | -- | A source location definition, indicated by the prefix @FF CD@.
    --
    --   This message associates the source location ID @srcLocId@ with the
    --   source location specified by @row@, @col@, @functionId@, and
    --   @filename@, for future use in call-stack messages.
    SourceLocationDef !BinarySourceLocationMessage
  deriving (Eq, Ord, Show, Read, Generic)

data CallStackChunk = MkCallStackChunk
  { callStackChunkThreadId :: !ThreadId
  , callStackChunkCapabilityId :: !CapabilityId
  , callStackChunk :: ![BinaryStackItem]
  }
  deriving (Eq, Ord, Show, Read, Generic)

data BinaryStringMessage = MkBinaryStringMessage
  { binaryStringMessageId :: !StringId
  , binaryStringMessage :: !Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

data BinarySourceLocationMessage = MkBinarySourceLocationMessage
  { binarySourceLocationMessageId :: {-# UNPACK #-} !SourceLocationId
  , binarySourceLocationRow :: {-# UNPACK #-} !Word32
  , binarySourceLocationColumn :: {-# UNPACK #-} !Word32
  , binarySourceLocationFilename :: {-# UNPACK #-} !StringId
  }
  deriving (Eq, Ord, Show, Read, Generic)

data BinaryStackItem
  = BinaryIpe {-# UNPACK #-} !IpeId
  | BinaryMessage
      {-# UNPACK #-} !StringId
      {-# UNPACK #-} !(Maybe SourceLocationId)
  deriving (Eq, Ord, Show, Read, Generic)

-- | The ID of a thread.
newtype ThreadId
  = MkThreadId
  { getThreadId :: Word64
  }
  deriving (Show, Eq, Ord, Read, Generic)

-- | The ID of a capability.
newtype CapabilityId
  = MkCapabilityId
  { getCapabilityId :: Int
  }
  deriving (Show, Eq, Ord, Read, Generic)

newtype StringId = MkStringId
  { getStringId :: Word64
  }
  deriving (Eq, Ord, Show, Read, Generic)

incrementStringLocationId :: StringId -> StringId
incrementStringLocationId (MkStringId sid) = MkStringId (sid + 1)

newtype SourceLocationId = MkSourceLocationId
  { getSourceLocationId :: Word64
  }
  deriving (Eq, Ord, Show, Read, Generic)

incrementSourceLocationId :: SourceLocationId -> SourceLocationId
incrementSourceLocationId (MkSourceLocationId slId) = MkSourceLocationId (slId + 1)

newtype IpeId = MkIpeId
  { getIpeId :: Word64
  }
  deriving (Eq, Ord, Show, Read, Generic)

deserializeEventlogMessage :: LBS.ByteString -> Either String Message
deserializeEventlogMessage msg = case runGetOrFail get msg of
  Left (_, _, errMsg) -> Left errMsg
  Right (_, _, callStackMessage) -> Right callStackMessage

-- | Combine all 'CallStackChunk's into a single 'CallStackChunk'.
-- We assume that all 'CallStackChunk' only differ in their 'callStackChunk' values.
--
-- 'joinCallStackChunks' is the conceptually inverse of 'chunkCallStackMessage'.
joinCallStackChunks :: NonEmpty CallStackChunk -> CallStackChunk
joinCallStackChunks msgs =
  MkCallStackChunk
    { callStackChunkThreadId = callStackChunkThreadId $ NonEmpty.head msgs
    , callStackChunkCapabilityId = callStackChunkCapabilityId $ NonEmpty.head msgs
    , callStackChunk = concatMap (reverse . callStackChunk) . reverse $ NonEmpty.toList msgs
    }

-- ----------------------------------------------------------------------------
-- Binary instances
-- ----------------------------------------------------------------------------

callStackFinalMessageTag :: Word16
callStackFinalMessageTag = 0xFFCA

callStackPartialMessageTag :: Word16
callStackPartialMessageTag = 0xFFCB

callStackStringMessageTag :: Word16
callStackStringMessageTag = 0xFFCC

callStackSourceLocationMessageTag :: Word16
callStackSourceLocationMessageTag = 0xFFCD

callStackMessageTags :: [Word16]
callStackMessageTags =
  [ callStackFinalMessageTag
  , callStackPartialMessageTag
  , callStackStringMessageTag
  , callStackSourceLocationMessageTag
  ]

-- | Each message in the eventlog can be at most 2^16 bytes
eventlogBufferSize :: Word64
eventlogBufferSize = (2 :: Word64) ^ (16 :: Word64)

-- | Size limit of strings that can occur in the eventlog.
stringLengthLimit :: Word16
stringLengthLimit =
  word64ToWord16 $
    eventlogBufferSize
      - 2 {- 0xFFCC -}
      - 8 {- Word64 of 'StringId' -}
      - 2 {- Word16 for the length of the string to serialise -}

-- | The limit of stack items that can go in one eventlog message in bytes.
callStackSizeLimit :: Word16
callStackSizeLimit =
  callStackSizeLimit_ eventlogBufferSize

-- | The limit of stack items that can go in one eventlog message in bytes
-- with configurable the eventlog message size.
callStackSizeLimit_ :: Word64 -> Word16
callStackSizeLimit_ eventlogSize =
  word64ToWord16
    ( eventlogSize
        - 2 {- 0xFFCA or 0xFFCB -}
        - 4 {- Word32 of 'CapabilityId' -}
        - 4 {- Word32 of 'ThreadId' -}
        - 2 {- Word16 for the length of stack entry -}
    )

-- | Size in bytes of the given 'BinaryStackItem'
byteSizeOf :: BinaryStackItem -> Word16
byteSizeOf = \case
  BinaryIpe{} -> 1 + 8 {- 0x1 + Word64 of 'IpeId' -}
  BinaryMessage _ Nothing -> 1 + 8 {- 0x2 + Word64 of 'StringId' -}
  BinaryMessage _ (Just _) -> 1 + 8 + 8 {- 0x3 + Word64 of 'StringId' + Word64 of 'SourceLocationId' -}

instance Binary Message where
  put = \case
    CallStackFinal msg ->
      putWithTag callStackFinalMessageTag msg
    CallStackChunk msg ->
      putWithTag callStackPartialMessageTag msg
    StringDef msg ->
      putWithTag callStackStringMessageTag msg
    SourceLocationDef msg ->
      putWithTag callStackSourceLocationMessageTag msg
   where
    putWithTag t msg = putWord16 t >> put msg

  get = do
    tag <- getWord16
    case tag of
      _
        | tag == callStackFinalMessageTag ->
            CallStackFinal <$> get
        | tag == callStackPartialMessageTag ->
            CallStackChunk <$> get
        | tag == callStackStringMessageTag ->
            StringDef <$> get
        | tag == callStackSourceLocationMessageTag ->
            SourceLocationDef <$> get
        | otherwise ->
            fail $
              "Message.get: Unknown tag expected one of "
                ++ tags
                ++ " but got "
                ++ showAsHex tag
   where
    tags = List.intercalate ", " $ map showAsHex callStackMessageTags

instance Binary CallStackChunk where
  put msg = do
    putWord32 . fromIntegral . getCapabilityId $ callStackChunkCapabilityId msg
    putWord32 . fromIntegral . getThreadId $ callStackChunkThreadId msg
    -- TODO: This _should be_ a Word64.
    let
      items = callStackChunk msg
    putWord16 $ intToWord16 $ length items
    mapM_ put items

  get = do
    capId <- getWord32
    tid <- getWord32 -- TODO: This _should be_ a Word64.
    len <- getWord16
    items <- replicateM (word16ToInt len) get
    pure
      MkCallStackChunk
        { callStackChunkThreadId = MkThreadId . fromIntegral $ tid
        , callStackChunkCapabilityId = MkCapabilityId . fromIntegral $ capId
        , callStackChunk = items
        }

instance Binary BinaryStackItem where
  put = \case
    BinaryIpe ipeId -> do
      putWord8 0x1
      put ipeId
    BinaryMessage sid Nothing -> do
      putWord8 0x2
      put sid
    BinaryMessage sid (Just lid) -> do
      putWord8 0x3
      put sid
      put lid

  get = do
    getWord8 >>= \case
      0x1 -> BinaryIpe <$> get
      0x2 -> BinaryMessage <$> get <*> pure Nothing
      0x3 -> BinaryMessage <$> get <*> (Just <$> get)
      n -> fail $ "StackItem: Unexpected tag byte encounter: " <> show n

instance Binary BinarySourceLocationMessage where
  put msg = do
    put $ binarySourceLocationMessageId msg
    putWord32 (binarySourceLocationRow msg)
    putWord32 (binarySourceLocationColumn msg)
    put (binarySourceLocationFilename msg)

  get = do
    MkBinarySourceLocationMessage
      <$> get
      <*> getWord32
      <*> getWord32
      <*> get

instance Binary BinaryStringMessage where
  put msg = do
    put $ binaryStringMessageId msg
    putTextWord16 stringLengthLimit (binaryStringMessage msg)

  get = do
    MkBinaryStringMessage
      <$> get
      <*> getTextWord16

instance Binary SourceLocationId where
  put = putWord64 . coerce
  get = coerce getWord64

instance Binary StringId where
  put = putWord64 . coerce
  get = coerce getWord64

instance Binary IpeId where
  put = putWord64 . coerce
  get = coerce getWord64
