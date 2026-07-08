module GHC.Stack.Profiler.Core.Eventlog (
  -- * Eventlgog Message types
  BinaryEventlogMessage (..),
  BinaryCallStackMessage (..),
  BinaryStringMessage (..),
  BinarySourceLocationMessage (..),
  BinaryStackItem (..),
  CapabilityId (..),
  StringId (..),
  incrementStringLocationId,
  SourceLocationId (..),
  incrementSourceLocationId,
  IpeId (..),

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
import Data.Coerce (coerce)
import qualified Data.List as List
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
--  := FF CA (stack: STACK>
--   | FF CB (prefix: STACK>
--   | FF CC (stringId: 'Word64') (string: CStringLen)
--   | FF CD (srcLocId: 'Word64') (row: 'Word32') (col: 'Word32') (functionId: 'Word64') (filename: 'Word64')
--
-- STACK
--  := (capability: 'Word32') (threadId: 'Word32') (length: 'Int16') (ENTRY)+
--   # check that length < (2^16-8) / 9
--
-- ENTRY
--  := 01 (ipe: 'Word64')
--   | 02 (stringId: 'Word64')
--   | 03 (stringId: 'Word64') (srcLocId: 'Word64')
--
-- CStringLen
--   := (length: 'Int16') (Char)+
--    # check that length < 2^16-8
-- @
data BinaryEventlogMessage
  = CallStackFinal !BinaryCallStackMessage
  | CallStackChunk !BinaryCallStackMessage
  | StringDef !BinaryStringMessage
  | SourceLocationDef !BinarySourceLocationMessage
  deriving (Eq, Ord, Show, Read, Generic)

data BinaryCallStackMessage = MkBinaryCallStackMessage
  { binaryCallThreadId :: !Word64
  , binaryCallCapabilityId :: !CapabilityId
  , binaryCallStack :: ![BinaryStackItem]
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

-- | Simple newtype for the ID of a capability.
newtype CapabilityId
  = MkCapabilityId
  { getCapabilityId :: Word64
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

instance Binary BinaryEventlogMessage where
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
              "BinaryEventlogMessage.get: Unknown tag expected one of "
                ++ tags
                ++ " but got "
                ++ showAsHex tag
   where
    tags = List.intercalate ", " $ map showAsHex callStackMessageTags

instance Binary BinaryCallStackMessage where
  put msg = do
    putWord32 $ word64ToWord32 $ getCapabilityId $ binaryCallCapabilityId msg
    putWord32 $ word64ToWord32 $ binaryCallThreadId msg
    let
      items = binaryCallStack msg
    putWord16 $ intToWord16 $ length items
    mapM_ put items

  get = do
    capId <- getWord32
    tid <- getWord32
    len <- getWord16
    items <- replicateM (word16ToInt len) get
    pure
      MkBinaryCallStackMessage
        { binaryCallThreadId = word32ToWord64 tid
        , binaryCallCapabilityId = MkCapabilityId $ word32ToWord64 capId
        , binaryCallStack = items
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
