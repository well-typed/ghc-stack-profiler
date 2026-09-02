{-# LANGUAGE MultiWayIf #-}

module GHC.Stack.Profiler.Core.Eventlog (
  -- * Eventlog Message types
  Message (..),
  CallStackChunk (..),
  StringDef (..),
  SourceLocationDef (..),
  CallStackFrame (..),
  ThreadId (..),
  CapabilityId (..),
  StringId (..),
  nextStringId,
  SourceLocationId (..),
  nextSourceLocationId,
  IpeId (..),
  deserializeEventlogMessage,
  joinCallStackChunks,

  -- * Low-level API
  MessageTag (..),
  messageTagSize,
  CallStackFrameTag (..),
  ipeIdSize,
  stringIdSize,
  sourceLocationIdSize,
  messageMaxSize,
  messageMinSize,
  stringDefBodyMaxSize,
  sourceLocationDefSize,
  ShortText (..),
  toShortText,
  truncateTextToByteLimit,
  callStackFrameTagSize,
  callStackMaxLen,
  callStackMaxLen',
  callStackFrameSize,
  callStackFrameMaxSize,
  capabilityIdSize,
  threadIdSize,
) where

import Control.Exception (assert)
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get (getByteString, runGetOrFail)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import GHC.Generics
import GHC.Stack.Profiler.Core.Util
import Text.Printf (printf)

-- ----------------------------------------------------------------------------
-- Eventlog Messages
-- ----------------------------------------------------------------------------

-- | Efficient serialisation format of the GHC RTS callstack.
--
-- Message format:
--
-- @
-- 'Message'
--  := FF CA (stackFinal: 'CallStackChunk')
--   | FF CB (stackChunk: 'CallStackChunk')
--   | FF CC (stringDef: 'StringDef')
--   | FF CD (sourceLocationDef: 'SourceLocationDef')
--
-- 'CallStackChunk'
--  := (capabilityId: 'Word32') (threadId: 'Word32') (callStackLen: 'Word16') (callStack: 'CallStackFrame'{callStackLen})
--  -- NOTE: callStackLen must be smaller than (2^16 - 8) / 9
--
-- 'CallStackFrame'
--  := 01 (ipe: 'Word64')
--   | 02 (stringId: 'Word64')
--   | 03 (stringId: 'Word64') (sourceLocationId: 'Word64')
--
-- 'StringDef'
--  := (stringId: 'Word64') (stringLen: 'Word16') (string: 'Char'{stringLen})
--  -- NOTE: stringLen must be smaller than 2^16 - 8
--
-- 'SourceLocationDef'
--  := (sourceLocationId: 'Word64') (row: 'Word32') (column: 'Word32') (functionId: 'Word64') (filename: 'Word64')
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
    --   This messages associates the string ID @stringId@ with the string
    --   @strLen@, for future use in call-stack messages and source location
    --   definitions.
    StringDef !StringDef
  | -- | A source location definition, indicated by the prefix @FF CD@.
    --
    --   This message associates the source location ID @srcLocId@ with the
    --   source location specified by @row@, @col@, @functionId@, and
    --   @filename@, for future use in call-stack messages.
    SourceLocationDef !SourceLocationDef
  deriving (Eq, Ord, Show, Read, Generic)

data CallStackChunk = MkCallStackChunk
  { callStackChunkThreadId :: !ThreadId
  , callStackChunkCapabilityId :: !CapabilityId
  , callStackChunk :: ![CallStackFrame]
  }
  deriving (Eq, Ord, Show, Read, Generic)

data StringDef = MkStringDef
  { stringDefId :: !StringId
  , stringDefBody :: !Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

data SourceLocationDef = MkSourceLocationDef
  { sourceLocationDefId :: {-# UNPACK #-} !SourceLocationId
  , sourceLocationDefRow :: {-# UNPACK #-} !Word32
  , sourceLocationDefColumn :: {-# UNPACK #-} !Word32
  , sourceLocationDefFilename :: {-# UNPACK #-} !StringId
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CallStackFrame
  = CallStackFrameIpe {-# UNPACK #-} !IpeId
  | CallStackFrameAnn {-# UNPACK #-} !StringId {-# UNPACK #-} !(Maybe SourceLocationId)
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

nextStringId :: StringId -> StringId
nextStringId (MkStringId sid) = MkStringId (sid + 1)

newtype SourceLocationId = MkSourceLocationId
  { getSourceLocationId :: Word64
  }
  deriving (Eq, Ord, Show, Read, Generic)

nextSourceLocationId :: SourceLocationId -> SourceLocationId
nextSourceLocationId (MkSourceLocationId slId) = MkSourceLocationId (slId + 1)

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
-- 'joinCallStackChunks' is the conceptually inverse of 'chunkCallStack'.
joinCallStackChunks :: NonEmpty CallStackChunk -> CallStackChunk
joinCallStackChunks msgs =
  MkCallStackChunk
    { callStackChunkThreadId = callStackChunkThreadId $ NonEmpty.head msgs
    , callStackChunkCapabilityId = callStackChunkCapabilityId $ NonEmpty.head msgs
    , callStackChunk = concatMap (reverse . callStackChunk) . reverse $ NonEmpty.toList msgs
    }

-------------------------------------------------------------------------------
-- Binary instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Message Tags

data MessageTag
  = CallStackFinalTag
  | CallStackChunkTag
  | StringDefTag
  | SourceLocationDefTag
  deriving (Bounded, Enum, Eq, Show)

messageTagSize :: Int
messageTagSize = 2

messageTagToWord16 :: MessageTag -> Word16
messageTagToWord16 = \case
  CallStackFinalTag -> 0xFFCA
  CallStackChunkTag -> 0xFFCB
  StringDefTag -> 0xFFCC
  SourceLocationDefTag -> 0xFFCD

instance Binary MessageTag where
  put :: MessageTag -> Put
  put = putWord16 . messageTagToWord16

  get :: Get MessageTag
  get =
    getWord16 >>= \case
      0xFFCA -> pure CallStackFinalTag
      0xFFCB -> pure CallStackChunkTag
      0xFFCC -> pure StringDefTag
      0xFFCD -> pure SourceLocationDefTag
      badTag ->
        fail $
          printf
            "Found invalid message tag %s. Expected one of %s."
            (showAsHex badTag)
            (List.intercalate ", " messageTags)
   where
    messageTags :: [String]
    messageTags = [showAsHex (messageTagToWord16 tag) | tag <- [minBound .. maxBound]]

-------------------------------------------------------------------------------
-- Messages

instance Binary Message where
  put :: Message -> Put
  put = \case
    CallStackFinal callStackChunk -> do
      put CallStackFinalTag
      put callStackChunk
    CallStackChunk callStackChunk -> do
      put CallStackChunkTag
      put callStackChunk
    StringDef stringDef -> do
      put StringDefTag
      put stringDef
    SourceLocationDef sourceLocationDef -> do
      put SourceLocationDefTag
      put sourceLocationDef

  get :: Get Message
  get =
    get >>= \case
      CallStackFinalTag ->
        CallStackFinal <$> get
      CallStackChunkTag ->
        CallStackChunk <$> get
      StringDefTag ->
        StringDef <$> get
      SourceLocationDefTag ->
        SourceLocationDef <$> get

messageMaxSize :: Int
messageMaxSize =
  fromIntegral (maxBound @Word16)

messageMinSize :: Int
messageMinSize =
  messageTagSize
    + minimum
      [ {- CallStackChunk/CallStackFinal -}
        capabilityIdSize + threadIdSize + callStackLenSize
      , {- StringDef -}
        stringIdSize + stringDefBodyLenSize
      , {- SourceLocationDef -}
        sourceLocationDefSize
      ]

-------------------------------------------------------------------------------
-- ThreadId

instance Binary ThreadId where
  put :: ThreadId -> Put
  put (MkThreadId threadId) =
    -- TODO: This _should be_ a Word64.
    putWord32 (fromIntegral threadId)

  get :: Get ThreadId
  get = MkThreadId . fromIntegral <$> getWord32

threadIdSize :: Int
threadIdSize = 4

-------------------------------------------------------------------------------
-- CapabilityId

instance Binary CapabilityId where
  put :: CapabilityId -> Put
  put (MkCapabilityId capabilityId) =
    -- TODO: This _should be_ an Int or Word16.
    putWord32 (fromIntegral capabilityId)

  get :: Get CapabilityId
  get = MkCapabilityId . fromIntegral <$> getWord32

capabilityIdSize :: Int
capabilityIdSize = 4

-------------------------------------------------------------------------------
-- CallStackChunks

instance Binary CallStackChunk where
  put :: CallStackChunk -> Put
  put MkCallStackChunk{callStackChunkCapabilityId, callStackChunkThreadId, callStackChunk} = do
    put callStackChunkCapabilityId
    put callStackChunkThreadId
    let
      callStackChunkLength = length callStackChunk
    putWord16 $ fromIntegral callStackChunkLength
    mapM_ put callStackChunk

  get :: Get CallStackChunk
  get = do
    callStackChunkCapabilityId <- get
    callStackChunkThreadId <- get
    callStackChunkLength <- fromIntegral <$> getWord16
    callStackChunk <- replicateM callStackChunkLength get
    pure MkCallStackChunk{callStackChunkThreadId, callStackChunkCapabilityId, callStackChunk}

-------------------------------------------------------------------------------
-- CallStackFrameTags

data CallStackFrameTag
  = CallStackFrameIpeTag
  | CallStackFrameAnnWithNothingTag
  | CallStackFrameAnnWithJustSourceLocationTag
  deriving (Bounded, Enum, Eq, Show)

callStackFrameTagSize :: Int
callStackFrameTagSize = 1

callStackFrameTagToWord8 :: CallStackFrameTag -> Word8
callStackFrameTagToWord8 = \case
  CallStackFrameIpeTag -> 0x1
  CallStackFrameAnnWithNothingTag -> 0x2
  CallStackFrameAnnWithJustSourceLocationTag -> 0x3

instance Binary CallStackFrameTag where
  put :: CallStackFrameTag -> Put
  put = putWord8 . callStackFrameTagToWord8

  get :: Get CallStackFrameTag
  get =
    getWord8 >>= \case
      0x1 -> pure CallStackFrameIpeTag
      0x2 -> pure CallStackFrameAnnWithNothingTag
      0x3 -> pure CallStackFrameAnnWithJustSourceLocationTag
      badTag ->
        fail $
          printf
            "Found invalid call-stack frame tag %s. Expected one of %s."
            (showAsHex badTag)
            (List.intercalate ", " callStackFrameTags)
   where
    callStackFrameTags :: [String]
    callStackFrameTags = [showAsHex (callStackFrameTagToWord8 tag) | tag <- [minBound .. maxBound]]

-------------------------------------------------------------------------------
-- CallStackFrames

instance Binary IpeId where
  put :: IpeId -> Put
  put = putWord64 . coerce

  get :: Get IpeId
  get = coerce getWord64

ipeIdSize :: Int
ipeIdSize = 8

instance Binary CallStackFrame where
  put :: CallStackFrame -> Put
  put = \case
    CallStackFrameIpe ipeId -> do
      put CallStackFrameIpeTag
      put ipeId
    CallStackFrameAnn stringId Nothing -> do
      put CallStackFrameAnnWithNothingTag
      put stringId
    CallStackFrameAnn stringId (Just sourceLocationId) -> do
      put CallStackFrameAnnWithJustSourceLocationTag
      put stringId
      put sourceLocationId

  get :: Get CallStackFrame
  get = do
    get >>= \case
      CallStackFrameIpeTag ->
        CallStackFrameIpe <$> get
      CallStackFrameAnnWithNothingTag ->
        CallStackFrameAnn <$> get <*> pure Nothing
      CallStackFrameAnnWithJustSourceLocationTag ->
        CallStackFrameAnn <$> get <*> (Just <$> get)

-------------------------------------------------------------------------------
-- StringDefs

instance Binary StringId where
  put :: StringId -> Put
  put = putWord64 . coerce

  get :: Get StringId
  get = coerce getWord64

stringIdSize :: Int
stringIdSize = 8

instance Binary StringDef where
  put :: StringDef -> Put
  put MkStringDef{stringDefId, stringDefBody} = do
    put stringDefId
    put $ MkUnsafeShortText (truncateTextToByteLimit stringDefBodyMaxSize stringDefBody)

  get :: Get StringDef
  get = do
    stringDefId <- get
    -- NOTE: This allows reading stringDefBody with a lengthWord8 of up to
    --       the maxBound of Word16, which is bigger than stringDefBodyMaxSize.
    --       This causes a slight mismatch between the size of stringDefBody
    --       read by get and written by put, which means that get followed by
    --       put is not the identity. However, this would only truncate the
    --       stringDefBody if that binary representation was created manually,
    --     rather than via put, so this is likely not an issue.
    MkUnsafeShortText stringDefBody <- get
    pure MkStringDef{stringDefId, stringDefBody}

stringDefBodyLenSize :: Int
stringDefBodyLenSize = 2

stringDefBodyMaxSize :: Int
stringDefBodyMaxSize =
  messageMaxSize
    - messageTagSize
    - stringIdSize
    - stringDefBodyLenSize

-------------------------------------------------------------------------------
-- SourceLocationDefs

instance Binary SourceLocationId where
  put :: SourceLocationId -> Put
  put = putWord64 . coerce

  get :: Get SourceLocationId
  get = coerce getWord64

sourceLocationIdSize :: Int
sourceLocationIdSize = 8

instance Binary SourceLocationDef where
  put :: SourceLocationDef -> Put
  put msg = do
    put $ sourceLocationDefId msg
    putWord32 (sourceLocationDefRow msg)
    putWord32 (sourceLocationDefColumn msg)
    put (sourceLocationDefFilename msg)

  get :: Get SourceLocationDef
  get = MkSourceLocationDef <$> get <*> getWord32 <*> getWord32 <*> get

sourceLocationDefSize :: Int
sourceLocationDefSize =
  sourceLocationIdSize {- sourceLocationDefId -}
    + 4 {- sourceLocationDefRow -}
    + 4 {- sourceLocationDefColumn -}
    + stringIdSize {- sourceLocationDefFilename -}

-------------------------------------------------------------------------------
-- Trim Text to a byte-size limit

-- | A 'Text' whose 'TF.lengthWord8' is at most @'maxBound' :: 'Word16'@ bytes.
newtype ShortText = MkUnsafeShortText Text
  deriving (Eq, Show)

toShortText :: Text -> ShortText
toShortText text =
  MkUnsafeShortText (truncateTextToByteLimit maxBoundWord16 text)
 where
  maxBoundWord16 = fromIntegral (maxBound @Word16)

instance Binary ShortText where
  put :: ShortText -> Put
  put (MkUnsafeShortText text) = do
    putWord16 (fromIntegral (TF.lengthWord8 text))
    putByteString (TE.encodeUtf8 text)

  get :: Get ShortText
  get = do
    lengthWord8 <- fromIntegral <$> getWord16
    bytes <- getByteString lengthWord8
    pure $ MkUnsafeShortText (TE.decodeUtf8Lenient bytes)

-- | @'truncateTextToByteLimit' byteLimit text@ truncates @text@ such that its
--   UTF-8 serialisation fits within @byteLimit@ bytes.
truncateTextToByteLimit :: Int -> Text -> Text
truncateTextToByteLimit byteLimit text
  | TF.lengthWord8 text <= byteLimit = text
  | TF.lengthWord8 text' <= byteLimit = text'
  | otherwise =
      -- @'takeWord8' n@ takes the first n bytes and _expands_ to complete the
      -- last code point, which means it may return up to n+3 bytes. Hence, if
      -- this happens, we drop the final code point.
      assert (byteLimit < TF.lengthWord8 text' && TF.lengthWord8 text' <= byteLimit + 3) $
        T.dropEnd 1 text'
 where
  text' = TF.takeWord8 (fromIntegral byteLimit) text

-------------------------------------------------------------------------------
-- Size Invariants

callStackLenSize :: Int
callStackLenSize = 2

-- | The maximum number of `CallStackFrame`s in a single `Message`.
callStackMaxLen :: Int
callStackMaxLen = callStackMaxLen' messageMaxSize

-- | The maximum number of `CallStackFrame`s in a single `Message`,
--   with a variable `messageMaxSize`. Used for testing.
callStackMaxLen' :: Int -> Int
callStackMaxLen' messageMaxSize' =
  fromIntegral
    ( messageMaxSize'
        - messageTagSize
        - capabilityIdSize
        - threadIdSize
        - callStackLenSize
    )

-- | Size in bytes of the given 'CallStackFrame'
callStackFrameSize :: CallStackFrame -> Int
callStackFrameSize = \case
  CallStackFrameIpe{} ->
    1 {- CallStackFrameTag -}
      + 8 {- IpeId -}
  CallStackFrameAnn _ Nothing ->
    1 {- CallStackFrameTag -}
      + 8 {- StringId -}
  CallStackFrameAnn _ (Just _) ->
    1 {- CallStackFrameTag -}
      + 8 {- StringId -}
      + 8 {- SourceLocationId -}

callStackFrameMaxSize :: Int
callStackFrameMaxSize =
  17 {- see case for CallStackFrameAnn in callStackFrameSize -}
