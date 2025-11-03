{-# LANGUAGE OverloadedStrings #-}

module GHC.Stack.Profiler.ThreadSample where

import Control.Concurrent
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.Stack.CloneStack (StackSnapshot)

import GHC.Stack.Profiler.Util

newtype CapabilityId =
  CapabilityId
    { getCapabilityId :: Word64
    }
  deriving (Show, Eq, Ord)

data ThreadSample =
  ThreadSample
    { threadSampleId :: ThreadId
    , threadSampleCapability :: CapabilityId
    , threadSampleStackSnapshot :: StackSnapshot
    }

deserializeCallStackMessage :: LBS.ByteString -> Either String CallStackMessage
deserializeCallStackMessage = Right . runGet get

-- | A fully decoded rts callstack that can be serialised to the EventLog.
--
-- Message format:
--
-- @
--  MESSAGE
--     := "CA" "11" <STACK>
--  STACK
--     := <capability: Word32> <threadId: Word32> <length: Word8> <ENTRY>+
--  ENTRY
--     := "01" <ipe: Word64>
--      | "02" <string: STRING>
--      | "03" <row: Word32> <col: Word32> <function: CStringLen> <filename: CStringLen>
--  CStringLen
--      := <length: Word8> <Char>+
-- @
--
data CallStackMessage =
  MkCallStackMessage
    { callThreadId :: Word64
    , callCapabilityId :: CapabilityId
    , callStack :: [StackItem]
    }
  deriving (Eq, Ord, Show)

data StackItem
  = IpeId !Word64
  | UserMessage !String
  | SourceLocation !SourceLocation
  deriving (Eq, Ord, Show)

data SourceLocation =
  MkSourceLocation
    { line :: !Word32
    , column :: !Word32
    , functionName :: !Text
    , fileName :: !Text
    }
  deriving (Eq, Ord, Show)

instance Binary CallStackMessage where
  put msg = do
    putByteString "CA"
    putByteString "11"
    putWord32 $ word64ToWord32 $ getCapabilityId $ callCapabilityId msg
    putWord32 $ word64ToWord32 $ callThreadId msg
    -- limit number of items to serialise to 'cutOffLength'
    let items = take cutOffLength $ callStack msg
    putWord8 $ intToWord8 $ length items
    mapM_ put items

  get = do
    _ <- getByteString 2 -- CA
    _ <- getByteString 2 -- 11
    capId <- getWord32
    tid <- getWord32
    len <- getWord8
    items <- replicateM (word8ToInt len) get
    pure MkCallStackMessage
      { callThreadId = word32ToWord64 tid
      , callCapabilityId = CapabilityId $ word32ToWord64 capId
      , callStack = items
      }

instance Binary SourceLocation where
  put loc = do
    putWord32 $ line loc
    putWord32 $ column loc
    putStringLen $ Text.unpack $ functionName loc
    putStringLen $ Text.unpack $ fileName loc

  get = do
    MkSourceLocation
      <$> getWord32
      <*> getWord32
      <*> (Text.pack <$> getStringLen)
      <*> (Text.pack <$> getStringLen)

instance Binary StackItem where
  put = \ case
    IpeId ipeId -> do
      putWord8 1
      putWord64 ipeId
    UserMessage msg -> do
      putWord8 2
      putStringLen msg
    SourceLocation loc -> do
      putWord8 3
      put loc

  get = do
    getWord8 >>= \ case
      1 -> IpeId <$> getWord64
      2 -> UserMessage <$> getStringLen
      3 -> SourceLocation <$> get
      n -> fail $ "StackItem: Unexpected tag byte encounter: " <> show n
