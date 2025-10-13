{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module ThreadSample where

import Control.Concurrent
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (cast)
import Data.Maybe
import Unsafe.Coerce

import GHC.Stack.Annotation.Experimental

import GHC.Internal.Conc.Sync
import GHC.Internal.InfoProv.Types (InfoProv(..))
import GHC.Internal.Heap.Closures
import GHC.Internal.Stack.CloneStack
import qualified GHC.Internal.Stack.Decode as Decode
import GHC.Internal.Stack.Types

import Util

data ThreadSample =
  ThreadSample
    { threadSampleId :: ThreadId
    , threadSampleStackSnapshot :: StackSnapshot
    }

sampleThread :: ThreadId -> IO ThreadSample
sampleThread tid = do
  stack <- cloneThreadStack tid
  pure ThreadSample
    { threadSampleId = tid
    , threadSampleStackSnapshot = stack
    }

serializeThreadSample :: ThreadSample -> IO LBS.ByteString
serializeThreadSample sample = do
  callStackMessage <- threadSampleToCallStackMessage sample
  pure $ runPut $ put callStackMessage

deserializeCallStackMessage :: LBS.ByteString -> Either String CallStackMessage
deserializeCallStackMessage = Right . runGet get

-- Message format:
--
-- MESSAGE
--  := "CA" "11" <STACK>
-- STACK
--  := <threadId: Word32> <ENTRY>+
-- ENTRY
--  := "01" <ipe: Word64>
--   | "02" <string: STRING>
--   | "03" <row: Word32> <col: Word32> <function: CStringLen> <filename: CStringLen>
-- CStringLen
--   := <length: Word8> <Char>+

data CallStackMessage = MkCallStackMessage
  { callThreadId :: Word64
  , callStack :: [StackItem]
  } deriving (Eq, Ord, Show)

data StackItem
  = IpeId !Word64
  | UserMessage !String
  | SourceLocation !SourceLocation
  deriving (Eq, Ord, Show)

data SourceLocation = MkSourceLocation
  { line :: !Word32
  , column :: !Word32
  , functionName :: !String
  , fileName :: !String
  } deriving (Eq, Ord, Show)

instance Binary CallStackMessage where
  put msg = do
    putByteString "CA"
    putByteString "11"
    putWord32 $ word64ToWord32 $ callThreadId msg
    putWord8 $ intToWord8 $ length $ callStack msg -- TODO: limit number of stack entries to 255
    mapM_ put $ callStack msg

  get = annotateStackString "CallStackMessage" $ do
    _ <- getByteString 2 -- CA
    _ <- getByteString 2 -- 11
    tid <- getWord32
    len <- getWord8
    items <- replicateM (word8ToInt len) get
    pure MkCallStackMessage
      { callThreadId = word32ToWord64 tid
      , callStack = items
      }

instance Binary SourceLocation where
  put loc = do
    putWord32 $ line loc
    putWord32 $ column loc
    putStringLen $ functionName loc
    putStringLen $ fileName loc

  get = annotateStackString "SourceLocation" $ do
    MkSourceLocation
      <$> getWord32
      <*> getWord32
      <*> getStringLen
      <*> getStringLen

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

  get = annotateStackString "StackItem" $ do
    getWord8 >>= \ case
      1 -> IpeId <$> getWord64
      2 -> UserMessage <$> getStringLen
      3 -> SourceLocation <$> get
      n -> fail $ "StackItem: Unexpected tag byte encounter: " <> show n


threadSampleToCallStackMessage :: ThreadSample -> IO CallStackMessage
threadSampleToCallStackMessage sample = do
  frames <- Decode.decodeStackWithIpe $ threadSampleStackSnapshot sample
  let stackMessages = mapMaybe (uncurry stackFrameToStackItem) frames
  pure MkCallStackMessage
    { callThreadId = fromThreadId $ threadSampleId sample
    , callStack = stackMessages
    }

stackFrameToStackItem :: StackFrame -> Maybe InfoProv -> Maybe StackItem
stackFrameToStackItem frame mIpe =
  case frame of
    AnnFrame { annotation = Box someStackAnno } ->
      case unsafeCoerce someStackAnno of
        SomeStackAnnotation ann ->
          case cast ann of
            Just (CallStackAnnotation cs) ->
              case getCallStack cs of
                [] -> Nothing
                ((name, sourceLoc):_) ->
                  Just $ SourceLocation $ MkSourceLocation
                    { line = intToWord32 $ srcLocStartLine sourceLoc
                    , column = intToWord32 $ srcLocStartCol sourceLoc
                    , functionName = name
                    , fileName = srcLocFile sourceLoc
                    }

            Nothing -> case cast ann of
              Just (StringAnnotation msg) ->
                Just $ UserMessage msg
              Nothing ->
                Nothing
    _ ->
      IpeId . infoProvId <$> mIpe

infoProvId :: InfoProv -> Word64
infoProvId (InfoProv {ipProvId}) =
  ipProvId
