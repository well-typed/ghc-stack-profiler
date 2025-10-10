{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ThreadSample where

import Control.Concurrent
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (cast)
import Data.Word
import Data.Maybe
import Unsafe.Coerce

import GHC.Stack.Annotation.Experimental

import GHC.Internal.Conc.Sync
import GHC.Internal.InfoProv.Types (InfoProv(..))
import GHC.Internal.Heap.Closures
import GHC.Internal.Stack.CloneStack
import GHC.Internal.Stack.Decode
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
  frames <- decodeStackWithIpe $ threadSampleStackSnapshot sample
  pure $ toLazyByteString $ threadStackMessage (threadSampleId sample) frames

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

threadStackMessage :: ThreadId -> [(StackFrame, Maybe InfoProv)] -> Builder
threadStackMessage tid entries =
  mconcat
    [ byteString "CA"
    , byteString "11"
    , putWord32 (word64ToWord32 $ fromThreadId tid)
    , payload
    ]
  where
    payload = mconcat $ fmap (uncurry encodeEntry) entries

encodeEntry :: StackFrame -> Maybe InfoProv -> Builder
encodeEntry frame mIpe =
  case frame of
    AnnFrame {annotation = Box someStackAnno } ->
      case unsafeCoerce someStackAnno of
        SomeStackAnnotation ann ->
          case cast ann of
            Just callStackAnn -> encodeCallStackAnno callStackAnn
            Nothing -> case cast ann of
              Just stringAnn -> putStringLenAnno stringAnn
              Nothing -> mempty
    _ ->
      maybe mempty encodeInfoProv mIpe

ipeEntry :: Word8
ipeEntry = 1

stringEntry :: Word8
stringEntry = 2

sourceLocEntry :: Word8
sourceLocEntry = 3

encodeInfoProv :: InfoProv -> Builder
encodeInfoProv (InfoProv {ipProvId}) =
  word8 ipeEntry <> putWord64 ipProvId

putStringLenAnno :: StringAnnotation -> Builder
putStringLenAnno (StringAnnotation msg) =
  putStringLenWithTag stringEntry msg

encodeCallStackAnno :: CallStackAnnotation -> Builder
encodeCallStackAnno (CallStackAnnotation cs) = case getCallStack cs of
  [] -> mempty
  ((name, sourceLoc):_) ->
    word8 sourceLocEntry <> encodeSourceLocation name sourceLoc

encodeSourceLocation :: String -> SrcLoc -> Builder
encodeSourceLocation name srcLoc =
  mconcat
    [ putWord32 (intToWord32 $ srcLocStartLine srcLoc)
    , putWord32 (intToWord32 $ srcLocStartCol srcLoc)
    , putStringLen (srcLocFile srcLoc)
    , putStringLen name
    ]
