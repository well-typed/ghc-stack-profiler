{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module MyLib where

import Control.Concurrent
import Unsafe.Coerce
import Data.ByteString.Builder
import Data.ByteString ()
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Typeable (cast)
import Data.Word

import GHC.Stack.Annotation.Experimental

import GHC.Internal.Conc.Sync
import GHC.Internal.InfoProv.Types (InfoProv(..))
import GHC.Internal.Heap.Closures
import GHC.Internal.Stack.CloneStack
import GHC.Internal.Stack.Decode
import GHC.Internal.Stack.Types

import Debug.Trace (traceEventIO)
import Debug.Trace.Binary

sampleToEventlog :: ThreadId -> IO ()
sampleToEventlog tid = do
  stack <- cloneThreadStack tid
  cs <- decodeStackWithIpe stack
  let msg = buildMessage tid cs
  traceBinaryEventIO $ LBS.toStrict $ toLazyByteString msg

-- Message format:
--
-- MESSAGE
--  := "CA" "11" <STACK>
-- STACK
--  := <THREAD_ID> <ENTRY>+
-- ENTRY
--  := "01" <ENTRY_LENGTH> <IPE>
--   | "02" <ENTRY_LENGTH> <CHAR>+
--   | "03" <ROW> <COL> <FILEPATH> <FUNCTION_NAME>
-- FILEPATH
--   := <FILEPATH_LENGTH> <CHAR>+
-- FUNCTION_NAME
--   := <FUNCTION_NAME_LENGTH> <CHAR>+

cutOffLength :: Int
cutOffLength = 255

buildMessage :: ThreadId -> [(StackFrame, Maybe InfoProv)] -> Builder
buildMessage tid entries =
  byteString "CA" <> byteString "11" <> word32BE (fromIntegral @Word64 @Word32 $ fromThreadId tid)

-- encodeEntry :: StackFrame -> Maybe InfoProv -> Builder
-- encodeEntry frame mIpe =
--   case frame of
--     AnnFrame {annotation = Box someStackAnno } ->
--       case unsafeCoerce someStackAnno of
--         SomeStackAnnotation ann ->
--           case cast ann of
--             Just callStackAnn -> encodeCallStackAnno callStackAnn
--             Nothing -> case cast ann of
--               Just stringAnn -> encodeStringAnno stringAnn
--               Nothing -> mempty
--     _ ->
--       maybe mempty encodeInfoProv mIpe

-- ipeEntry :: Word8
-- ipeEntry = 1

-- stringEntry :: Word8
-- stringEntry = 2

-- sourceLocEntry :: Word8
-- sourceLocEntry = 3

-- encodeInfoProv :: InfoProv -> Builder
-- encodeInfoProv (InfoProv {ipName}) =
--   encodeStringWithTag ipeEntry ipName

-- encodeStringAnno :: StringAnnotation -> Builder
-- encodeStringAnno (StringAnnotation msg) =
--   encodeStringWithTag stringEntry msg

-- encodeCallStackAnno :: CallStackAnnotation -> Builder
-- encodeCallStackAnno (CallStackAnnotation cs) = case getCallStack cs of
--   [] -> mempty
--   ((name, sourceLoc):_) ->
--     let
--       encodedSourceLoc = toLazyByteString $ encodeSourceLocation name sourceLoc
--       len = LBS.length encodedSourceLoc
--     in
--       word8 (len + 1) <> word8 sourceLocEntry <> lazyByteString encodedSourceLoc

-- encodeSourceLocation :: String -> SrcLoc -> Builder
-- encodeSourceLocation name srcLoc =
--   mconcat
--     [ word32BE (intToWord32 $ srcLocStartLine srcLoc)
--     , word32BE (intToWord32 $ srcLocStartCol srcLoc)
--     , encodeString (srcLocFile srcLoc)
--     , encodeString name
--     ]

-- encodeStringWithTag :: Word8 -> String -> Builder
-- encodeStringWithTag tag msg =
--  word8 (len + 1) <> word8 tag <> stringUtf8 msg
--   where
--     shortName = take cutOffLength msg
--     -- this is safe as we made sure that cutOffLength fits in a Word8
--     len = intToWord8 $ length shortName

-- encodeString :: String -> Builder
-- encodeString msg =
--  word8 len <> stringUtf8 msg
--   where
--     shortName = take cutOffLength msg
--     -- this is safe as we made sure that cutOffLength fits in a Word8
--     len = intToWord8 $ length shortName

-- intToWord32 :: Int -> Word32
-- intToWord32 = fromIntegral

-- intToWord8 :: Int -> Word8
-- intToWord8 = fromIntegral
