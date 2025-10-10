module Util where

import Data.ByteString.Builder
import Data.Word

cutOffLength :: Int
cutOffLength = 255

putStringLenWithTag :: Word8 -> String -> Builder
putStringLenWithTag tag msg =
  word8 tag <> putStringLen msg

putStringLen :: String -> Builder
putStringLen msg =
 word8 len <> stringUtf8 msg
  where
    shortName = take cutOffLength msg
    -- this is safe as we made sure that cutOffLength fits in a Word8
    len = intToWord8 $ length shortName

word64ToWord32 :: Word64 -> Word32
word64ToWord32 = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

putWord64 :: Word64 -> Builder
putWord64 = word64BE

putWord32 :: Word32 -> Builder
putWord32 = word32BE
