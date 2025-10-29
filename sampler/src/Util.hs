module Util where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

cutOffLength :: Int
cutOffLength = 255

putStringLenWithTag :: Word8 -> String -> Put
putStringLenWithTag tag msg =
  putWord8 tag <> putStringLen msg

putStringLen :: String -> Put
putStringLen msg =
  putWord8 len <> putStringUtf8 msg
  where
    shortName = take cutOffLength msg
    -- this is safe as we made sure that cutOffLength fits in a Word8
    len = intToWord8 $ length shortName

getStringLen :: Get String
getStringLen = do
  len <- getWord8
  replicateM (word8ToInt len) get

putWord64 :: Word64 -> Put
putWord64 = putWord64be

putWord32 :: Word32 -> Put
putWord32 = putWord32be

putWord16 :: Word16 -> Put
putWord16 = putWord16be

getWord64 :: Get Word64
getWord64 = getWord64be

getWord32 :: Get Word32
getWord32 = getWord32be

getWord16 :: Get Word16
getWord16 = getWord16be

word64ToWord32 :: Word64 -> Word32
word64ToWord32 = fromIntegral

word32ToWord64 :: Word32 -> Word64
word32ToWord64 = fromIntegral

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral

intToWord64 :: Int -> Word64
intToWord64 = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

