module GHC.Stack.Profiler.Core.Util (
  idToInt,
  putTextWord16,
  getTextWord16,
  showAsHex,
  putWord64,
  putWord32,
  putWord16,
  getWord64,
  getWord32,
  getWord16,
  word64ToWord32,
  word32ToWord64,
  word64ToWord16,
  word32ToInt,
  word64ToInt,
  intToWord64,
  intToWord32,
  intToWord16,
  word16ToInt,
  intToWord8,
  word8ToInt,
) where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Coerce (Coercible, coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Numeric

idToInt :: (Coercible a Word64) => a -> Int
idToInt = word64ToInt . coerce

putTextWord16 :: Word16 -> Text -> Put
putTextWord16 bound msg =
  putWord16 len <> putStringUtf8 (Text.unpack msg)
 where
  shortName = Text.take (word16ToInt bound) msg
  -- this is safe as 'bound' is a 'Word16' itself
  -- so the short string can be at most have a length of 'Word16'
  len = intToWord16 $ Text.length shortName

getTextWord16 :: Get Text
getTextWord16 = do
  len <- getWord16
  s <- replicateM (word16ToInt len) get
  pure $ Text.pack s

showAsHex :: (Integral a) => a -> String
showAsHex d = "0x" ++ Numeric.showHex d ""

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

word64ToWord16 :: Word64 -> Word16
word64ToWord16 = fromIntegral

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral

intToWord64 :: Int -> Word64
intToWord64 = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
