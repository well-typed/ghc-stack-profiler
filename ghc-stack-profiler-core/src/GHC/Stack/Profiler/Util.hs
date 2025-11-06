module GHC.Stack.Profiler.Util where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Coerce (coerce, Coercible)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Numeric

idToInt :: Coercible a Word64 => a -> Int
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

showAsHex :: Integral a => a -> String
showAsHex d = Numeric.showHex d ""

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

-- | @'chunksOf' n@ splits a list into length-n pieces.  The last
--   piece will be shorter if @n@ does not evenly divide the length of
--   the list.  If @n <= 0@, @'chunksOf' n l@ returns an infinite list
--   of empty lists.
--
-- >>> chunksOf 3 [1..12]
-- [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
--
-- >>> chunksOf 3 "Hello there"
-- ["Hel","lo ","the","re"]
--
-- >>> chunksOf 3 ([] :: [Int])
-- []
--
--   Note that @'chunksOf' n []@ is @[]@, not @[[]]@.  This is
--   intentional, and satisfies the property that
--
--   @chunksOf n xs ++ chunksOf n ys == chunksOf n (xs ++ ys)@
--
--   whenever @n@ evenly divides the length of @xs@.
--
--
-- This is the definition of 'chunksOf' as defined in
-- https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split-Internals.html#v:chunksOf
--
-- We avoid an extra dependency on 'split', as we want to
-- only depend on GHC boot libraries in the core package.
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []
