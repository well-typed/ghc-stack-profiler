module GHC.Stack.Profiler.Util (
  castPtrToWord64,
) where

import Data.Word
import Foreign.Ptr

castPtrToWord64 :: Ptr a -> Word64
castPtrToWord64 ptr = case ptrToWordPtr ptr of
  WordPtr w -> fromIntegral w -- On platforms that use 32-bit systems, the key is still Word64
