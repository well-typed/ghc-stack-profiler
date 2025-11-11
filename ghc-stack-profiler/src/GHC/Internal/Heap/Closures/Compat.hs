{-# LANGUAGE CPP #-}
module GHC.Internal.Heap.Closures.Compat (
  Box(..),
) where

#if MIN_VERSION_ghc_internal(9,1400,0)
import GHC.Internal.Heap.Closures ( Box(..) )
#else
import GHC.Exts.Heap.Closures ( Box(..) )
#endif
