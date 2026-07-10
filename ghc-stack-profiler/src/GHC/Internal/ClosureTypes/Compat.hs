{-# LANGUAGE CPP #-}

module GHC.Internal.ClosureTypes.Compat (
  ClosureType (ANN_FRAME, ..),
) where

import GHC.Internal.ClosureTypes (ClosureType (..))

#if !MIN_VERSION_ghc_internal(9,1400,0)
-- Unmatchable, as ANN_FRAME is a stack frame type introduced in GHC 9.14
pattern ANN_FRAME :: ClosureType
pattern ANN_FRAME <- (const Nothing -> Just _)
#endif
