{-# LANGUAGE CPP #-}
module GHC.Internal.Stack.Constants.Compat (
  WordOffset(..),
  offsetStgAnnFrameAnn,
) where

#if MIN_VERSION_ghc_internal(9,1400,0)
#if defined(PROFILING)
import GHC.Internal.Stack.ConstantsProf
#else
import GHC.Internal.Stack.Constants
#endif
#else
import GHC.Exts.Stack.Constants

offsetStgAnnFrameAnn :: WordOffset
offsetStgAnnFrameAnn = error "offsetStgAnnFrameAnn is only available in GHC >=9.14"
#endif

