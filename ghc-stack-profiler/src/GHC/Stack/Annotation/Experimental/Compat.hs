{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module GHC.Stack.Annotation.Experimental.Compat (
  SomeStackAnnotation (..),
  showStackAnnotationLocation,
  showStackAnnotationDescription,
) where

#if MIN_VERSION_ghc_internal(9,1400,0)
import GHC.Stack.Annotation.Experimental
#else
import Data.Typeable
#endif
import GHC.Stack.Types (SrcLoc)

#if !MIN_VERSION_ghc_internal(9,1400,0)
data SomeStackAnnotation where
  SomeStackAnnotation :: forall a. (Typeable a) => a -> SomeStackAnnotation
#endif

showStackAnnotationLocation :: SomeStackAnnotation -> Maybe SrcLoc
#if MIN_VERSION_ghc_internal(9,1402,0)
showStackAnnotationLocation =
  stackAnnotationSourceLocation
#else
showStackAnnotationLocation =
  \ _ann -> Nothing
#endif

showStackAnnotationDescription :: SomeStackAnnotation -> String
#if MIN_VERSION_ghc_internal(9,1402,0)
showStackAnnotationDescription =
  displayStackAnnotationShort
#elif MIN_VERSION_ghc_internal(9,1400,0)
showStackAnnotationDescription =
  displayStackAnnotation
#else
showStackAnnotationDescription =
  \ _ann -> "showStackAnnotationLocation: Impossible, no value should be created with ghc-internal < 9.1400"
#endif
