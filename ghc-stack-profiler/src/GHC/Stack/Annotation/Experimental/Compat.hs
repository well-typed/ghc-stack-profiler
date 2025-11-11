{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module GHC.Stack.Annotation.Experimental.Compat (
  SomeStackAnnotation(..),
  CallStackAnnotation(..),
  StringAnnotation(..),
) where

#if MIN_VERSION_ghc_internal(9,1400,0)
import GHC.Stack.Annotation.Experimental
#else
import Data.Typeable
import GHC.Stack.Types (CallStack)

data SomeStackAnnotation where
  SomeStackAnnotation :: forall a. (Typeable a) => a -> SomeStackAnnotation

data StringAnnotation where
  StringAnnotation :: String -> StringAnnotation

newtype CallStackAnnotation = CallStackAnnotation CallStack
#endif
