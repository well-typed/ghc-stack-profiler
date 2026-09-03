{-# LANGUAGE OverloadedStrings #-}

module GHC.Stack.Profiler.Core.Internal.CallStack (
  CallStack (..),
  StackItem (..),
  SourceLocation (..),
) where

import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics
import GHC.Stack.Profiler.Core.Internal.Eventlog

-- ----------------------------------------------------------------------------
-- Decoded RTS CallStack
-- ----------------------------------------------------------------------------

-- | A decoded rts callstack that can be serialised to the EventLog.
data CallStack = MkCallStack
  { callThreadId :: !ThreadId
  , callCapabilityId :: !CapabilityId
  , callStack :: [StackItem]
  }
  deriving (Eq, Ord, Show, Generic)

data StackItem
  = IpeId !IpeId
  | UserAnnotation !String !(Maybe SourceLocation)
  deriving (Eq, Ord, Show, Generic)

-- | A Haskell source location.
data SourceLocation = MkSourceLocation
  { line :: !Word32
  , column :: !Word32
  , fileName :: !Text
  }
  deriving (Eq, Ord, Show, Generic)
