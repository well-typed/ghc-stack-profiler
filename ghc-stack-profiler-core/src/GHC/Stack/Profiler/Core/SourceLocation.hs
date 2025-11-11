module GHC.Stack.Profiler.Core.SourceLocation where

import Data.Word (Word32)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A Haskell source location.
data SourceLocation =
  MkSourceLocation
    { line :: !Word32
    , column :: !Word32
    , functionName :: !Text
    , fileName :: !Text
    }
  deriving (Eq, Ord, Show, Generic)
