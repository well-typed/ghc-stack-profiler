module GHC.Stack.Profiler.Speedscope.Types where

import Data.Text (Text)
import Data.Word (Word64)
import GHC.Stack.Profiler.Core.Eventlog (CapabilityId)
import GHC.Stack.Profiler.Core.SourceLocation (SourceLocation)

data InfoProv = InfoProv
  { infoProvId :: !InfoProvId
  , infoProvSrcLoc :: !Text
  , infoProvModule :: !Text
  , infoProvLabel :: !Text
  , infoTableName :: !Text
  , infoClosureDesc :: !Int
  , infoTyDesc :: !Text
  }
  deriving (Eq, Ord, Show)

data Sample = Sample
  { sampleThreadId :: !Word64
  -- ^ thread id
  , sampleCapabilityId :: !CapabilityId
  -- ^ Capability id
  , sampleCostCentreStack :: [CostCentreId]
  -- ^ stack ids
  }
  deriving (Eq, Ord, Show)

newtype CostCentreId = CostCentreId Word64
  deriving (Eq, Ord, Show)
  deriving newtype (Num)

newtype InfoProvId = InfoProvId Word64
  deriving (Eq, Ord, Show)

data UserCostCentre
  = CostCentreMessage !Text
  | CostCentreSrcLoc !SourceLocation
  | CostCentreIpe !InfoProv
  deriving (Eq, Ord, Show)
