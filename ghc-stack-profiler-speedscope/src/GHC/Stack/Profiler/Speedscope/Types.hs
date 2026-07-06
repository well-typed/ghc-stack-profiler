module GHC.Stack.Profiler.Speedscope.Types where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified GHC.Stack.Profiler.Core.Eventlog as GSP (CapabilityId, IpeId (..))
import qualified GHC.Stack.Profiler.Core.SourceLocation as GSP (SourceLocation (..))
import qualified GHC.Stack.Profiler.Core.Util as GSP
import IpeDB.Types.InfoProv (InfoProv (..), InfoProvId (..))
import IpeDB.Types.SrcLoc (Point (..), Range (..), SrcLoc (..))

data Sample = Sample
  { sampleThreadId :: !Word64
  -- ^ Thread ID.
  , sampleCapabilityId :: !GSP.CapabilityId
  -- ^ Capability ID.
  , sampleStack :: [StackFrameId]
  -- ^ Stack IDs.
  }
  deriving (Eq, Show)

newtype StackFrameId = StackFrameId Word64
  deriving (Eq, Ord, Show)
  deriving newtype (Num)

data StackFrame
  = StackFrameMessage !Text !SrcLoc
  | StackFrameInfoProv !InfoProv
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Conversions from ghc-stack-profiler-core types to ipedb types

toInfoProvId :: GSP.IpeId -> InfoProvId
toInfoProvId (GSP.MkIpeId x) = InfoProvId x

toSrcLoc :: Maybe GSP.SourceLocation -> SrcLoc
toSrcLoc = \case
  Nothing ->
    UnhelpfulSrcLoc
  Just GSP.MkSourceLocation{fileName, column, line} ->
    SrcLoc (Text.unpack fileName) (Just $! Range'Point column line)

srcLocFile :: SrcLoc -> Maybe Text
srcLocFile SrcLoc{srcFilePath}
  | null srcFilePath = Nothing
  | otherwise = Just (Text.pack srcFilePath)

srcLocColumn :: SrcLoc -> Maybe Int
srcLocColumn SrcLoc{srcRange = Just Range{start = Point{column}}} = Just (GSP.word32ToInt column)
srcLocColumn _ = Nothing

srcLocLine :: SrcLoc -> Maybe Int
srcLocLine SrcLoc{srcRange = Just Range{start = Point{line}}} = Just (GSP.word32ToInt line)
srcLocLine _ = Nothing
