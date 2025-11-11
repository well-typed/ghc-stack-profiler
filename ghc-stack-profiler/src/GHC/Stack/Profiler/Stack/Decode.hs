{-# LANGUAGE MagicHash #-}
module GHC.Stack.Profiler.Stack.Decode (
  decodeStackWithIpProvId,
) where

import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Data.Typeable (cast)

import GHC.Stack.Annotation.Experimental.Compat
import GHC.Stack.CloneStack (StackSnapshot(..))
import GHC.Internal.ClosureTypes.Compat
import GHC.Internal.Stack.Constants.Compat
import GHC.Internal.Stack.Decode.Compat as Decode
import GHC.Internal.Stack.Types

import GHC.Exts.Heap.InfoTable.Types

import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.Util
import GHC.Stack.Profiler.Stack.Compat (lookupIpeIdForStackFrame)

decodeStackWithIpProvId :: StackSnapshot -> IO [StackItem]
decodeStackWithIpProvId (StackSnapshot stack#) = do
  info <- getInfoTableForStack stack#
  case tipe info of
    STACK -> do
      let sfls = stackFrameLocations stack#
      stack' <- stackFrameLocationItems sfls
      pure stack'
    _ -> error $ "Expected STACK closure, got " ++ show info
  where
    stackFrameLocations :: StackSnapshot# -> [StackFrameLocation]
    stackFrameLocations s# =
      stackHead s#
        : go (advanceStackFrameLocation (stackHead s#))
      where
        go :: Maybe StackFrameLocation -> [StackFrameLocation]
        go Nothing = []
        go (Just r) = r : go (advanceStackFrameLocation r)

stackFrameLocationItems :: [StackFrameLocation] -> IO [StackItem]
stackFrameLocationItems frames =
  catMaybes <$> traverse stackFrameLocationItem frames

stackFrameLocationItem :: StackFrameLocation -> IO (Maybe StackItem)
stackFrameLocationItem (StackSnapshot stack#, index) = do
  stackItbl <- getInfoTableOnStack stack# index
  case tipe (infoTable stackItbl) of
    ANN_FRAME ->
      let Box annotation = getClosureBox stack# (index + offsetStgAnnFrameAnn)
       in pure $ stackAnnotationToStackItem (unsafeCoerce annotation)
    _ ->
      fmap (IpeId . MkIpeId) <$> lookupIpeIdForStackFrame stackItbl

stackAnnotationToStackItem :: SomeStackAnnotation -> Maybe StackItem
stackAnnotationToStackItem = \ case
  SomeStackAnnotation ann ->
    case cast ann of
      Just (CallStackAnnotation cs) ->
        case getCallStack cs of
          [] -> Nothing
          ((name, sourceLoc):_) ->
            Just $ SourceLocation $ MkSourceLocation
              { line = intToWord32 $ srcLocStartLine sourceLoc
              , column = intToWord32 $ srcLocStartCol sourceLoc
              , functionName = Text.pack $ name
              , fileName = Text.pack $ srcLocFile sourceLoc
              }

      Nothing -> case cast ann of
        Just (StringAnnotation msg) ->
          Just $ UserMessage msg
        Nothing ->
          Nothing
