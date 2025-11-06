{-# LANGUAGE MagicHash #-}
module GHC.Stack.Profiler.Decode (
  SymbolTable,
  serializeThreadSample,
  threadSampleToCallStackMessage,
) where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as Text
import Data.Typeable (cast)
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import Unsafe.Coerce
import Data.Tuple

import GHC.Stack.Annotation.Experimental
import GHC.Stack.CloneStack (StackSnapshot(..))
import GHC.Internal.Conc.Sync
import GHC.Internal.InfoProv.Types (lookupIpProvId)
import GHC.Internal.Heap.Closures
import qualified GHC.Internal.Stack.Decode as Decode
import GHC.Internal.Stack.Types

import GHC.Stack.Profiler.SymbolTable
import GHC.Stack.Profiler.ThreadSample
import GHC.Stack.Profiler.Eventlog
import GHC.Stack.Profiler.Util

type SymbolTable = SymbolTableWriter MapTable

serializeThreadSample :: IORef SymbolTable -> ThreadSample -> IO [LBS.ByteString]
serializeThreadSample tableRef sample = do
  eventlogMessages <- threadSampleToCallStackMessage tableRef sample
  pure $ map (runPut . put) eventlogMessages

threadSampleToCallStackMessage :: IORef SymbolTable -> ThreadSample -> IO [BinaryEventlogMessage]
threadSampleToCallStackMessage tableRef sample = do
  frames <- decodeStackWithIpProvId $ threadSampleStackSnapshot sample
  let callStackItems = fmap NonEmpty.head . NonEmpty.group $ mapMaybe (uncurry stackFrameToStackItem) frames
  let callStackMessage = MkCallStackMessage
        { callThreadId = fromThreadId $ threadSampleId sample
        , callCapabilityId = threadSampleCapability sample
        , callStack = callStackItems
        }
  -- TODO: Abstract IORef SymbolTable somehow
  atomicModifyIORef' tableRef $ \ table ->
    swap $ dehydrateCallStackMessage table callStackMessage

decodeStackWithIpProvId :: StackSnapshot -> IO [(StackFrame, Maybe Word64)]
decodeStackWithIpProvId snapshot = do
  snd <$> Decode.decodeStackWithFrameUnpack unpackStackFrameWithIpProvId snapshot

unpackStackFrameWithIpProvId :: Decode.StackFrameLocation -> IO (StackFrame, Maybe Word64)
unpackStackFrameWithIpProvId stackFrameLoc = do
  Decode.unpackStackFrameTo stackFrameLoc
    (\ info infoKey _nextChunk@(StackSnapshot stack#) -> do
      -- TODO: We don't want to decode the stack frames at all,
      -- we only need to decode the 'AnnFrame' stack frame.
      -- framesWithIpe <- decodeStackWithIpProvId nextChunk
      mIpeId <- lookupIpProvId infoKey
      pure
        ( UnderflowFrame
          { info_tbl = info,
            nextChunk =
              GenStgStackClosure
                { ssc_info = info,
                  ssc_stack_size = Decode.getStackFields stack#,
                  ssc_stack = [] -- map fst framesWithIpe
                }
          }
        , mIpeId
        )

    )
    (\ frame infoKey -> do
      mIpeId <- lookupIpProvId infoKey
      pure (frame, mIpeId))

stackFrameToStackItem :: StackFrame -> Maybe Word64 -> Maybe StackItem
stackFrameToStackItem frame mIpe =
  case frame of
    AnnFrame { annotation = Box someStackAnno } ->
      case unsafeCoerce someStackAnno of
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
    _ ->
      IpeId . MkIpeId <$> mIpe
