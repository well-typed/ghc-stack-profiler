module GHC.Stack.Profiler.Decode (
  StackSymbolTable,
  SymbolTableWriter,
  serializeThreadSample,
  serializeBinaryEventlogMessage,
  serializeBinaryEventlogMessages,
  threadSampleToCallStackMessage,
  binaryEventlogDefinitions,
) where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Conc.Sync (fromThreadId)

import Control.Exception (assert)
import qualified Data.Tuple as Tuple
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Stack.Decode (decodeStackWithIpProvId)
import GHC.Stack.Profiler.SymbolTable

serializeThreadSample :: StackSymbolTable -> ThreadSample -> IO [LBS.ByteString]
serializeThreadSample tableRef sample = do
  eventlogMessages <- threadSampleToCallStackMessage tableRef sample
  pure $ serializeBinaryEventlogMessages eventlogMessages

threadSampleToCallStackMessage :: StackSymbolTable -> ThreadSample -> IO [BinaryEventlogMessage]
threadSampleToCallStackMessage tableRef sample = do
  frames <- decodeStackWithIpProvId $ threadSampleStackSnapshot sample
  -- removes immediate duplicates
  let
    callStackItems = fmap NonEmpty.head $ NonEmpty.group frames
  let
    callStackMessage =
      MkCallStackMessage
        { callThreadId = fromThreadId $ threadSampleId sample
        , callCapabilityId = threadSampleCapability sample
        , callStack = callStackItems
        }
  modifySymbolWriter tableRef $ \table -> do
    pure $ Tuple.swap $ dehydrateCallStackMessage table callStackMessage

serializeBinaryEventlogMessage :: BinaryEventlogMessage -> LBS.ByteString
serializeBinaryEventlogMessage = runPut . put

serializeBinaryEventlogMessages :: [BinaryEventlogMessage] -> [LBS.ByteString]
serializeBinaryEventlogMessages = map serializeBinaryEventlogMessage

binaryEventlogDefinitions :: SymbolTableWriter MapTable -> ([BinaryStringMessage], [BinarySourceLocationMessage])
binaryEventlogDefinitions table =
  let
    knownStrings = getKnownStrings $ writerTable table
    knownSrcLocs = getKnownSourceLocations $ writerTable table

    stringDefs =
      fmap (uncurry MkBinaryStringMessage) knownStrings

    srcLocDefs =
      map (uncurry go) knownSrcLocs
  in
    ( stringDefs
    , srcLocDefs
    )
 where
  go :: SourceLocationId -> SourceLocation -> BinarySourceLocationMessage
  go sid s =
    let
      (funcId, newFuncName, _) = lookupOrInsertText table (writerTable table) (functionName s)
      (fileId, newFileName, _) = lookupOrInsertText table (writerTable table) (fileName s)
    in
      -- These should always be found
      assert (not newFuncName) $
        assert (not newFileName) $
          MkBinarySourceLocationMessage
            { binarySourceLocationMessageId = sid
            , binarySourceLocationRow = line s
            , binarySourceLocationColumn = column s
            , binarySourceLocationFunctionId = funcId
            , binarySourceLocationFilename = fileId
            }
