module GHC.Stack.Profiler.Decode (
  StackSymbolTable,
  SymbolTableWriter,
  initMessages,
  serializeCallStackMessage,
  serializeBinaryEventlogMessage,
  serializeBinaryEventlogMessages,
  threadSampleToCallStackMessage,
  binaryEventlogDefinitions,
) where

import Control.Concurrent.STM
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Conc.Sync (fromThreadId)

import Control.Exception (assert)
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Stack.Decode (decodeStackWithIpProvId)
import GHC.Stack.Profiler.SymbolTable

threadSampleToCallStackMessage :: ThreadSample -> IO CallStackMessage
threadSampleToCallStackMessage sample = do
  frames <- decodeStackWithIpProvId $ threadSampleStackSnapshot sample
  let
    -- removes immediate duplicates
    callStackItems = fmap NonEmpty.head $ NonEmpty.group frames

  pure
    MkCallStackMessage
      { callThreadId = fromThreadId $ threadSampleId sample
      , callCapabilityId = threadSampleCapability sample
      , callStack = callStackItems
      }

serializeCallStackMessage :: StackSymbolTable -> CallStackMessage -> STM [BinaryEventlogMessage]
serializeCallStackMessage tableRef callStackMessage = do
  table <- readSymbolTable tableRef
  let
    (eventlogMessages, newTable) = dehydrateCallStackMessage table callStackMessage
  writeSymbolTable newTable tableRef
  pure eventlogMessages

serializeBinaryEventlogMessage :: BinaryEventlogMessage -> LBS.ByteString
serializeBinaryEventlogMessage = runPut . put

serializeBinaryEventlogMessages :: [BinaryEventlogMessage] -> [LBS.ByteString]
serializeBinaryEventlogMessages = map serializeBinaryEventlogMessage

initMessages :: SymbolTableWriter MapTable -> [LBS.ByteString]
initMessages symbolTable =
  let
    (stringDefs, srcLocDefs) = binaryEventlogDefinitions symbolTable
    binaryEventlogMessages =
      ( map StringDef stringDefs
          ++ map SourceLocationDef srcLocDefs
      )
  in
    serializeBinaryEventlogMessages binaryEventlogMessages

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
