module GHC.Stack.Profiler.Decode (
  CallStackSample (..),
  StackSymbolTable,
  SymbolTableWriter,
  initMessages,
  serializeCallStack,
  serializeMessage,
  serializeMessages,
  decodeToCallStack,
  definitions,
) where

import Control.Concurrent.STM
import Control.Exception (assert)
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics (Generic)
import GHC.Stack.CloneStack (StackSnapshot)
import GHC.Stack.Profiler.Core
import GHC.Stack.Profiler.Stack.Decode (decodeStackWithIpProvId)
import GHC.Stack.Profiler.SymbolTable

-- | A 'CallStackSample' is a snapshot of a threads RTS callstack.
-- This callstack is a copy of the original callstack, so can be traversed and
-- decoded without affecting the running thread.
--
-- The 'StackSnapshot' is a boxed value and needs to be garbage collected.
-- Note, as long as 'StackSnapshot' is alive, you keep the full callstack
-- alive, which might be quite expensive.
data CallStackSample = CallStackSample
  { callStackSampleThreadId :: !ThreadId
  , callStackSampleCapabilityId :: !CapabilityId
  , callStackSampleStackSnapshot :: !StackSnapshot
  }
  deriving (Generic)

decodeToCallStack :: CallStackSample -> IO CallStack
decodeToCallStack sample = do
  frames <- decodeStackWithIpProvId $ callStackSampleStackSnapshot sample
  let
    -- removes immediate duplicates
    callStackItems = fmap NonEmpty.head $ NonEmpty.group frames

  pure
    MkCallStack
      { callThreadId = callStackSampleThreadId sample
      , callCapabilityId = callStackSampleCapabilityId sample
      , callStack = callStackItems
      }

serializeCallStack :: StackSymbolTable -> CallStack -> STM [Message]
serializeCallStack tableRef callStackMessage = do
  table <- readSymbolTable tableRef
  let
    (eventlogMessages, newTable) = dehydrateCallStack table callStackMessage
  writeSymbolTable newTable tableRef
  pure eventlogMessages

serializeMessage :: Message -> LBS.ByteString
serializeMessage = runPut . put

serializeMessages :: [Message] -> [LBS.ByteString]
serializeMessages = map serializeMessage

initMessages :: SymbolTableWriter MapTable -> [LBS.ByteString]
initMessages symbolTable =
  let
    (stringDefs, srcLocDefs) = definitions symbolTable
    binaryEventlogMessages =
      ( map StringDef stringDefs
          ++ map SourceLocationDef srcLocDefs
      )
  in
    serializeMessages binaryEventlogMessages

definitions :: SymbolTableWriter MapTable -> ([StringDef], [SourceLocationDef])
definitions table =
  let
    knownStrings = getKnownStrings $ writerTable table
    knownSrcLocs = getKnownSourceLocations $ writerTable table

    stringDefs =
      fmap (uncurry MkStringDef) knownStrings

    srcLocDefs =
      map (uncurry go) knownSrcLocs
  in
    ( stringDefs
    , srcLocDefs
    )
 where
  go :: SourceLocationId -> SourceLocation -> SourceLocationDef
  go sid s =
    let
      (fileId, newFileName, _) = lookupOrInsertText table (writerTable table) (fileName s)
    in
      -- These should always be found
      assert (not newFileName) $
        MkSourceLocationDef
          { sourceLocationDefId = sid
          , sourceLocationDefRow = line s
          , sourceLocationDefColumn = column s
          , sourceLocationDefFilename = fileId
          }
