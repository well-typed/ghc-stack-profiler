module GHC.Stack.Profiler.Decode (
  SymbolTable,
  serializeThreadSample,
  threadSampleToCallStackMessage,
) where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.List.NonEmpty as NonEmpty
import Data.Tuple

import GHC.Internal.Conc.Sync

import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Stack.Decode (decodeStackWithIpProvId)

type SymbolTable = SymbolTableWriter MapTable

serializeThreadSample :: IORef SymbolTable -> ThreadSample -> IO [LBS.ByteString]
serializeThreadSample tableRef sample = do
  eventlogMessages <- threadSampleToCallStackMessage tableRef sample
  pure $ map (runPut . put) eventlogMessages

threadSampleToCallStackMessage :: IORef SymbolTable -> ThreadSample -> IO [BinaryEventlogMessage]
threadSampleToCallStackMessage tableRef sample = do
  frames <- decodeStackWithIpProvId $ threadSampleStackSnapshot sample
  -- removes immediate duplicates
  let callStackItems = fmap NonEmpty.head $ NonEmpty.group frames
  let callStackMessage = MkCallStackMessage
        { callThreadId = fromThreadId $ threadSampleId sample
        , callCapabilityId = threadSampleCapability sample
        , callStack = callStackItems
        }
  -- TODO: Abstract IORef SymbolTable somehow
  atomicModifyIORef' tableRef $ \ table ->
    swap $ dehydrateCallStackMessage table callStackMessage
