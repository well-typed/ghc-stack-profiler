module GHC.Stack.Profiler.Decode (
  SymbolTable,
  serializeThreadSample,
  threadSampleToCallStackMessage,
) where

import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Conc.Sync (fromThreadId)

import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Stack.Decode (decodeStackWithIpProvId)

type SymbolTable = SymbolTableWriter MapTable

serializeThreadSample :: TVar SymbolTable -> ThreadSample -> IO [LBS.ByteString]
serializeThreadSample tableRef sample = do
  eventlogMessages <- threadSampleToCallStackMessage tableRef sample
  pure $ map (runPut . put) eventlogMessages

threadSampleToCallStackMessage :: TVar SymbolTable -> ThreadSample -> IO [BinaryEventlogMessage]
threadSampleToCallStackMessage tableRef sample = do
  frames <- decodeStackWithIpProvId $ threadSampleStackSnapshot sample
  -- removes immediate duplicates
  let callStackItems = fmap NonEmpty.head $ NonEmpty.group frames
  let callStackMessage = MkCallStackMessage
        { callThreadId = fromThreadId $ threadSampleId sample
        , callCapabilityId = threadSampleCapability sample
        , callStack = callStackItems
        }
  -- TODO: Abstract TVar SymbolTable somehow
  STM.atomically $ do
    table <- TVar.readTVar tableRef
    let (messages, newTable) = dehydrateCallStackMessage table callStackMessage
    TVar.writeTVar tableRef newTable
    pure messages
