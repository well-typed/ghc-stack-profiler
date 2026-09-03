module GHC.Stack.Profiler.Sampler (
  Interval (..),
  SamplerDescr (..),
  withSampler,
  startSampler,
  stopSampler,
) where

import Control.Concurrent (ThreadId, myThreadId, threadCapability, threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.Chan (writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Control.Monad.STM (atomically)
import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Foldable (for_)
import GHC.Conc (BlockReason (..), ThreadStatus (..), labelThread, threadStatus)
import GHC.Conc.Sync (fromThreadId)
import GHC.Internal.Control.Monad (forever)
import GHC.Stack.CloneStack (cloneThreadStack)
import qualified GHC.Stack.Profiler.Core as GSPC
import GHC.Stack.Profiler.Decode (
  CallStackSample (..),
  decodeToCallStack,
  serializeCallStack,
  serializeMessages,
 )
import GHC.Stack.Profiler.Manager (
  ControlMessage (..),
  Manager (..),
  Sampler (..),
  cancelSampler,
  registerSamplerThread,
  shouldProfile,
  unregisterSamplerThread,
 )

-- NOTE: Part of the public API.

-- | The sampling interval.
newtype Interval
  = MkIntervalMillis {intervalMillis :: Int}
  deriving stock (Eq, Show)

-- | @`fromInteger` n@ constructs an interval of @n@ milliseconds.
instance Num Interval where
  (+) :: Interval -> Interval -> Interval
  (+) = coerce @(Int -> Int -> Int) (+)

  (-) :: Interval -> Interval -> Interval
  (-) = coerce @(Int -> Int -> Int) (+)

  (*) :: Interval -> Interval -> Interval
  (*) = coerce @(Int -> Int -> Int) (*)

  abs :: Interval -> Interval
  abs = coerce @(Int -> Int) abs

  signum :: Interval -> Interval
  signum = coerce @(Int -> Int) signum

  fromInteger :: Integer -> Interval
  fromInteger = coerce @(Integer -> Int) fromInteger

-- | Get the interval in microseconds.
intervalMicros :: Interval -> Int
intervalMicros = (* 1_000) . intervalMillis
{-# INLINE intervalMicros #-}

-- | A description used to construct a `Sampler` thread.
data SamplerDescr = MkSamplerDescr
  { samplerManager :: Manager
  , samplerThreads :: IO [ThreadId]
  , sampleInterval :: !Interval
  }

withSampler :: SamplerDescr -> (Sampler -> IO a) -> IO a
withSampler sampler action =
  bracket
    (startSampler sampler)
    (stopSampler (samplerManager sampler))
    action

-- | Run a `SamplerDescr`.
startSampler :: SamplerDescr -> IO Sampler
startSampler sampler@MkSamplerDescr{samplerManager, sampleInterval} = do
  barrier <- newEmptyMVar
  samplerAsync <- async $ do
    () <- takeMVar barrier
    samplerThreadId <- myThreadId
    labelThread samplerThreadId $
      "Stack Sampler " <> show (fromThreadId samplerThreadId)
    forever $ do
      sampleThreads sampler
      -- TODO: Measure the delay at each step and subtract that from the next tick.
      threadDelay (intervalMicros sampleInterval)

  let
    samplerThread = MkSampler{samplerAsync}

  -- Register this sampler thread to avoid sampling it
  registerSamplerThread samplerManager samplerThread
  putMVar barrier ()
  pure samplerThread

-- | Stop a `Sampler` thread.
stopSampler :: Manager -> Sampler -> IO ()
stopSampler manager samplerThread = do
  cancelSampler samplerThread
    `finally` unregisterSamplerThread manager samplerThread

-- | Take one `CallStackSample` for every thread sampled by the `Sampler`.
sampleThreads :: SamplerDescr -> IO ()
sampleThreads MkSamplerDescr{samplerManager, samplerThreads} = do
  -- Wait until the manager signals to start profiling.
  atomically (STM.check =<< shouldProfile samplerManager)
  -- List all threads that should be sampled.
  threadIds <- samplerThreads
  -- Sample all threads.
  for_ threadIds $ \threadId ->
    sampleThread samplerManager threadId

-- | Take one `CallStackSample` for the given `ThreadId` and send it to the given `Manager`.
sampleThread :: Manager -> ThreadId -> IO ()
sampleThread manager threadId =
  sampleCallStackFor threadId
    >>= maybe (pure ()) (sendCallStackSample manager)

-- | Send a `CallStackSample` to the given `Manager`.
sendCallStackSample :: Manager -> CallStackSample -> IO ()
sendCallStackSample manager callStackSample = do
  callStack <- decodeToCallStack callStackSample
  binaryMessages <-
    atomically $ do
      -- TODO: Should these two STM calls be put in a single transaction?
      messages <- serializeCallStack (symbolTableRef manager) callStack
      STM.check =<< shouldProfile manager
      pure $! serializeMessages messages
  writeChan (messageChan manager) $!
    WriteProfileSample $
      BSL.toStrict <$> binaryMessages

-- | Take a `CallStackSample` for the given `ThreadId`.
sampleCallStackFor :: ThreadId -> IO (Maybe CallStackSample)
sampleCallStackFor threadId = do
  status <- threadStatus threadId
  (capNo, _lockedToCap) <- threadCapability threadId
  if canTakeCallStackSample status
    then do
      cloneThreadStack threadId >>= \stackSnapshot ->
        pure $
          Just $
            CallStackSample
              { callStackSampleThreadId = GSPC.MkThreadId . fromThreadId $ threadId
              , callStackSampleCapabilityId = GSPC.MkCapabilityId capNo
              , callStackSampleStackSnapshot = stackSnapshot
              }
    else pure Nothing

-- | Can a `CallStackSample` be taken for the given `ThreadId`?
canTakeCallStackSample :: ThreadStatus -> Bool
canTakeCallStackSample = \case
  ThreadRunning -> True
  ThreadBlocked BlockedOnMVar -> True
  _ -> False
{-# INLINE canTakeCallStackSample #-}
