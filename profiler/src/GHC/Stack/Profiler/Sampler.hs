module GHC.Stack.Profiler.Sampler (
  -- * Run sample profiler
  withSampleProfiler,
  withSampleProfilerForMyThread,
  withSampleProfilerForThread,
  -- * Configuration of sample profiler
  SamplerProfilerConfig(..),
  -- * Basic thread sampler
  sampleThread,
  -- * Low level helpers for setting up custom
  -- sample profilers threads
  runWithSampleProfiler,
  setupSampleProfiler,
  tearDownSamplers,
  sampleToEventlog,
  -- * Thread filtering
  isProfilerThread,
  isRtsThread,
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import GHC.Conc
import GHC.Conc.Sync

import GHC.Stack.CloneStack (cloneThreadStack)

import qualified Debug.Trace.Binary.Compat as Compat
import GHC.Stack.Profiler.Decode
import GHC.Stack.Profiler.ThreadSample
import GHC.Stack.Profiler.Util

data SamplerProfilerConfig = MkSamplerProfilerConfig
  { samplerThreadId :: ThreadId
  } deriving (Show, Eq, Ord)

withSampleProfiler :: Int -> IO a -> IO a
withSampleProfiler delay act = do
  runWithSampleProfiler sampleAction delay act
  where
    sampleAction config = do
      tids <- listThreads
      userThreads <- filterM (isThreadOfInterest config) tids
      forM_ userThreads $ \tid ->
        sampleToEventlog tid

    isThreadOfInterest :: SamplerProfilerConfig -> ThreadId -> IO Bool
    isThreadOfInterest config tid = do
      lbl <- threadLabel tid
      pure $ not $ or
        [ isProfilerThread config tid lbl
        , isRtsThread tid lbl
        ]

withSampleProfilerForMyThread :: Int -> IO a -> IO a
withSampleProfilerForMyThread delay act = do
  tid <- myThreadId
  withSampleProfilerForThread tid delay act

withSampleProfilerForThread :: ThreadId -> Int -> IO a -> IO a
withSampleProfilerForThread tid delay act =
  runWithSampleProfiler (const (sampleToEventlog tid)) delay act

runWithSampleProfiler :: (SamplerProfilerConfig -> IO ()) -> Int -> IO a -> IO a
runWithSampleProfiler sampleAction delay act = do
  if Compat.userTracingEnabled
    then bracket (setupSampleProfiler sampleAction delay) tearDownSamplers (const act)
    else act

tearDownSamplers :: SamplerProfilerConfig -> IO ()
tearDownSamplers MkSamplerProfilerConfig{samplerThreadId} =
  killThread samplerThreadId

setupSampleProfiler :: (SamplerProfilerConfig -> IO ()) -> Int -> IO SamplerProfilerConfig
setupSampleProfiler sampleAction delay = do
  samplerThreadConfigMVar <- newEmptyMVar
  sid <- forkIO $ do
    config <- takeMVar samplerThreadConfigMVar
    sampleThreadId <- myThreadId
    labelThread sampleThreadId "Sample Profiler Thread"
    forever $ do
      sampleAction config
      -- TODO: this is wrong, we don't sample every delay time as sampling takes time as well
      threadDelay delay

  let sampleThreadConf = MkSamplerProfilerConfig
        { samplerThreadId = sid
        }
  putMVar samplerThreadConfigMVar sampleThreadConf
  pure sampleThreadConf

isProfilerThread :: SamplerProfilerConfig -> ThreadId -> Maybe String -> Bool
isProfilerThread MkSamplerProfilerConfig {samplerThreadId} tid _lbl = tid == samplerThreadId

isRtsThread :: ThreadId -> Maybe String -> Bool
isRtsThread _    Nothing    = False
isRtsThread _tid (Just lbl) =
  lbl `elem` ["TimerManager"] || "IOManager on cap" `List.isPrefixOf` lbl

sampleThread :: ThreadId -> IO (Maybe ThreadSample)
sampleThread tid = do
  tidStatus <- threadStatus tid
  (cap, _lockedToCap) <- threadCapability tid
  case tidStatus of
    ThreadRunning -> do
      stack <- cloneThreadStack tid
      pure $ Just $ ThreadSample
        { threadSampleId = tid
        , threadSampleCapability = CapabilityId $ intToWord64 cap
        , threadSampleStackSnapshot = stack
        }
    _ -> do
      -- Only running threads need to be sampled
      pure Nothing

sampleToEventlog :: ThreadId -> IO ()
sampleToEventlog tid = do
  sampleThread tid >>= \ case
    Nothing -> pure ()
    Just threadSample -> do
      msg <- serializeThreadSample threadSample
      Compat.traceBinaryEventIO $ LBS.toStrict msg
