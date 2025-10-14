{-# LANGUAGE LambdaCase #-}
module Sampler where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import GHC.Conc
import GHC.Conc.Sync

import Debug.Trace.Binary

import ThreadSample

data SamplerThread = MkSamplerThread
  { samplerThreadId :: ThreadId
  } deriving (Show, Eq, Ord)

withSampleProfiler :: Int -> IO a -> IO a
withSampleProfiler delay act = do
  bracket setupSamplers tearDownSamplers (const act)
  where
    setupSamplers = do
      samplerThreadConfigMVar <- newEmptyMVar
      sid <- forkIO $ do
        config <- takeMVar samplerThreadConfigMVar
        sampleThreadId <- myThreadId
        labelThread sampleThreadId "Sample Profiler Thread"
        forever $ do
          tids <- listThreads
          userThreads <- filterM (isThreadOfInterest config) tids
          forM_ userThreads $ \tid ->
            sampleToEventlog tid
          -- TODO: this is wrong, we don't sample every delay time as sampling takes time as well
          threadDelay delay

      let sampleThreadConf = MkSamplerThread
            { samplerThreadId = sid
            }
      putMVar samplerThreadConfigMVar sampleThreadConf
      pure sampleThreadConf

    tearDownSamplers MkSamplerThread{samplerThreadId} =
      killThread samplerThreadId

    isThreadOfInterest :: SamplerThread -> ThreadId -> IO Bool
    isThreadOfInterest config tid = do
      lbl <- threadLabel tid
      pure $ not $ or
        [ isProfilerThread config tid lbl
        , isBuiltinThread tid lbl
        ]

isProfilerThread :: SamplerThread -> ThreadId -> Maybe String -> Bool
isProfilerThread MkSamplerThread {samplerThreadId} tid lbl = tid == samplerThreadId

isBuiltinThread :: ThreadId -> Maybe String -> Bool
isBuiltinThread _    Nothing    = False
isBuiltinThread _tid (Just lbl) =
  lbl `elem` ["TimerManager"] || "IOManager on cap" `List.isPrefixOf` lbl


sampleToEventlog :: ThreadId -> IO ()
sampleToEventlog tid = do
  sampleThread tid >>= \ case
    Nothing -> pure ()
    Just threadSample -> do
      msg <- serializeThreadSample threadSample
      traceBinaryEventIO $ LBS.toStrict msg
