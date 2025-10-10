module Sampler where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy as LBS

import Debug.Trace.Binary

import ThreadSample

sampleMyThread :: Int -> IO ThreadId
sampleMyThread delay = do
  tid <- myThreadId
  sampler <- forkIO $ forever $ do
    sampleToEventlog tid
    -- TODO: this is wrong, we don't sample every delay time as sampling takes time as well
    threadDelay delay
  pure sampler

sampleToEventlog :: ThreadId -> IO ()
sampleToEventlog tid = do
  threadSample <- sampleThread tid
  msg <- serializeThreadSample threadSample
  traceBinaryEventIO $ LBS.toStrict msg
