{-# OPTIONS -fno-omit-yields #-}
module Main where

import GHC.Stack.Annotation
import GHC.Stack.Profiler
import GHC.Eventlog.Socket
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Debug.Trace (traceMarkerIO, flushEventLog)

main :: IO ()
main = withRootStackProfiler False $ \ manager -> withStackProfiler manager (SampleIntervalMs 10) $ do
  startFromEnv

  -- Actual work is performed here
  numArgs <- getArgs

  let
    nums = case numArgs of
      [] -> [40]
      xs -> fmap read xs


  traverse_ runFib nums
  where
    runFib n = do
      traceMarkerIO $ "Starting fib " <> show n
      putStrLn $ "Fib result of: " <> show n <> " is " <> show (annotateStackString ("fib " ++ show n) $ fib n)
      traceMarkerIO $ "Finished fib " <> show n
      flushEventLog

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
