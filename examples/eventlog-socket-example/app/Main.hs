{-# OPTIONS -fno-omit-yields #-}
module Main where

import GHC.Stack.Annotation
import GHC.Stack.Profiler
import GHC.Eventlog.Socket (startWait)
import System.Environment (lookupEnv)
import System.Directory
import System.IO.Temp

main :: IO ()
main = setupRootStackProfiler False $ \ manager -> withStackProfiler manager (SampleIntervalMs 10) $ do
  mfp <- lookupEnv "EVENTLOG_SOCKET_EXAMPLE_SOCKET"
  fp <- maybe
    (do
      cacheDir <- getXdgDirectory XdgCache "eventlog-socket"
      createDirectoryIfMissing False cacheDir
      emptyTempFile cacheDir "socket"
    )
    pure
    mfp
  putStrLn $ "Waiting for connection at " ++ fp
  startWait fp

  -- Actual work is performed here
  print $ annotateStackString "fib 41" $ fib 41
  print $ annotateStackString "fib 42" $ fib 42
  print $ annotateStackString "fib 43" $ fib 43
  print $ annotateStackString "fib 40" $ fib 40

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
