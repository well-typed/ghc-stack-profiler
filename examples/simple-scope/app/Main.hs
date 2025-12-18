{-# OPTIONS -fno-omit-yields #-}
module Main where

import GHC.Stack.Annotation
import GHC.Stack.Profiler

main :: IO ()
main = setupRootStackProfiler True $ \ manager -> do
  withStackProfilerForMyThread manager (SampleIntervalMs 10) $ print $ annotateStackString "fib 41" $ fib 41
  print $ annotateStackString "fib 42" $ fib 42
  withStackProfilerForMyThread manager (SampleIntervalMs 10) $ print $ annotateStackString "fib 43" $ fib 43
  print $ annotateStackString "fib 40" $ fib 40

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
