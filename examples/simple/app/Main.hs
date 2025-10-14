{-# LANGUAGE NumericUnderscores #-}
module Main where

import GHC.Stack.Annotation.Experimental
import Sampler

main :: IO ()
main = withSampleProfiler 10_000 {- 10 ms -} $ do
  print $ annotateStackString "fib 41" $ fib 41
  print $ annotateStackString "fib 42" $ fib 42
  print $ annotateStackString "fib 43" $ fib 43
  print $ annotateStackString "fib 40" $ fib 40

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
