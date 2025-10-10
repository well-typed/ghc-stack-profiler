{-# LANGUAGE NumericUnderscores #-}
module Main where

import Control.Concurrent
import GHC.Stack.Annotation.Experimental
import Sampler

main :: IO ()
main = do
  sid <- sampleMyThread 10_000 -- sample 10 milliseconds
  print sid
  print =<< myThreadId
  print $ annotateStackString "fib 41" $ fib 41
  print $ annotateStackString "fib 42" $ fib 42
  print $ annotateStackString "fib 43" $ fib 43
  print $ annotateStackString "fib 40" $ fib 40
  killThread sid

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
