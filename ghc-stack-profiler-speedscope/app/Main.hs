module Main where

import Control.Exception.Backtrace
import GHC.Stack.Profiler.Speedscope

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  entry
