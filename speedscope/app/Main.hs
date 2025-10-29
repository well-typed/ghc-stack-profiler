{-# LANGUAGE LambdaCase        #-}

module Main where

import Control.Exception.Backtrace
import Speedscope

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  entry
