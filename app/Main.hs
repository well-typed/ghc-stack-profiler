{-# LANGUAGE LambdaCase        #-}

module Main where

import System.Environment
import System.Exit
import Data.Ord
import Data.Either (partitionEithers)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Function
import Text.Printf
import GHC.RTS.Events

import Control.Exception.Backtrace

import ThreadSample

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  [name] <- getArgs
  readEventLogFromFile name >>= \ case
    Left err -> do
      die $ "Error: " <> err
    Right eventlog -> do
      case partitionEithers $ getSamples eventlog of
        (errs@(_:_), _) -> do
          mapM_ print errs
          die $ "Error while eventlog decoding: \n" <> unlines (fmap show errs)
        ([], messages) -> do
          let
            samples =
              messages
                & summariseSamples
                & sortSamples

          printSamples (length messages) samples

  where
    summariseSamples = Map.fromListWith (+) . fmap (,1)

    sortSamples samples =
      List.sortOn (Down . snd) $ Map.assocs samples

    printSamples :: Int -> [(CallStackMessage, Int)] -> IO ()
    printSamples !num samples =
      mapM_ (\ (s, !n) -> printf " (%-3.2f %%) %s\n" (n `divide` num) (showSample s)) samples

    showSample :: CallStackMessage -> String
    showSample = show

    divide :: Int -> Int -> Double
    divide !x !n =
      fromIntegral x / fromIntegral n

getSamples :: EventLog -> [Either String CallStackMessage]
getSamples ev =
  mapMaybe (findCallStackMessage . evSpec) $ events $ dat ev

findCallStackMessage :: EventInfo -> Maybe (Either String CallStackMessage)
findCallStackMessage = \ case
  UserBinaryMessage bs ->
    Just $ deserializeCallStackMessage $ LBS.fromStrict bs
  _ ->
    Nothing
