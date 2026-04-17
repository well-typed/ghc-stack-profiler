{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty

import Data.Maybe
import qualified Data.Text as T
import GHC.Eventlog.Socket.Test
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Temp (withTempDirectory)
import Text.Read (readMaybe)
import Control.Exception.Backtrace

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  -- Allow the user to overwrite the TCP port:
  tcpPort <- (fromMaybe "4242" . (readMaybe =<<)) <$> lookupEnv "GHC_EVENTLOG_INET_PORT"

  -- Create logger:
  withLogger $ do
    -- Create temporary directory:
    withTempDirectory "/tmp" "eventlog-socket" $ \tmpDir -> do
      -- Base socket addresses
      let
        unixTests = tests <*> pure (EventlogSocketUnixAddr $ tmpDir </> "ghc_eventlog.sock")
      let
        inetTests = tests <*> pure (EventlogSocketInetAddr "127.0.0.1" tcpPort)
      defaultMain . testGroup "Tests" . runProgramTests $ unixTests <> inetTests
 where
  tests :: (HasLogger) => [EventlogSocketAddr -> ProgramTest]
  tests =
    [ test_oddball
    , test_JumpyJump
    ]

-- |
-- Test that @jumpy-jump@ produces a parseable eventlog.
test_JumpyJump :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_JumpyJump =
  let
    fibber =
      Program
        { name = "jumpy-jump"
        , args = []
        , rtsopts = ["-l-au"]
        , eventlogSocketBuildFlags = []
        }
  in
    programTestFor "test_jumpyJump" fibber $ \eventlogSocket -> do
      assertEventlogWith eventlogSocket $
        hasWallClockTime
        -- Validate that the startEventLogging hook fires.
        -- hasMatchingUserMarker ("HookPostStartEventLogging" `T.isPrefixOf`)
          -- -- Validate that the Finished marker is seen.
          -- &> hasMatchingUserMarker ("Finished" `T.isPrefixOf`)

-- |
-- Test that @jumpy-jump@ produces a parseable eventlog.
test_oddball :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball =
  let
    fibber =
      Program
        { name = "oddball"
        , args = []
        , rtsopts = ["-l-au"]
        , eventlogSocketBuildFlags = []
        }
  in
    programTestFor "test_oddball" fibber $ \eventlogSocket -> do
      assertEventlogWith eventlogSocket $
        hasWallClockTime
        -- Validate that the startEventLogging hook fires.
        -- hasMatchingUserMarker ("HookPostStartEventLogging" `T.isPrefixOf`)
          -- -- Validate that the Finished marker is seen.
          -- &> hasMatchingUserMarker ("Finished" `T.isPrefixOf`)
