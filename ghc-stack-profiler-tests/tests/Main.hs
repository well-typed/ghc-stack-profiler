{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty

import Control.Exception.Backtrace
import qualified Data.Binary as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Either (isRight)
import qualified Data.Machine as M
import Data.Maybe
import qualified Data.Text as T
import qualified GHC.Eventlog.Socket.Control as C
import GHC.Eventlog.Socket.Test (
  EventlogSocketAddr (EventlogSocketInetAddr, EventlogSocketUnixAddr),
  HasLogger,
  HasTestInfo,
  Program (Program, args, eventlogSocketBuildFlags, name, rtsopts),
  ProgramTest,
  allOf,
  anyOf,
  assertEventlogWith,
  assertEventlogWith',
  hasMatchingUserMarker,
  programTestFor,
  runProgramTests,
  sendCommand,
  withLogger,
  (!>),
  (&>), times,
 )
import qualified GHC.RTS.Events as E
import GHC.Stack.Profiler.Core.Eventlog (BinaryEventlogMessage)
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Temp (withTempDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Machine ((~>))

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
    [ test_oddball_HasCallStackMessage
    , test_oddball_ControlCallStackProfiling
    ]

-- |
-- Test that @oddball@ produces call-stack events.
test_oddball_HasCallStackMessage :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball_HasCallStackMessage =
  let
    oddball =
      Program
        { name = "oddball"
        , args = []
        , rtsopts = ["-l-au", "--eventlog-flush-interval=1"]
        , eventlogSocketBuildFlags = []
        }
  in
    programTestFor "test_oddball_HasCallStackMessage" oddball $ \eventlogSocket -> do
      assertEventlogWith eventlogSocket $
        hasCallStackEvent -- TODO: This needs to be delimited.

startCallStackProfiling :: C.Command
startCallStackProfiling = C.userCommand "ghc-stack-profiler" (C.CommandId 0x1)

stopCallStackProfiling :: C.Command
stopCallStackProfiling = C.userCommand "ghc-stack-profiler" (C.CommandId 0x2)

-- |
-- Test that the `StartCallStackProfiling` and `StopCallStackProfiling` commands
-- are respected, i.e., that once the `StopCallStackProfiling` command is sent,
-- after some iterations, no more call-stack events are received, and once the
-- `StartCallStackProfiling` command is sent, call-stack events resume.
test_oddball_ControlCallStackProfiling :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball_ControlCallStackProfiling =
  let
    oddball =
      Program
        { name = "oddball"
        , args = []
        , rtsopts = ["-l-au", "--eventlog-flush-interval=1"]
        , eventlogSocketBuildFlags = []
        }
  in
    programTestFor "test_oddball_ControlCallStackProfiling" oddball $ \eventlogSocket -> do
      assertEventlogWith' eventlogSocket $ \socket ->
        hasMatchingUserMarker ("Summing" `T.isPrefixOf`)
          &> hasCallStackEvent -- TODO: This needs to be delimited.
          &> sendCommand socket stopCallStackProfiling
          !> hasMatchingUserMarker (=="ghc-stack-profiler: Stop profiling")
          &> hasNoCallStackEvent
          ~> (2 `times` hasMatchingUserMarker ("Summing" `T.isPrefixOf`))
          &> sendCommand socket startCallStackProfiling
          !> hasMatchingUserMarker (=="ghc-stack-profiler: Start profiling")
          &> hasCallStackEvent -- TODO: This needs to be delimited.

-- | Assert that the input stream contains a binary call-stack event.
hasCallStackEvent :: (HasLogger, HasTestInfo) => M.ProcessT IO E.Event E.Event
hasCallStackEvent =
  anyOf isCallStackEvent onSuccess onFailure
 where
  onSuccess = printf "Found call-stack message after %d events."
  onFailure = printf "Did not find call-stack message after %d events."

-- | Assert that the input stream contains no binary call-stack events.
hasNoCallStackEvent :: (HasLogger, HasTestInfo) => M.ProcessT IO E.Event E.Event
hasNoCallStackEvent =
  allOf (not . isCallStackEvent) onSuccess onFailure
 where
  onSuccess = printf "Did not find call-stack event after %d events."
  onFailure = printf "Found call-stack event after %d events."

-- | Check if an event is a binary call-stack event.
isCallStackEvent :: E.Event -> Bool
isCallStackEvent ev =
  case E.evSpec ev of
    E.UserBinaryMessage msg -> isCallStackMessage msg
    _otherwise -> False

-- | Check if a `ByteString` is a binary call-stack message.
isCallStackMessage :: ByteString -> Bool
isCallStackMessage msg =
  isRight (B.decodeOrFail @BinaryEventlogMessage (BSL.fromStrict msg))

-- Validate that the startEventLogging hook fires.
-- hasMatchingUserMarker ("HookPostStartEventLogging" `T.isPrefixOf`)
-- -- Validate that the Finished marker is seen.
-- &> hasMatchingUserMarker ("Finished" `T.isPrefixOf`)
