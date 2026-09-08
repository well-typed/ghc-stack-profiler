{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (myThreadId)
import Data.Maybe (isNothing)
import GHC.Stack.Profiler (Glob, startManager, startSamplerFor, stopManager, stopSampler, withManager)
import qualified GHC.Stack.Profiler as Glob (matches)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "Tests" $
      [ testGroup "Profiler" $
          [ bug_stopManagerTwice
          , test_stopSamplerTwice
          ]
      , testGroup "Glob" $
          [ runGlobTest globTest
          | globTest <- globTests
          ]
      ]

-------------------------------------------------------------------------------
-- Manager
-------------------------------------------------------------------------------

bug_stopManagerTwice :: TestTree
bug_stopManagerTwice = do
  testCase "stopManager twice deadlocks" $ do
    manager <- startManager False
    stopManager manager
    timedOut <-
      fmap isNothing . timeout 5_000_000 $ do
        stopManager manager
    assertBool "Test did not time out" timedOut

test_stopSamplerTwice :: TestTree
test_stopSamplerTwice = do
  testCase "stopSampler twice" $
    withManager False $ \manager -> do
      threadId <- myThreadId
      sampler <- startSamplerFor manager threadId 10
      stopSampler manager sampler
      timedOut <-
        fmap isNothing . timeout 5_000_000 $ do
          stopSampler manager sampler
      assertBool "Test timed out" (not timedOut)

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

globTests :: [GlobTest]
globTests =
  [ "*" `Matches` ""
  , "*" `Matches` "hello, world!"
  , "hell*" `Matches` "hello, world!"
  , "*!" `Matches` "hello, world!"
  , "hell*world!" `Matches` "hello, world!"
  , "henlo*" `NotMatches` "hello, world!"
  , "*worldy!" `NotMatches` "hello, world!"
  , "beach party" `NotMatches` "hello, world!"
  , "\\*" `NotMatches` ""
  , "\\*" `Matches` "*"
  , "hello\\*" `Matches` "hello*"
  , "\\*world" `Matches` "*world"
  , "hello\\*world" `Matches` "hello*world"
  , "\\**\\*" `Matches` "*helloworld*"
  ]

data GlobTest
  = Glob `Matches` String
  | Glob `NotMatches` String
  deriving (Show)

runGlobTest :: GlobTest -> TestTree
runGlobTest test =
  testCase (show test) $
    assertBool (show test) $
      case test of
        pat `Matches` str -> pat `Glob.matches` str
        pat `NotMatches` str -> not (pat `Glob.matches` str)
