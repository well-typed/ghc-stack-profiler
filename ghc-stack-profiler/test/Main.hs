{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Stack.Profiler (Glob)
import qualified GHC.Stack.Profiler as Glob (matches)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "Tests" $
      [ testGroup "Glob" $
          [ runGlobTest globTest
          | globTest <- globTests
          ]
      ]

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
