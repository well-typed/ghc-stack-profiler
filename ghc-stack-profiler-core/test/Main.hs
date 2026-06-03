{-# OPTIONS_GHC -Wno-orphans #-}
module Main where
import Test.Tasty
import Test.Tasty.QuickCheck
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.ThreadSample
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ properties
  ]

properties :: TestTree
properties =
  testGroup "property"
    [ testProperty "chunkCallStackMessage" $
        \ message ->
          let
            msgs = mapMaybe go (chunkCallStackMessage message)
            go = \ case
              CallStackChunk csm -> Just csm
              CallStackFinal csm -> Just csm
              _ -> Nothing
          in
            catCallStackMessage (NonEmpty.fromList msgs) === message
    ]

instance Arbitrary BinaryCallStackMessage where
  arbitrary =
    MkBinaryCallStackMessage
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CapabilityId where
  arbitrary =
    MkCapabilityId <$> arbitrary

instance Arbitrary BinaryStackItem where
  arbitrary =
    oneof
      [ BinaryIpe <$> arbitrary
      , BinaryMessage <$> arbitrary <*> arbitrary
      ]

instance Arbitrary IpeId where
  arbitrary =
    MkIpeId <$> arbitrary

instance Arbitrary StringId where
  arbitrary =
    MkStringId <$> arbitrary

instance Arbitrary SourceLocationId where
  arbitrary =
    MkSourceLocationId <$> arbitrary
