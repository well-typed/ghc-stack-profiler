{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.ThreadSample
import GHC.Stack.Profiler.Core.Util (word32ToWord64)
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ properties
    ]

properties :: TestTree
properties =
  testGroup
    "property"
    [ testProperty "chunkCallStackMessage . catCallStackMessage" $
        withNumTests 500 $
          withMaxSize (fromIntegral callStackSizeLimit * 3) $
            chunkingRoundTrip_prop callStackSizeLimit
    , testProperty "chunkCallStackMessage size < chunkCallStackMessage" $
        withNumTests 500 $ do
          withMaxSize (fromIntegral callStackSizeLimit * 3) $
            messageChunkSize_prop eventlogBufferSize callStackSizeLimit
    , testProperty "chunkCallStackMessage_ n . catCallStackMessage" $
        withNumTests 500 $
          withEventlogSizeGen $ \eventlogSize ->
            withMaxSize (fromIntegral eventlogSize * 20) $
              chunkingRoundTrip_prop (callStackSizeLimit_ eventlogSize)
    , testProperty "chunkCallStackMessage_ n < size" $
        withNumTests 500 $ do
          withEventlogSizeGen $ \eventlogSize ->
            withMaxSize (fromIntegral eventlogSize * 5) $
              messageChunkSize_prop eventlogSize (callStackSizeLimit_ eventlogSize)
    ]
 where
  chunkingRoundTrip_prop stackSizeLimit message =
    let
      msgs = mapMaybe go (chunkCallStackMessage_ stackSizeLimit message)
      go = \case
        CallStackChunk csm -> Just csm
        CallStackFinal csm -> Just csm
        _ -> Nothing
    in
      catCallStackMessage (NonEmpty.fromList msgs) === message

  messageChunkSize_prop eventlogSize stackSizeLimit message =
    let
      msgs = mapMaybe go (chunkCallStackMessage_ stackSizeLimit message)
      go = \case
        CallStackChunk csm -> Just csm
        CallStackFinal csm -> Just csm
        _ -> Nothing
    in
      conjoin $
        map (eventlogMessageSmallerThanStackSizeLimit_prop (fromIntegral eventlogSize)) msgs

  eventlogMessageSmallerThanStackSizeLimit_prop :: Word -> BinaryCallStackMessage -> Property
  eventlogMessageSmallerThanStackSizeLimit_prop eventlogLimit chunk =
    let
      -- We need either 'CallStackFinal' or 'CallStackChunk' to add the leading '0xFFCA' or '0xFFCB'.
      -- This way, the eventlogLimit length is the correct thing to check.
      msg = runPut (put $ CallStackFinal chunk)
    in
      (LBS.length msg <= fromIntegral eventlogLimit) === True

withEventlogSizeGen :: (Word64 -> Property) -> Property
withEventlogSizeGen k =
  -- 29 bytes is minimum possible eventlog size.
  -- 12 bytes need to be subtracted for message overhead.
  -- The largest stack item is 17 bytes.
  forAll (choose (29, 1000)) k

instance Arbitrary BinaryCallStackMessage where
  arbitrary =
    MkBinaryCallStackMessage
      <$> (word32ToWord64 <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CapabilityId where
  arbitrary =
    MkCapabilityId <$> word32ToWord64 <$> arbitrary

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
