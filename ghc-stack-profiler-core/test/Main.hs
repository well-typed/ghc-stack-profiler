{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Binary (Binary, Word16, Word32, decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import GHC.Stack.Profiler.Core
import GHC.Stack.Profiler.Core.Internal
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options (IsOption (..))
import Test.Tasty.QuickCheck
import Text.Printf (printf)

main :: IO ()
main =
  defaultMain $
    adjustOption increaseDefaultMaxSize $
      testGroup "Tests" $
        [ testGroup "Size" $
            [ test_encodeSize "MessageTag" (Proxy @MessageTag) messageTagSize
            , test_encodeSize "CallStackFrameTag" (Proxy @CallStackFrameTag) callStackFrameTagSize
            , prop_encodeSizeInv "IpeId" (Proxy @IpeId) (== ipeIdSize)
            , prop_encodeSizeInv "ThreadId" (Proxy @ThreadId) (== threadIdSize)
            , prop_encodeSizeInv "CapabilityId" (Proxy @CapabilityId) (== capabilityIdSize)
            , prop_encodeSizeInv "CallStackFrame" (Proxy @CallStackFrame) (<= callStackFrameMaxSize)
            , prop_chunkCallStackSizeInv
            , let
                gen =
                  -- Generate a callStack, divide it into chunks, then pick one of the messages.
                  elements . chunkCallStack =<< arbitrary
              in
                prop_encodeSizeInv' "CallStackChunk/CallStackFinal" gen show Nothing (<= messageMaxSize)
            , prop_encodeSizeInv "StringId" (Proxy @StringId) (== stringIdSize)
            , let
                gen =
                  StringDef <$> arbitrary
                showFor msg@(StringDef MkStringDef{stringDefId, stringDefBody}) =
                  printf
                    "stringDefId == %d && length stringDefBody == %d && %s"
                    (getStringId stringDefId)
                    (T.length stringDefBody)
                    (labelFor msg)
                labelFor (StringDef MkStringDef{stringDefBody}) =
                  if fromIntegral (T.length stringDefBody) > stringDefBodyMaxSize
                    then "length stringDefBody >  stringDefBodyMaxSize"
                    else "length stringDefBody <= stringDefBodyMaxSize"
              in
                prop_encodeSizeInv' "StringDef" gen showFor (Just labelFor) (<= messageMaxSize)
            , prop_encodeSizeInv "SourceLocationId" (Proxy @SourceLocationId) (== sourceLocationIdSize)
            , prop_encodeSizeInv "SourceLocationDef" (Proxy @SourceLocationDef) (== sourceLocationDefSize)
            , prop_encodeSizeInv "ShortText" (Proxy @ShortText) (<= maxBoundWord16 + 2)
            , prop_truncateTextToByteLimit
            ]
        , testGroup "Encode/Decode" $
            [ test_encodeDecode "MessageTag" (Proxy @MessageTag)
            , test_encodeDecode "CallStackFrameTag" (Proxy @CallStackFrameTag)
            , prop_encodeDecode "IpeId" (Proxy @IpeId)
            , prop_encodeDecode "ThreadId" (Proxy @ThreadId)
            , prop_encodeDecode "CapabilityId" (Proxy @CapabilityId)
            , prop_encodeDecode "CallStackFrame" (Proxy @CallStackFrame)
            , prop_chunkAndJoinCallStack
            , let
                to :: CallStackChunk -> [Message]
                to = chunkCallStack
                from :: [Message] -> CallStackChunk
                from = joinCallStackChunks . NonEmpty.fromList . mapMaybe getCallStackFrame
              in
                prop_encodeDecodeVia' "CallStackChunk/CallStackFinal" arbitrary show Nothing to from
            , prop_encodeDecode "StringId" (Proxy @StringId)
            , let
                -- The roundtrip property only holds if the byte length of stringDefBody
                -- is less than stringDefBodyMaxSize, otherwise it's truncated.
                gen = do
                  MkStringDef{stringDefId, stringDefBody} <- arbitrary
                  let
                    stringDefBody' = truncateTextToByteLimit stringDefBodyMaxSize stringDefBody
                  pure $ StringDef MkStringDef{stringDefId, stringDefBody = stringDefBody'}
              in
                prop_encodeDecode' "StringDef" gen show Nothing
            , prop_encodeDecode "SourceLocationId" (Proxy @SourceLocationId)
            , prop_encodeDecode "SourceLocationDef" (Proxy @SourceLocationDef)
            , prop_encodeDecode "ShortText" (Proxy @ShortText)
            ]
        ]
 where
  increaseDefaultMaxSize :: QuickCheckMaxSize -> QuickCheckMaxSize
  increaseDefaultMaxSize v@(QuickCheckMaxSize _) =
    if v /= defaultValue then v else QuickCheckMaxSize (2 * maxBoundWord16)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Tests - Size Invariants

-- | Test a size invariant on the result of `encode` using `Enum`.
test_encodeSize :: (Binary a, Bounded a, Enum a, Eq a, Show a) => TestName -> Proxy a -> Int -> TestTree
test_encodeSize testName (_pa :: Proxy a) size =
  testGroup testName $
    [ testCase (printf "length (encode %s) == %d" (show a) size) $ do
        fromIntegral (BSL.length (encode a)) @?= size
    | (a :: a) <- [minBound .. maxBound]
    ]

-- | Test a size invariant on the result of `encode` using QuickCheck.
prop_encodeSizeInv :: (Arbitrary a, Binary a, Eq a, Show a) => TestName -> Proxy a -> (Int -> Bool) -> TestTree
prop_encodeSizeInv testName (_pa :: Proxy a) sizeInv =
  testProperty testName $ \(a :: a) ->
    sizeInv (fromIntegral (BSL.length (encode a)))

-- | Variant of `prop_encodeSizeInv` that accepts a custom generator, show function, and label function.
prop_encodeSizeInv' :: (Binary a, Eq a) => TestName -> Gen a -> (a -> String) -> Maybe (a -> String) -> (Int -> Bool) -> TestTree
prop_encodeSizeInv' testName gen showFor maybeLabelFor sizeInv =
  testProperty testName $
    forAllShow gen showFor $ \a ->
      maybe property (label . ($ a)) maybeLabelFor $
        sizeInv (fromIntegral (BSL.length (encode a)))

--------------------------------------------------------------------------------
-- Tests - Encode/Decode Roundtrips

-- | Test that an `encode`/`decode` roundtrip using a `Binary` instance is the identity using `Enum`.
test_encodeDecode :: (Binary a, Bounded a, Enum a, Eq a, Show a) => TestName -> Proxy a -> TestTree
test_encodeDecode testName (_pa :: Proxy a) =
  testGroup testName $
    [ testCase (printf "decode (encode %s) == %s" (show a) (show a)) $ do
        decode (encode a) @?= a
    | (a :: a) <- [minBound .. maxBound]
    ]

-- | Test that an `encode`/`decode` roundtrip using a `Binary` instance is the identity using QuickCheck.
prop_encodeDecode :: (Arbitrary a, Binary a, Eq a, Show a) => TestName -> Proxy a -> TestTree
prop_encodeDecode testName (_pa :: Proxy a) =
  testProperty testName $ \(a :: a) ->
    decode (encode a) == a

-- | Variant of `prop_encodeDecode` that accepts a custom generator, show function, and label function.
prop_encodeDecode' :: (Binary a, Eq a) => TestName -> Gen a -> (a -> String) -> Maybe (a -> String) -> TestTree
prop_encodeDecode' testName gen showFor maybeLabelFor =
  prop_encodeDecodeVia' testName gen showFor maybeLabelFor id id

-- | Variant of `prop_encodeDecode` that accepts conversions to/from a type with a `Binary` instance.
prop_encodeDecodeVia :: (Arbitrary a, Eq a, Show a, Binary b) => TestName -> (a -> b) -> (b -> a) -> TestTree
prop_encodeDecodeVia testName to from =
  prop_encodeDecodeVia' testName arbitrary show Nothing to from

-- | Variant of `prop_encodeDecodeVia'` that accepts conversions to/from a type with a `Binary` instance.
prop_encodeDecodeVia' :: (Eq a, Binary b) => TestName -> Gen a -> (a -> String) -> Maybe (a -> String) -> (a -> b) -> (b -> a) -> TestTree
prop_encodeDecodeVia' testName gen showFor maybeLabelFor to from =
  testProperty testName $
    forAllShow gen showFor $ \a ->
      maybe property (label . ($ a)) maybeLabelFor $
        from (decode (encode (to a))) == a

--------------------------------------------------------------------------------
-- Tests - Auxilliary

-- | Test that `chunkCallStack_` works as advertised.
prop_chunkCallStackSizeInv :: TestTree
prop_chunkCallStackSizeInv =
  testProperty "length (encode message) <= messageMaxSize | message <- chunkCallStack callStack" $ \callStack ->
    forAll (choose (messageMinSize, 2 * messageMaxSize)) $ \messageMaxSize' ->
      conjoin
        [ BSL.length (encode message) <= fromIntegral messageMaxSize'
        | message <- chunkCallStack_ (callStackMaxLen' messageMaxSize') callStack
        ]

-- | Test that `chunkCallStack_` and `joinCallStackChunks` are inverses.
prop_chunkAndJoinCallStack :: TestTree
prop_chunkAndJoinCallStack =
  testProperty "joinCallStackChunks (chunkCallStack_ n callStack) == callStack" $
    \(Positive n) callStack ->
      case NonEmpty.nonEmpty (mapMaybe getCallStackFrame (chunkCallStack_ n callStack)) of
        Nothing -> True
        Just callStackChunks -> joinCallStackChunks callStackChunks == callStack

-- | Test that `truncateTextToByteLimit` works as advertised.
prop_truncateTextToByteLimit :: TestTree
prop_truncateTextToByteLimit =
  testProperty "lengthWord8 (truncateTextToByteLimit byteLimit text) <= byteLimit" $
    \(NonNegative byteLimit) (UnicodeString (T.pack -> text)) ->
      label (labelFor byteLimit text) $
        let
          text' = truncateTextToByteLimit byteLimit text
        in
          if TF.lengthWord8 text <= byteLimit
            then
              -- If the original text fit within the byteLimit, the text should be unchanged.
              text == text'
            else
              -- Otherwise:
              and
                [ -- 1. The new text length should be within 3 byte of the byteLimit.
                  byteLimit - 3 <= TF.lengthWord8 text' && TF.lengthWord8 text' <= byteLimit
                , -- 2. The new text should be a prefix of the old text.
                  text' `T.isPrefixOf` text
                ]
 where
  labelFor :: Int -> Text -> String
  labelFor byteLimit text
    | TF.lengthWord8 text <= byteLimit = "lengthWord8 text <= byteLimit"
    | otherwise = "lengthWord8 text >  byteLimit"

--------------------------------------------------------------------------------
-- Helpers

-- | Get the `CallStackChunk` from a `Message`.
getCallStackFrame :: Message -> Maybe CallStackChunk
getCallStackFrame = \case
  CallStackFinal callStackChunk -> Just callStackChunk
  CallStackChunk callStackChunk -> Just callStackChunk
  _otherwise -> Nothing

maxBoundWord16 :: Int
maxBoundWord16 = fromIntegral (maxBound @Word16)

--------------------------------------------------------------------------------
-- Generators

instance Arbitrary IpeId where
  arbitrary :: Gen IpeId
  arbitrary = MkIpeId <$> arbitrary

instance Arbitrary ThreadId where
  arbitrary :: Gen ThreadId
  arbitrary = MkThreadId . fromIntegral <$> arbitrary @Word32

instance Arbitrary CapabilityId where
  arbitrary :: Gen CapabilityId
  arbitrary = MkCapabilityId . fromIntegral <$> arbitrary @Word32

instance Arbitrary CallStackFrame where
  arbitrary :: Gen CallStackFrame
  arbitrary =
    oneof
      [ CallStackFrameIpe <$> arbitrary
      , CallStackFrameAnn <$> arbitrary <*> arbitrary
      ]

instance Arbitrary CallStackChunk where
  arbitrary :: Gen CallStackChunk
  arbitrary = MkCallStackChunk <$> arbitrary <*> arbitrary <*> (NonEmpty.toList <$> arbitrary)

instance Arbitrary StringId where
  arbitrary :: Gen StringId
  arbitrary = MkStringId <$> arbitrary

instance Arbitrary StringDef where
  arbitrary :: Gen StringDef
  arbitrary = MkStringDef <$> arbitrary <*> arbitraryUnicodeText

instance Arbitrary SourceLocationId where
  arbitrary :: Gen SourceLocationId
  arbitrary = MkSourceLocationId <$> arbitrary

instance Arbitrary SourceLocationDef where
  arbitrary :: Gen SourceLocationDef
  arbitrary = MkSourceLocationDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ShortText where
  arbitrary :: Gen ShortText
  arbitrary = toShortText <$> arbitraryUnicodeText

arbitraryUnicodeText :: Gen Text
arbitraryUnicodeText = T.pack . getUnicodeString <$> arbitrary
