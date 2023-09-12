module CoreSpec.BigNumber (bigNumberSpec) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), isJust)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Network.Ethereum.Core.BigNumber (BigNumber, embed, fromString, fromTwosComplement256, toString, toTwosComplement256, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, unHex)
import Simple.JSON (readImpl, writeImpl)
import Test.QuickCheck (class Arbitrary, quickCheck, (<?>), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

bigNumberSpec :: Spec Unit
bigNumberSpec = describe "BigNumber-spec" do
  describe "toBigNumber tests" do
    it "fails on invalid input" do
      map show (fromString "!@#!@#@!#!@-----bla") `shouldEqual` Nothing
      map show (fromString "-----bla") `shouldEqual` Nothing
      map show (fromString "-----20") `shouldEqual` Nothing

    it "can handle turning strings into numbers" $ liftEffect do
      quickCheck \(bn :: BigNumber) ->
        (fromString $ toString bn) == Just bn <?> ("Failed to convert " <> show bn <> " to hex string " <> show (toString bn))

    it "can handle turning ints into big numbers" $ liftEffect $ do
      quickCheck \(x :: Int) -> unsafeToInt (embed x) === x

  describe "BigNumber arithmetic" do
    it "can add, subtract, and multiply BigNumbers as an Int-Alegbra" $ liftEffect do
      quickCheck \(x :: Int) (y :: Int) -> (embed x + embed y) === embed @BigNumber (x + y)
      quickCheck \(x :: Int) (y :: Int) -> (embed x - embed y) === embed @BigNumber (x - y)
      quickCheck \(SmallInt x) (SmallInt y) -> (embed x * embed y) === embed @BigNumber (x * y)

    it "works like a Euclidian Ring" $ liftEffect $ do
      quickCheck \(x :: Int) (y :: Int) ->
        let
          y' = if y == 0 then 1 else y
        in
          (embed x `div` embed y') == embed @BigNumber (x `div` y') &&
            (embed x `mod` embed y') == embed @BigNumber (x `mod` y')

    it "can handle deserialization" $ liftEffect do
      quickCheck \(_bnString :: HexString) ->
        let
          bnString = if (_bnString == mempty) then "0" else unHex _bnString
          (d1 :: Maybe BigNumber) = hush $ runExcept $ readImpl (unsafeToForeign bnString)
          (d2 :: Maybe BigNumber) = hush $ A.decodeJson (A.fromString bnString)
          d3 = fromString bnString
        in
          ( isJust d1
              && (d3 == d1)
              && (d3 == d2)
              && ((A.decodeJson (A.encodeJson d1)) == Right d3)
              && (runExcept (readImpl (writeImpl d1)) == Right d3)
          ) <?> ("Failed to deserialize " <> bnString <> " to a BigNumber")

    it "can do twosComplement" $ liftEffect do
      quickCheck \(bn :: BigNumber) ->
        fromTwosComplement256 (toTwosComplement256 bn) === bn

newtype SmallInt = SmallInt Int

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> Gen.chooseInt 0 10000