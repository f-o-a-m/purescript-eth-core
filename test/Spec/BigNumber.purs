module CoreSpec.BigNumber (bigNumberSpec) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, fromSignedHexString, fromTwosComplement256, hexadecimal, parseBigNumber, toSignedHexString, toString, toTwosComplement256)
import Network.Ethereum.Core.BigNumber as Int
import Network.Ethereum.Core.HexString (HexString, unHex)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readImpl, writeImpl)
import Test.QuickCheck (class Arbitrary, quickCheck, (<?>), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

bigNumberSpec :: Spec Unit
bigNumberSpec = describe "BigNumber-spec" do
  describe "toBigNumber tests" do
    it "fails on invalid input" do
      map show (parseBigNumber decimal "!@#!@#@!#!@-----bla") `shouldEqual` Nothing
      map show (parseBigNumber decimal "-----bla") `shouldEqual` Nothing
      map show (parseBigNumber decimal "-----20") `shouldEqual` Nothing

    it "can handle turning strings into numbers" $ liftEffect do
      quickCheck \(x :: Int) -> (parseBigNumber decimal $ show x) === Just (embed x)
      map show (parseBigNumber hexadecimal "0x1") `shouldEqual` Just "1"
      map show (parseBigNumber hexadecimal "0x01") `shouldEqual` Just "1"
      map show (parseBigNumber hexadecimal "0xf") `shouldEqual` Just "15"
      map show (parseBigNumber hexadecimal "0x0f") `shouldEqual` Just "15"
      map show (parseBigNumber hexadecimal "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639935"
      map show (parseBigNumber hexadecimal "0x0") `shouldEqual` Just "0"

    it "can handle turning ints into big numbers" $ liftEffect $ do
      quickCheck \(x :: Int) -> embed x === (unsafeParseBN decimal $ show x)

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
          (embed x `div` embed y') == embed @BigNumber (x `div` y')
      quickCheck \(x :: Int) (y :: Int) ->
        let
          y' = if y == 0 then 1 else y
        in
          (embed x `mod` embed y') === embed @BigNumber (x `mod` y')

    it "can handle deserialization" $ liftEffect do
      quickCheck \(_bnString :: HexString) ->
        let
          bnString = "0x" <> unHex _bnString
          d1 = hush $ runExcept $ readImpl (unsafeToForeign bnString)
          (d2 :: Maybe BigNumber) = hush $ A.decodeJson (A.fromString bnString)
          d3 = parseBigNumber hexadecimal bnString
        in
          ( isJust d1
              && (d3 == d1)
              && (d3 == d2)
              && ((A.decodeJson (A.encodeJson d1)) == Right d3)
              && (runExcept (readImpl (writeImpl d1)) == Right d3)
          ) <?> ("Failed to deserialize " <> bnString <> " to a BigNumber")

    it "can go to and from hex string" $ liftEffect do
      quickCheck \(bn :: BigNumber) ->
        (fromSignedHexString =<< toSignedHexString bn) == Just bn <?> ("Failed to convert " <> show bn <> " to hex string " <> show (toString hexadecimal bn))

    it "can do twosComplement" $ liftEffect do
      quickCheck \(bn :: BigNumber) ->
        fromTwosComplement256 (toTwosComplement256 bn) === bn

newtype SmallInt = SmallInt Int

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> Gen.chooseInt 0 10000

unsafeParseBN
  :: Int.Radix
  -> String
  -> BigNumber
unsafeParseBN r x = unsafePartial $ fromJust $ parseBigNumber r x