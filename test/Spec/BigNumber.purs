module CoreSpec.BigNumber (bigNumberSpec) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (unsafeToForeign)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, hexadecimal, parseBigNumber)
import Network.Ethereum.Core.BigNumber as Int
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

    it "can handle turning strings into numbers" do
      map show (parseBigNumber decimal "1") `shouldEqual` Just "1"
      map show (parseBigNumber hexadecimal "0x1") `shouldEqual` Just "1"
      map show (parseBigNumber hexadecimal "0x01") `shouldEqual` Just "1"
      map show (parseBigNumber decimal "15") `shouldEqual` Just "15"
      map show (parseBigNumber hexadecimal "0xf") `shouldEqual` Just "15"
      map show (parseBigNumber hexadecimal "0x0f") `shouldEqual` Just "15"
      map show (parseBigNumber decimal "-1") `shouldEqual` Just "-1"
      map show (parseBigNumber hexadecimal "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639935"
      map show (parseBigNumber hexadecimal "0") `shouldEqual` Just "0"
      map show (parseBigNumber hexadecimal "0x0") `shouldEqual` Just "0"

    it "can handle turning ints into big numbers" $ liftEffect $ do
      quickCheck \(x :: Int) -> embed x === (unsafeParseBN decimal $ show x)

    it "can handle roundedDiv" do
      div (embed @BigNumber 2) (embed 1) `shouldEqual` embed 2
      div (embed @BigNumber 2) (embed 2) `shouldEqual` embed 1
      div (embed @BigNumber 2) (embed 3) `shouldEqual` embed 0
      div (embed @BigNumber 100) (embed 10) `shouldEqual` embed 10
      div (embed @BigNumber 100) (embed 3) `shouldEqual` embed 33

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

    it "can handle deserialization" do
      let
        bnString = "f43"
        d1 = unsafePartial $ fromJust $ hush $ runExcept $ readImpl (unsafeToForeign bnString)
        d2 = unsafePartial $ fromJust $ hush $ A.decodeJson (A.fromString bnString)
        d3 = unsafePartial $ fromJust $ parseBigNumber hexadecimal bnString
      d3 `shouldEqual` d1
      d3 `shouldEqual` d2
      runExcept (readImpl (writeImpl d1)) `shouldEqual` Right d3
      (A.decodeJson (A.encodeJson d1)) `shouldEqual` Right d3

newtype SmallInt = SmallInt Int

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> Gen.chooseInt 0 10000

unsafeParseBN
  :: Int.Radix
  -> String
  -> BigNumber
unsafeParseBN r x = unsafePartial $ fromJust $ parseBigNumber r x