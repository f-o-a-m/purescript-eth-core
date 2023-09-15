module CoreSpec.BigNumber (bigNumberSpec) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Gen (chooseInt)
import Data.Argonaut as A
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Ord (abs)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Network.Ethereum.Core.BigNumber (BigNumber, embed, fromString, fromTwosComplement, toString, toTwosComplement, unsafeToInt)
import Network.Ethereum.Core.BigNumber as BigNumber
import Network.Ethereum.Core.HexString (mkHexString, numberOfBytes, unHex)
import Network.Ethereum.Core.HexString as Hex
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readImpl, writeImpl)
import Test.QuickCheck (class Arbitrary, quickCheck, quickCheckGen, (<?>), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data

bigNumberSpec :: Spec Unit
bigNumberSpec = describe "BigNumber-spec" do
  describe "toBigNumber tests" do
    it "fails on invalid input" do
      map show (fromString "!@#!@#@!#!@-----bla") `shouldEqual` Nothing
      map show (fromString "-----bla") `shouldEqual` Nothing
      map show (fromString "-----20") `shouldEqual` Nothing

    it "can handle turning strings into numbers" $ liftEffect do
      quickCheckGen $ do
        bn <- BigNumber.generator
        pure $ (fromString $ toString bn) == Just bn <?> ("Failed to convert " <> show bn <> " to hex string " <> show (toString bn))

    it "can handle turning ints into big numbers" $ liftEffect $ do
      quickCheck \(x :: Int) -> unsafeToInt (embed x) === x

  describe "BigNumber arithmetic" do
    it "can add, subtract, and multiply BigNumbers as an Int-Alegbra" $ liftEffect do
      quickCheck \(x :: Int) (y :: Int) -> (embed x + embed y) === embed @BigNumber (x + y)
      quickCheck \(x :: Int) (y :: Int) -> (embed x - embed y) === embed @BigNumber (x - y)
      quickCheck \(SmallInt x) (SmallInt y) -> (embed x * embed y) === embed @BigNumber (x * y)

    it "works like a Ring" $ liftEffect $ checkLaws "BigNumber" $ do
      Data.checkEqGen BigNumber.generator
      Data.checkOrdGen BigNumber.generator
      Data.checkCommutativeRingGen BigNumber.generator
      Data.checkSemiringGen BigNumber.generator
      -- Necessary for the EuclideanRing test
      -- so as to prevent integer overflow when multiplying large integer values
      Data.checkEuclideanRingGen BigNumber.generator
      Data.checkRingGen BigNumber.generator

    it "can handle deserialization" $ liftEffect do
      quickCheckGen $ do
        n <- chooseInt 1 100
        _bnString <- Hex.generator n
        let
          bnString = if (_bnString == mempty) then "0" else unHex _bnString
          (d1 :: Maybe BigNumber) = hush $ runExcept $ readImpl (unsafeToForeign bnString)
          (d2 :: Maybe BigNumber) = hush $ A.decodeJson (A.fromString bnString)
          d3 = fromString bnString
        pure $
          ( isJust d1
              && (d3 == d1)
              && (d3 == d2)
              && ((A.decodeJson (A.encodeJson d1)) == Right d3)
              && (runExcept (readImpl (writeImpl d1)) == Right d3)
          ) <?> ("Failed to deserialize " <> bnString <> " to a BigNumber")

    it "can do twosComplement" $ liftEffect do
      quickCheckGen $ do
        bn <- BigNumber.generator
        let nBits = 8 * numberOfBytes (unsafePartial fromJust $ mkHexString (toString $ abs bn)) + 1
        pure $ (fromTwosComplement nBits (toTwosComplement nBits bn)) === bn

newtype SmallInt = SmallInt Int

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> Gen.chooseInt 0 10000