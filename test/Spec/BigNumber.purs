module CoreSpec.BigNumber (bigNumberSpec) where


import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.Either (Either(..), hush)
import Foreign (unsafeToForeign)
import Foreign.Class (decode, encode)
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, hexadecimal, parseBigNumber, divide)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readImpl)
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
       map show (parseBigNumber decimal "1")  `shouldEqual` Just  "1"
       map show (parseBigNumber hexadecimal "0x1") `shouldEqual` Just "1"
       map show (parseBigNumber hexadecimal "0x01") `shouldEqual` Just "1"
       map show (parseBigNumber decimal "15") `shouldEqual` Just "15"
       map show (parseBigNumber hexadecimal "0xf") `shouldEqual` Just "15"
       map show (parseBigNumber hexadecimal "0x0f") `shouldEqual` Just "15"
       map show (parseBigNumber decimal "-1") `shouldEqual` Just "-1"
       map show (parseBigNumber hexadecimal "-0x1") `shouldEqual` Just "-1"
       map show (parseBigNumber hexadecimal "-0x01") `shouldEqual` Just "-1"
       map show (parseBigNumber decimal "-15") `shouldEqual` Just "-15"
       map show (parseBigNumber hexadecimal "-0xf") `shouldEqual` Just "-15"
       map show (parseBigNumber hexadecimal "-0x0f") `shouldEqual` Just "-15"
       map show (parseBigNumber hexadecimal "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639935"
       map show (parseBigNumber hexadecimal "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639933"
       map show (parseBigNumber hexadecimal "-0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "-115792089237316195423570985008687907853269984665640564039457584007913129639935"
       map show (parseBigNumber hexadecimal "-0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` Just "-115792089237316195423570985008687907853269984665640564039457584007913129639933"
       map show (parseBigNumber hexadecimal "0") `shouldEqual` Just "0"
       map show (parseBigNumber hexadecimal "0x0") `shouldEqual` Just "0"
       map show (parseBigNumber hexadecimal "-0") `shouldEqual` Just "0"
       map show (parseBigNumber hexadecimal "-0x0") `shouldEqual` Just "0"

      it "can handle turning ints into big numbers" do
        Just (one :: BigNumber) `shouldEqual` (parseBigNumber decimal "1")
        Just (zero :: BigNumber) `shouldEqual` (parseBigNumber decimal "0")
        Just (embed 0 :: BigNumber) `shouldEqual` (parseBigNumber decimal "0")
        Just (embed 15 :: BigNumber) `shouldEqual` (parseBigNumber decimal "15")

      it "can handle roundedDiv" do
        divide (embed 2) (embed 1) `shouldEqual` embed 2
        divide (embed 2) (embed 2) `shouldEqual` embed 1
        divide (embed 2) (embed 3) `shouldEqual` embed 0
        divide (embed 100) (embed 10) `shouldEqual` embed 10
        divide (embed 100) (embed 3) `shouldEqual` embed 33

    describe "BigNumber arithmetic" do
      it "can add, subtract, and multiply BigNumbers as an Int-Alegbra" do
        (add <$> (parseBigNumber decimal "1") <*> (parseBigNumber decimal "1")) `shouldEqual` (parseBigNumber decimal "2")
        (sub <$> (parseBigNumber hexadecimal "0xf") <*> (parseBigNumber decimal "1")) `shouldEqual` (parseBigNumber decimal "14")
        ((parseBigNumber hexadecimal "0xf") >>= \x -> pure $ embed 15 + x) `shouldEqual` parseBigNumber decimal "30"
        ((parseBigNumber decimal "21") >>= \x -> pure $ x - zero) `shouldEqual` parseBigNumber hexadecimal "0x15"
        (Just $ one `mul` one) `shouldEqual` parseBigNumber decimal "1"
        (Just $ one * embed (-7)) `shouldEqual` parseBigNumber hexadecimal "-0x7"

      it "works like a Euclidian Ring" do
        ((embed 20) / (embed 5) :: BigNumber) `shouldEqual` (embed 4)
        ((embed 22) `mod` (embed 7) :: BigNumber) `shouldEqual` (embed 1)

      it "can handle deserialization" do
        let bnString = "f43"
            d1 = unsafePartial $ fromJust $ hush $ runExcept $ readImpl (unsafeToForeign bnString)
            d2 = unsafePartial $ fromJust $ hush $ runExcept $ decode (unsafeToForeign bnString)
            d3 = unsafePartial $ fromJust $ hush $ A.decodeJson (A.fromString bnString)
            d4 = unsafePartial $ fromJust $ parseBigNumber hexadecimal bnString
        d4 `shouldEqual` d1
        d4 `shouldEqual` d2
        d4 `shouldEqual` d3
        runExcept (decode (encode d1)) `shouldEqual` Right d4
        (A.decodeJson (A.encodeJson d1)) `shouldEqual` Right d4
