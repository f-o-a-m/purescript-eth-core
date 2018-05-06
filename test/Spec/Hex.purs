module CoreSpec.Hex (hexSpec) where

import Prelude

import Data.Argonaut as A
import Data.Either (Either(..))
import Data.ByteString as BS
import Data.Maybe (Maybe(Just), fromJust)
import Network.Ethereum.Core.HexString (HexString, mkHexString, toByteString, toUtf8, toAscii, fromUtf8, fromAscii)
import Node.Encoding (Encoding(Hex))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

hexSpec :: forall r . Spec r Unit
hexSpec = describe "hex-spec" do

    describe "bytestringFromHexString" do

      it "can convert byteStrings to HexString" do
        let hx = unsafePartial fromJust $ mkHexString "1234"
            bs1 = toByteString $ hx
            bs2 = BS.fromString "1234" Hex
        Just bs1 `shouldEqual` bs2

    describe "utf tests" do

      it "can convert hex strings to utf8" do
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "6d79537472696e67") `shouldEqual` "myString"
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700") `shouldEqual` "myString\00"
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "65787065637465642076616c7565000000000000000000000000000000000000")
          `shouldEqual` "expected value\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00"

      it "can convert strings to hex" do
        fromUtf8 "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromUtf8 "myString\00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromUtf8 "expected value\00\00\00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "65787065637465642076616c7565"

    describe "ascii tests" do

      it "can convert hex strings to ascii" do

        toAscii (unsafePartial (fromJust <<< mkHexString) "6d79537472696e67") `shouldEqual` "myString"
        toAscii (unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700") `shouldEqual` "myString\0000"
      --  toAscii ((fromJust <<< mkHexString) "0300000035e8c6d54c5d127c9dcebe9e1a37ab9b05321128d097590a3c100000000000006521df642ff1f5ec0c3a7aa6cea6b1e7b7f7cda2cbdf07362a85088e97f19ef94331c955c0e9321ad386428c")
      --    `shouldEqual` "\0003\0000\0000\00005èÆÕL]\0012|Î¾\001a7«\00052\0011(ÐY\n<\0010\0000\0000\0000\0000\0000\0000e!ßd/ñõì\f:z¦Î¦±ç·÷Í¢Ëß\00076*\bñùC1ÉUÀé2\001aÓB"

      it "can convert asci to hex" do
        fromAscii "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromAscii "myString\00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700"

    describe "json tests" do

      it "can convert hex strings to and from json" do

        let hx = (unsafePartial (fromJust <<< mkHexString) "0x6d79537472696e67")
            hxJson = A.fromString "0x6d79537472696e67"
        (A.encodeJson <$> (A.decodeJson hxJson :: Either String HexString)) `shouldEqual` Right hxJson
        A.decodeJson (A.encodeJson hx) `shouldEqual` Right hx
