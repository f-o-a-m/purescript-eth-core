module CoreSpec.Hex (hexSpec) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut as A
import Data.ByteString as BS
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(Just), fromJust)
import Foreign (unsafeToForeign)
import Foreign.Class (encode, decode)
import Network.Ethereum.Core.HexString (HexString, mkHexString, toByteString, toUtf8, toAscii, fromUtf8, fromAscii)
import Node.Encoding (Encoding(Hex))
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readImpl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

hexSpec :: Spec Unit
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
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700") `shouldEqual` "myString\x00"
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "65787065637465642076616c7565000000000000000000000000000000000000")
          `shouldEqual` "expected value\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

      it "can convert strings to hex" do
        fromUtf8 "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromUtf8 "myString\x00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromUtf8 "expected value\x00\x00\x00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "65787065637465642076616c7565"

    describe "ascii tests" do

      it "can convert hex strings to ascii" do

        toAscii (unsafePartial (fromJust <<< mkHexString) "6d79537472696e67") `shouldEqual` "myString"
        toAscii (unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700") `shouldEqual` "myString\x0000"
      --  toAscii ((fromJust <<< mkHexString) "0300000035e8c6d54c5d127c9dcebe9e1a37ab9b05321128d097590a3c100000000000006521df642ff1f5ec0c3a7aa6cea6b1e7b7f7cda2cbdf07362a85088e97f19ef94331c955c0e9321ad386428c")
      --    `shouldEqual` "\x0003\x0000\x0000\x00005èÆÕL]\x0012|Î¾\x001a7«\x00052\x0011(ÐY\n<\x0010\x0000\x0000\x0000\x0000\x0000\x0000e!ßd/ñõì\f:z¦Î¦±ç·÷Í¢Ëß\x00076*\bñùC1ÉUÀé2\x001aÓB"

      it "can convert asci to hex" do
        fromAscii "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromAscii "myString\x00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700"

    describe "json tests" do

      it "can convert hex strings to and from json" do

        let hx = (unsafePartial (fromJust <<< mkHexString) "0x6d79537472696e67")
            hxJson = A.fromString "0x6d79537472696e67"
        (A.stringify <<< A.encodeJson <$> (A.decodeJson hxJson :: Either JsonDecodeError HexString)) `shouldEqual` Right (A.stringify hxJson)
        A.decodeJson (A.encodeJson hx) `shouldEqual` Right hx

      it "can handle deserialization" do
        let hxString = "0f43"
            d1 = unsafePartial $ fromJust $ hush $ runExcept $ readImpl (unsafeToForeign hxString)
            d2 = unsafePartial $ fromJust $ hush $ runExcept $ decode (unsafeToForeign hxString)
            d3 = unsafePartial $ fromJust $ hush $ A.decodeJson (A.fromString hxString)
            d4 = unsafePartial $ fromJust $ mkHexString hxString
        d4 `shouldEqual` d1
        d4 `shouldEqual` d2
        d4 `shouldEqual` d3
        runExcept (decode (encode d1)) `shouldEqual` Right d4
        (A.decodeJson (A.encodeJson d1)) `shouldEqual` Right d4
