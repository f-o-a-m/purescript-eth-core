module CoreSpec.Hex (hexSpec) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.ByteString as BS
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(Just), fromJust)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Network.Ethereum.Core.HexString (HexString, fromAscii, fromUtf8, mkHexString, toAscii, toByteString, toUtf8, unHex)
import Node.Encoding (Encoding(Hex))
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readImpl, writeImpl)
import Test.QuickCheck (quickCheck)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

hexSpec :: Spec Unit
hexSpec = describe "hex-spec" do

  describe "bytestringFromHexString" do

    it "can convert byteStrings to HexString" do
      let
        hx = unsafePartial fromJust $ mkHexString "1234"
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

    it "can convert asci to hex" do
      fromAscii "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
      fromAscii "myString\x00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700"

  describe "json tests" do

    it "can convert hex strings to and from json" $ liftEffect do
      quickCheck $ \(hx :: HexString) ->
        let
          hxJson = A.fromString $ ("0x" <> unHex hx)
        in
          (A.stringify <<< A.encodeJson @HexString <$> (A.decodeJson hxJson)) == Right (A.stringify hxJson) &&
            A.decodeJson (A.encodeJson hx) == Right hx

    it "can handle deserialization" $ liftEffect do
      quickCheck $ \(_hxString :: HexString) ->
        let
          hxString = unHex _hxString
          d1 = unsafePartial $ fromJust $ hush $ runExcept $ readImpl (unsafeToForeign hxString)
          d2 = unsafePartial $ fromJust $ hush $ A.decodeJson (A.fromString hxString)
          d3 = unsafePartial $ fromJust $ mkHexString hxString
        in
          d3 == d1
            && d3 == d2
            && runExcept (readImpl (writeImpl d1)) == Right d3
            && (A.decodeJson (A.encodeJson d1)) == Right d3
