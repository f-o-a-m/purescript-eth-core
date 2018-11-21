module CoreSpec.Keccak256 (keccak256Spec) where

import Prelude

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.HexString (mkHexString, toByteString)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

keccak256Spec :: Spec Unit
keccak256Spec = describe "keccak256-spec" do
    describe "Keccak256 tests" do
      it "can hash strings" do
        keccak256 "test123" `shouldEqual` mkBS "f81b517a242b218999ec8eec0ea6e2ddbef2a367a14e93f4a32a39e260f686ad"
        keccak256 "test(int)" `shouldEqual` mkBS "f4d03772bec1e62fbe8c5691e1a9101e520e8f8b5ca612123694632bf3cb51b1"
      it "can hash hex strings" do
        keccak256 (unsafePartial (fromJust <<< mkHexString) "80") `shouldEqual` mkBS "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"
        keccak256 "0x80" `shouldEqual` mkBS "6b03a5eef7706e3fb52a61c19ab1122fad7237726601ac665bd4def888f0e4a0"
        keccak256 (unsafePartial (fromJust <<< mkHexString) "3c9229289a6125f7fdf1885a77bb12c37a8d3b4962d936f7e3084dece32a3ca1") `shouldEqual` mkBS "82ff40c0a986c6a5cfad4ddf4c3aa6996f1a7837f9c398e17e5de5cbd5a12b28"

mkBS :: String -> ByteString
mkBS = toByteString <<< unsafePartial fromJust <<< mkHexString
