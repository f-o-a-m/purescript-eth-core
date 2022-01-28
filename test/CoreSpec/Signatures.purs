module CoreSpec.Signatures (signatureSpec) where

import Prelude

import Common (RawTransaction(..), makeTransactionMessage, mkAddress', mkHexString', mkPrivateKey')
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Core.BigNumber (embed)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures as Sig
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

signatureSpec :: Spec Unit
signatureSpec = do
  describe "Testing Public / PrivateKeys" $ do

    it "can use the public / private key smart constructors" $ do
      let privateKey = mkPrivateKey' "3c9229289a6125f7fdf1885a77bb12c37a8d3b4962d936f7e3084dece32a3ca1"
          publicKey = Sig.privateToPublic privateKey
      validatePrivatePublicPair privateKey publicKey

  describe "Transaction Signing" $ do

      it "can hash, sign, and recover transactions" do
        let privateKey = mkPrivateKey' "ebd915cdf9623ca2a951c8a31f8c3277457c52dcf15ce92a825629fbc8d939cb"
            publicKey = Sig.privateToPublic privateKey
            address = Sig.publicToAddress publicKey
            rawTx = RawTransaction $
                      { data: mkHexString' "d14e62b80000000000000000000000000000000000000000000000000000000000000001"
                      , gas: embed 41669
                      , gasPrice: embed 1000000000
                      , nonce : embed 41
                      , value : Nothing
                      , to: Just $ mkAddress' "86b89c0906b111508d5caa38e2e61689a124c860"
                      }
            -- this is a rinkeby transaction
            chainId = Sig.ChainId 4
            hashedMessage = keccak256 $ makeTransactionMessage chainId rawTx
            sig@(Sig.Signature signature) = Sig.signMessage privateKey hashedMessage
            Sig.Signature testSignature = Sig.removeChainIdOffset chainId $ Sig.Signature
                                            { v: 43
                                            , r: mkHexString' "f2458a57ea1c25f9d6220efc7128aa3ecd9b1397b158f8c29ce27a520994d03c"
                                            , s: mkHexString' "0689a079ade60f4d1235fa866bf41e7ac6f11931d60548d082cbf7567c046cf7"
                                            }
        address `shouldEqual` mkAddress' "0x032392b4bdb5c7b19506131fdb8be108ce8ad995"
        Sig.recoverSender hashedMessage sig `shouldEqual` publicKey
        signature.v `shouldEqual` testSignature.v
        signature.r `shouldEqual` testSignature.r
        signature.s `shouldEqual` testSignature.s

--------------------------------------------------------------------------------
-- Helper types and functions
--------------------------------------------------------------------------------

validatePrivatePublicPair
  :: Sig.PrivateKey
  -> Sig.PublicKey
  -> Aff Unit
validatePrivatePublicPair private public = do
  let mpublic = Sig.mkPublicKey <<< Sig.unPublicKey $ public
  mpublic `shouldEqual` Just public
  let mprivate = Sig.mkPrivateKey <<< Sig.unPrivateKey $ private
  mprivate `shouldEqual` Just private
  Sig.privateToPublic private `shouldEqual` public

{-

Test Transactions

{
  "blockHash": "0xd3d3fda50e1f5e7d835f12db6a8c7ad68c10735c1928315d85aa6c04cde4197a",
  "blockNumber": 2123152,
  "from": "0x032392b4bdb5c7b19506131fdb8be108ce8ad995",
  "gas": 41669,
  "gasPrice": "1000000000",
  "hash": "0x221ea3bce6752d64f3b6330af3a2b09e225e026fd0309db04cb61814cadd99f2",
  "input": "0xd14e62b80000000000000000000000000000000000000000000000000000000000000001",
  "nonce": 41,
  "to": "0x86b89c0906b111508d5caa38e2e61689a124c860",
  "transactionIndex": 8,
  "value": "0",
  "v": "0x2b",
  "r": "0xf2458a57ea1c25f9d6220efc7128aa3ecd9b1397b158f8c29ce27a520994d03c",
  "s": "0x689a079ade60f4d1235fa866bf41e7ac6f11931d60548d082cbf7567c046cf7"
}

-}
