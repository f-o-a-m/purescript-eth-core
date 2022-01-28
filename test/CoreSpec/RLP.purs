module CoreSpec.RLP (rlpSpec) where

import Prelude

import Common (RawTransaction(..), makeTransactionMessage, mkAddress', mkHexString')
import Data.ByteString as BS
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Core.BigNumber (embed, pow)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.RLP as RLP
import Network.Ethereum.Core.Signatures as Sig
import Node.Encoding (Encoding(Hex, UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Type.Quotient (mkQuotient)

rlpSpec :: Spec Unit
rlpSpec = do

  describe "RLP Spec" $ do

    it "should return itself if single byte and less than 0x7f" do
      let encoded = RLP.rlpEncode $ RLP.RLPHexString (mkHexString' "0a")
      BS.length encoded `shouldEqual` 1

    it "can match the RLP encoding for ints" $ do
      let encoded = RLP.rlpEncode $ RLP.RLPInt 1024
      BS.length encoded `shouldEqual` 3
      BS.unsafeIndex encoded 0 `shouldEqual` mkQuotient 130
      BS.unsafeIndex encoded 1 `shouldEqual` mkQuotient 4
      BS.unsafeIndex encoded 2 `shouldEqual` mkQuotient 0

    it "can match the RLP encoding for strings" $ do
      let testString = "This function takes in a data, convert it to buffer if not, and a length for recursion"
          testStringBS = unsafePartial fromJust $ BS.fromString testString UTF8
          encoded = RLP.rlpEncode $ RLP.RLPByteString testStringBS

      BS.unsafeIndex encoded 0 `shouldEqual` mkQuotient 184
      BS.unsafeIndex encoded 1 `shouldEqual` mkQuotient 86

    it "can match the RLP encoding for nested arrays" $ do
      let nestedList = RLP.RLPArray
                         [ RLP.RLPArray []
                         , RLP.RLPArray [ RLP.RLPArray [] ]
                         , RLP.RLPArray [ RLP.RLPArray []
                                        , RLP.RLPArray [ RLP.RLPArray [] ]
                                        ]
                         ]
          encoded = RLP.rlpEncode nestedList
          testVal = BS.pack $ map mkQuotient [199, 192, 193, 192, 195, 192, 193, 192]
      encoded `shouldEqual` testVal

    it "can match the RLP encoding for transactions" $ do
      let testVal = BS.pack $ map mkQuotient [193, 128]
          encoded = RLP.rlpEncode $ RLP.RLPArray [RLP.RLPNull]
      encoded `shouldEqual` testVal

    it "can encode zero" $ do
      let encoded = RLP.rlpEncode $ RLP.RLPByteString $ BS.pack [mkQuotient 0]
          testVal = BS.pack [mkQuotient 0]
      encoded `shouldEqual` testVal

    it "can encode zero the int" $ do
      let encoded = RLP.rlpEncode $ RLP.RLPInt 0
          testVal = BS.pack [mkQuotient 128]
      encoded `shouldEqual` testVal

    it "EIP155 example" $ do
      let rawTx = RawTransaction $
                    { data: mempty
                    , gas: embed 21000
                    , gasPrice: embed 20 * (embed 10 `pow` 9)
                    , nonce : embed 9
                    , value : Just $ embed 10 `pow` 18
                    , to: Just $ mkAddress' "0x3535353535353535353535353535353535353535"
                    }
          chainId = Sig.ChainId 1
          rlpEncodingString = "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080"
          hashedMessageString = "daf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53"
          txMessage = makeTransactionMessage chainId rawTx
      BS.toString txMessage Hex `shouldEqual` rlpEncodingString
      BS.toString (keccak256 txMessage) Hex `shouldEqual` hashedMessageString

    it "can match the RLP encoding for transactions" $ do
      let rawTx = RawTransaction $
                    { data: mkHexString' "d14e62b80000000000000000000000000000000000000000000000000000000000000001"
                    , gas: embed 41669
                    , gasPrice: embed 1000000000
                    , nonce : embed 41
                    , value : Nothing
                    , to: Just $ mkAddress' "86b89c0906b111508d5caa38e2e61689a124c860"
                    }
          chainId = Sig.ChainId 4
          rlpEncodingString = "f84729843b9aca0082a2c59486b89c0906b111508d5caa38e2e61689a124c86080a4d14e62b80000000000000000000000000000000000000000000000000000000000000001048080"
          -- hashedMessageString = "f5bbe123594fb9f588c98b069063ce1d224917b7374c700e49ec4fce0f63f999"
      BS.toString (makeTransactionMessage chainId rawTx) Hex `shouldEqual` rlpEncodingString
