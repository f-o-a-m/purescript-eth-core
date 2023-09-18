module Common where

import Prelude

import Data.ByteString as BS
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.RLP as RLP
import Network.Ethereum.Core.Signatures as Sig
import Partial.Unsafe (unsafeCrashWith)

-- Helpers to construct test objects
mkPrivateKey' :: String -> Sig.PrivateKey
mkPrivateKey' pkey = case Sig.mkPrivateKey =<< Hex.mkHexString pkey of
  Nothing -> unsafeCrashWith $ "Invalid Private Key: " <> pkey
  Just pkey' -> pkey'

mkAddress' :: String -> Sig.Address
mkAddress' addr = case Sig.mkAddress =<< Hex.mkHexString addr of
  Nothing -> unsafeCrashWith $ "Invalid Address: " <> addr
  Just addr' -> addr'

mkHexString' :: String -> Hex.HexString
mkHexString' hx = case Hex.mkHexString hx of
  Nothing -> unsafeCrashWith $ "Invalid HexString: " <> hx
  Just hx' -> hx'

-- signature stuff should also work on transactions, these are arguable the most compliced examples.
newtype RawTransaction =
  RawTransaction
    { to :: Maybe Sig.Address
    , value :: Maybe BigNumber
    , gas :: BigNumber
    , gasPrice :: BigNumber
    , data :: Hex.HexString
    , nonce :: BigNumber
    }

derive instance Generic RawTransaction _
derive instance Eq RawTransaction

instance Show RawTransaction where
  show = genericShow

makeTransactionMessage
  :: Sig.ChainId
  -> RawTransaction
  -> BS.ByteString
makeTransactionMessage (Sig.ChainId chainId) (RawTransaction tx) =
  let
    txWithChainId =
      RLP.RLPArray
        [ RLP.RLPBigNumber tx.nonce
        , RLP.RLPBigNumber tx.gasPrice
        , RLP.RLPBigNumber tx.gas
        , maybe RLP.RLPNull RLP.RLPAddress tx.to
        , maybe RLP.RLPNull RLP.RLPBigNumber tx.value
        , RLP.RLPHexString tx.data
        , RLP.RLPInt chainId
        , RLP.RLPInt 0
        , RLP.RLPInt 0
        ]
  in
    RLP.rlpEncode txWithChainId
