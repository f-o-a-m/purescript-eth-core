module Network.Ethereum.Core.RLP
  ( class RLPEncode
  , RLPObject(..)
  , rlpEncode
  ) where

import Prelude
import Data.ByteString (ByteString)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString (HexString, unHex)
import Network.Ethereum.Core.Signatures (Address, unAddress)
import Unsafe.Coerce (unsafeCoerce)

class RLPEncode a where
  rlpEncode :: a -> ByteString

data RLPObject =
    RLPNull
  | RLPString String
  | RLPHexString HexString
  | RLPAddress Address
  | RLPInt Int
  | RLPBigNumber BigNumber
  | RLPByteString ByteString
  | RLPArray (Array RLPObject)

data RLPVal = RLPVal

foreign import _rlpNull :: RLPVal

transRLP
  :: RLPObject
  -> RLPVal
transRLP obj = case obj of
    RLPNull -> _rlpNull
    RLPString s -> mkRLPVal s
    RLPInt n -> mkRLPVal n
    RLPHexString hx -> mkRLPVal $ "0x" <> (unHex hx)
    RLPAddress addr -> transRLP <<< RLPHexString $ unAddress addr
    RLPBigNumber bn -> mkRLPVal bn
    RLPByteString bs -> mkRLPVal bs
    RLPArray as -> mkRLPVal $ map transRLP as
  where
    mkRLPVal :: forall a. a -> RLPVal
    mkRLPVal = unsafeCoerce

foreign import _rlpEncode :: RLPVal -> ByteString

instance rlpEncodeObject :: RLPEncode RLPObject where
  rlpEncode = _rlpEncode <<< transRLP
