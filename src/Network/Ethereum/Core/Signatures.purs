module Network.Ethereum.Core.Signatures
  ( PrivateKey
  , PublicKey
  , unPublicKey
  , mkPublicKey
  , unPrivateKey
  , mkPrivateKey
  , Address
  , unAddress
  , mkAddress
  , privateToPublic
  , privateToAddress
  , publicToAddress
  , Signature(..)
  , signMessage
  , recoverSender
  , ChainId(..)
  , addChainIdOffset
  , removeChainIdOffset
  ) where

import Prelude

import Data.Argonaut as A
import Data.ByteString as BS
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..), fromJust)
import Foreign (ForeignError(..), fail)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Core.HexString (HexString, dropHex, hexLength, toByteString, fromByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- | Opaque PrivateKey type
newtype PrivateKey = PrivateKey BS.ByteString

instance showPrivateKey :: Show PrivateKey where
  show (PrivateKey pk) = show $ fromByteString pk

derive instance eqPrivateKey :: Eq PrivateKey

-- | Opaque PublicKey type
newtype PublicKey = PublicKey BS.ByteString

instance showPublicKey :: Show PublicKey where
  show (PublicKey pk) = show $ fromByteString pk

derive instance eqPublicKey :: Eq PublicKey

foreign import isValidPublic
  :: BS.ByteString
  -> Boolean

foreign import isValidPrivate
  :: BS.ByteString
  -> Boolean

-- | Get the underlying `HexString` representation of a PublicKey.
unPublicKey
  :: PublicKey
  -> HexString
unPublicKey (PublicKey pk) = fromByteString pk

-- | Attempt to construct a PublicKey from a HexString
mkPublicKey
  :: HexString
  -> Maybe PublicKey
mkPublicKey publicKey =
  let publicKeyBS = toByteString publicKey
  in if isValidPublic publicKeyBS
       then Just $ PublicKey publicKeyBS
       else Nothing

-- | Get the underlying `HexString` representation of a PrivateKey
unPrivateKey
  :: PrivateKey
  -> HexString
unPrivateKey (PrivateKey pk) = fromByteString pk

mkPrivateKey
  :: HexString
  -> Maybe PrivateKey
mkPrivateKey privateKey =
  let privateKeyBS = toByteString privateKey
  in if isValidPrivate privateKeyBS
       then Just $ PrivateKey privateKeyBS
       else Nothing

-- | Produce the `PublicKey` for the corresponding `PrivateKey`.
foreign import privateToPublic
  :: PrivateKey
  -> PublicKey


-- | Represents and Ethereum address, which is a 20 byte `HexString`
newtype Address = Address HexString

derive newtype instance addressShow :: Show Address
derive newtype instance addressEq :: Eq Address
derive newtype instance addressOrd :: Ord Address
derive newtype instance encodeAddress :: Encode Address

_decode :: HexString -> Either String Address
_decode hx = case mkAddress hx of
  Nothing -> Left $ "Address must be 20 bytes long: " <> show hx
  Just res -> Right res

instance decodeAddress :: Decode Address where
  decode a = do
    hxString <- decode a
    either (fail <<< ForeignError) pure $ _decode hxString

instance decodeJsonAddress :: A.DecodeJson Address where
  decodeJson json = do
    hxString <- A.decodeJson json
    _decode hxString

instance encodeJsonAddress :: A.EncodeJson Address where
  encodeJson = A.encodeJson <<< unAddress

instance readFAddress :: ReadForeign Address where
  readImpl = decode

instance writeFAddress :: WriteForeign Address where
  writeImpl = encode

unAddress :: Address -> HexString
unAddress (Address a) = a

mkAddress :: HexString -> Maybe Address
mkAddress hx = if hexLength hx == 40 then Just <<< Address $ hx else Nothing

-- | Produce the `Address` corresponding to the `PrivateKey`.
privateToAddress
  :: PrivateKey
  -> Address
privateToAddress = publicToAddress <<< privateToPublic

-- | Produce the `Address` corresponding to the `PublicKey`
publicToAddress
  :: PublicKey
  -> Address
publicToAddress (PublicKey publicKey) =
  let addrHex = fromByteString $ keccak256 publicKey
  in unsafePartial fromJust <<< mkAddress $ dropHex 24 addrHex

newtype Signature =
  Signature { r :: HexString
            , s :: HexString
            , v :: Int
            }

derive instance genericSignature :: Generic Signature _
derive instance eqSignature :: Eq Signature

instance showSignature :: Show Signature where
  show = genericShow

foreign import ecSign :: Fn2 PrivateKey BS.ByteString {r :: BS.ByteString, s :: BS.ByteString, v :: Int}

-- | Sign the message with a `PrivateKey`
signMessage
  :: PrivateKey
  -> BS.ByteString
  -> Signature
signMessage privateKey message =
  let {r,s,v} = runFn2 ecSign privateKey message
  in Signature { r: fromByteString r
               , s: fromByteString s
               , v
               }

foreign import ecRecover :: Fn3 BS.ByteString BS.ByteString Int PublicKey

-- | Recover the sender of the message from the `Signature`.
recoverSender
  :: BS.ByteString
  -> Signature
  -> PublicKey
recoverSender messageHash (Signature {v,r,s}) =
  runFn3 ecRecover messageHash (toByteString r <> toByteString s) v

-- | Used in Ethereum to prevent replay attacks
newtype ChainId = ChainId Int

derive instance genericChainId :: Generic ChainId _
derive instance eqChainId :: Eq ChainId

instance showChainId :: Show ChainId where
  show = genericShow

-- | Add the ChainId offset to the `Signature` `v` parameter.
addChainIdOffset
  :: ChainId
  -> Signature
  -> Signature
addChainIdOffset (ChainId cId) (Signature sig) = Signature sig {v = sig.v + 35 + 2 * cId}

-- | Remove the ChainId offset from the `Signature` `v` parameter.
removeChainIdOffset
  :: ChainId
  -> Signature
  -> Signature
removeChainIdOffset (ChainId cId) (Signature sig) = Signature sig {v = sig.v - 35 - 2 * cId}
