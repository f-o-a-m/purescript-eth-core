module Network.Ethereum.Core.Signatures
  ( PrivateKey
  , PublicKey
  , unPublicKey
  , mkPublicKey
  , unPrivateKey
  , mkPrivateKey
  , generatePrivateKey
  , Address
  , unAddress
  , mkAddress
  , nullAddress
  , privateToPublic
  , privateToAddress
  , publicToAddress
  , Signature(..)
  , signMessage
  , toEthSignedMessage
  , recoverSender
  , ChainId(..)
  , addChainIdOffset
  , removeChainIdOffset
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.ByteString as BS
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Foreign (ForeignError(..), fail)
-- import Foreign.Class (class Decode, class Encode, decode, encode)
import Network.Ethereum.Core.HexString (HexString, takeBytes, nullWord, dropBytes, numberOfBytes, toByteString, fromByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)
import Type.Quotient (mkQuotient)

-- | Opaque PrivateKey type
newtype PrivateKey = PrivateKey BS.ByteString

instance Show PrivateKey where
  show (PrivateKey pk) = show $ fromByteString pk

derive instance Eq PrivateKey

-- | Opaque PublicKey type
newtype PublicKey = PublicKey BS.ByteString

instance Show PublicKey where
  show (PublicKey pk) = show $ fromByteString pk

derive instance Eq PublicKey

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
  let
    publicKeyBS = toByteString publicKey
  in
    if isValidPublic publicKeyBS then Just $ PublicKey publicKeyBS
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
  let
    privateKeyBS = toByteString privateKey
  in
    if isValidPrivate privateKeyBS then Just $ PrivateKey privateKeyBS
    else Nothing

-- | Produce the `PublicKey` for the corresponding `PrivateKey`.
foreign import privateToPublic
  :: PrivateKey
  -> PublicKey

-- | Generate a valid private key using `crypto.randomBytes`
foreign import generatePrivateKey :: Effect PrivateKey

-- | Represents and Ethereum address, which is a 20 byte `HexString`
newtype Address = Address HexString

derive newtype instance Show Address
derive newtype instance Eq Address
derive newtype instance Ord Address

_decode :: HexString -> Either String Address
_decode hx = case mkAddress hx of
  Nothing -> Left $ "Address must be 20 bytes long: " <> show hx
  Just res -> Right res

instance ReadForeign Address where
  readImpl a = do
    hexString <- readImpl a
    either (fail <<< ForeignError) pure $ _decode hexString

instance WriteForeign Address where
  writeImpl = writeImpl <<< unAddress

instance A.DecodeJson Address where
  decodeJson json = do
    hxString <- A.decodeJson json
    either (const <<< Left $ UnexpectedValue json) Right $ _decode hxString

instance A.EncodeJson Address where
  encodeJson = A.encodeJson <<< unAddress

unAddress :: Address -> HexString
unAddress (Address a) = a

mkAddress :: HexString -> Maybe Address
mkAddress hx = if numberOfBytes hx == 20 then Just <<< Address $ hx else Nothing

nullAddress :: Address
nullAddress = Address $ takeBytes 20 nullWord

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
  let
    addrHex = fromByteString $ keccak256 publicKey
  in
    unsafePartial fromJust <<< mkAddress $ dropBytes 12 addrHex

newtype Signature =
  Signature
    { r :: HexString
    , s :: HexString
    , v :: Int
    }

derive instance Generic Signature _
derive instance Eq Signature

instance Show Signature where
  show = genericShow

foreign import ecSign :: Fn2 PrivateKey BS.ByteString { r :: BS.ByteString, s :: BS.ByteString, v :: Int }

-- | Sign the message with a `PrivateKey`
signMessage
  :: PrivateKey
  -> BS.ByteString
  -> Signature
signMessage privateKey message =
  let
    { r, s, v } = runFn2 ecSign privateKey message
  in
    Signature
      { r: fromByteString r
      , s: fromByteString s
      , v
      }

-- | Prefix a message with the "Ethereum Signed Message" prefix
toEthSignedMessage :: BS.ByteString -> Maybe BS.ByteString
toEthSignedMessage bs = do
  let x19 = BS.singleton (mkQuotient 25) -- 0x19 == 25 dec
  pfx <- BS.fromString "Ethereum Signed Message:\n" BS.UTF8
  lenStr <- BS.fromString (show $ BS.length bs) BS.UTF8
  pure $ x19 <> pfx <> lenStr <> bs

foreign import ecRecover :: Fn3 BS.ByteString BS.ByteString Int PublicKey

-- | Recover the sender of the message from the `Signature`.
recoverSender
  :: BS.ByteString
  -> Signature
  -> PublicKey
recoverSender messageHash (Signature { v, r, s }) =
  runFn3 ecRecover messageHash (toByteString r <> toByteString s) v

-- | Used in Ethereum to prevent replay attacks
newtype ChainId = ChainId Int

derive instance Generic ChainId _
derive instance Eq ChainId

instance Show ChainId where
  show = genericShow

-- | Add the ChainId offset to the `Signature` `v` parameter.
addChainIdOffset
  :: ChainId
  -> Signature
  -> Signature
addChainIdOffset (ChainId cId) (Signature sig) = Signature sig { v = sig.v + 35 + 2 * cId }

-- | Remove the ChainId offset from the `Signature` `v` parameter.
removeChainIdOffset
  :: ChainId
  -> Signature
  -> Signature
removeChainIdOffset (ChainId cId) (Signature sig) = Signature sig { v = sig.v - 35 - 2 * cId }
