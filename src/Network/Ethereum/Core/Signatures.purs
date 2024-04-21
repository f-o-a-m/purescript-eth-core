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
  , generator
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Node.Buffer.Immutable as B
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Foreign (ForeignError(..), fail)
import Network.Ethereum.Core.HexString (HexString, dropBytes, fromBuffer, nullWord, numberOfBytes, takeBytes, toBuffer)
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.Keccak256 (keccak256)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)
import Node.Encoding (Encoding(UTF8))

-- | Opaque PrivateKey type
newtype PrivateKey = PrivateKey B.ImmutableBuffer

instance Show PrivateKey where
  show (PrivateKey pk) = show $ fromBuffer pk

derive instance Eq PrivateKey

-- | Opaque PublicKey type
newtype PublicKey = PublicKey B.ImmutableBuffer

instance Show PublicKey where
  show (PublicKey pk) = show $ fromBuffer pk

derive instance Eq PublicKey

foreign import isValidPublic
  :: B.ImmutableBuffer
  -> Boolean

foreign import isValidPrivate
  :: B.ImmutableBuffer
  -> Boolean

-- | Get the underlying `HexString` representation of a PublicKey.
unPublicKey
  :: PublicKey
  -> HexString
unPublicKey (PublicKey pk) = fromBuffer pk

-- | Attempt to construct a PublicKey from a HexString
mkPublicKey
  :: HexString
  -> Maybe PublicKey
mkPublicKey publicKey =
  let
    publicKeyB = toBuffer publicKey
  in
    if isValidPublic publicKeyB then Just $ PublicKey publicKeyB
    else Nothing

-- | Get the underlying `HexString` representation of a PrivateKey
unPrivateKey
  :: PrivateKey
  -> HexString
unPrivateKey (PrivateKey pk) = fromBuffer pk

mkPrivateKey
  :: HexString
  -> Maybe PrivateKey
mkPrivateKey privateKey =
  let
    privateKeyB = toBuffer privateKey
  in
    if isValidPrivate privateKeyB then Just $ PrivateKey privateKeyB
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

generator :: forall m. MonadGen m => m Address
generator = Address <$> Hex.generator 20

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
    addrHex = fromBuffer $ keccak256 publicKey
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

foreign import ecSign :: Fn2 PrivateKey B.ImmutableBuffer { r :: B.ImmutableBuffer, s :: B.ImmutableBuffer, v :: Int }

-- | Sign the message with a `PrivateKey`
signMessage
  :: PrivateKey
  -> B.ImmutableBuffer
  -> Signature
signMessage privateKey message =
  let
    { r, s, v } = runFn2 ecSign privateKey message
  in
    Signature
      { r: fromBuffer r
      , s: fromBuffer s
      , v
      }

-- | Prefix a message with the "Ethereum Signed Message" prefix
toEthSignedMessage :: B.ImmutableBuffer -> B.ImmutableBuffer
toEthSignedMessage bs =
  let x19 = B.fromArray [25] -- 0x19 == 25 dec
      pfx = B.fromString "Ethereum Signed Message:\n" UTF8
      lenStr = B.fromString (show $ B.size bs) UTF8
  in B.concat [x19, pfx, lenStr, bs]

foreign import ecRecover :: Fn3 B.ImmutableBuffer B.ImmutableBuffer Int PublicKey

-- | Recover the sender of the message from the `Signature`.
recoverSender
  :: B.ImmutableBuffer
  -> Signature
  -> PublicKey
recoverSender messageHash (Signature { v, r, s }) =
  runFn3 ecRecover messageHash (B.concat [toBuffer r, toBuffer s]) v

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
