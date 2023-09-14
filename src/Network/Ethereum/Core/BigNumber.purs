module Network.Ethereum.Core.BigNumber
  ( class Algebra
  , BigNumber(..)
  , embed
  , module Int
  , pow
  , toString
  , toStringAs
  , fromString
  , fromStringAs
  , toTwosComplement
  , fromTwosComplement
  , unsafeToInt
  , generator
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (Radix, binary, decimal, floor, fromNumber, hexadecimal) as Int
import Data.Int (hexadecimal)
import Data.Maybe (Maybe, fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, un)
import Data.Ring.Module (class LeftModule, class RightModule)
import Data.String (Pattern(..), stripPrefix)
import Foreign as F
import JS.BigInt (BigInt)
import JS.BigInt as BI
import Network.Ethereum.Core.HexString as Hex
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

-- | Large Integer, needed for handling numbers of up to 32 bytes
newtype BigNumber = BigNumber BigInt

derive instance Newtype BigNumber _
derive instance Generic BigNumber _
derive newtype instance Eq BigNumber
derive newtype instance Ord BigNumber

instance Show BigNumber where
  show (BigNumber bn) = BI.toStringAs Int.decimal bn

derive newtype instance Semiring BigNumber
derive newtype instance Ring BigNumber
instance CommutativeRing BigNumber
derive newtype instance EuclideanRing BigNumber

generator :: forall m. MonadGen m => m BigNumber
generator = do
  n <- chooseInt 1 32
  bytes <- Hex.generator n
  pure
    $ fromTwosComplement (8 * n)
    $ BigNumber
    $ unsafePartial fromJust
    $ BI.fromStringAs hexadecimal (Hex.unHex bytes)

fromString :: String -> Maybe BigNumber
fromString s = fromStringAs Int.hexadecimal s

fromStringAs :: Int.Radix -> String -> Maybe BigNumber
fromStringAs r s = BigNumber <$> BI.fromStringAs r s

toString :: BigNumber -> String
toString bn = toStringAs Int.hexadecimal bn

toStringAs :: Int.Radix -> BigNumber -> String
toStringAs r (BigNumber bn) = BI.toStringAs r bn

instance ReadForeign BigNumber where
  readImpl x = do
    _str <- F.readString x
    let str = fromMaybe _str $ stripPrefix (Pattern "0x") _str
    maybe (F.fail $ F.ForeignError "Expected hex encoded BigInt") pure (fromString str)

instance WriteForeign BigNumber where
  writeImpl = writeImpl <<< ("0x" <> _) <<< toString

instance A.DecodeJson BigNumber where
  decodeJson json = do
    _str <- A.decodeJson json
    let str = fromMaybe _str $ stripPrefix (Pattern "0x") _str
    maybe (Left $ A.TypeMismatch "Expected hex encoded BigInt") pure $ fromString str

instance A.EncodeJson BigNumber where
  encodeJson = A.encodeJson <<< ("0x" <> _) <<< toString

toNumber :: BigNumber -> Number
toNumber = BI.toNumber <<< un BigNumber

-- | Exponentiate a `BigNumber`
pow :: BigNumber -> Int -> BigNumber
pow (BigNumber n) k = BigNumber $ BI.pow n (BI.fromInt k)

-- | Unsafely coerce a BigNumber to an Int.
unsafeToInt :: BigNumber -> Int
unsafeToInt n = unsafePartial $ fromJust
  $ Int.fromNumber
  $ toNumber n

toTwosComplement :: Int -> BigNumber -> BigNumber
toTwosComplement _nbits (BigNumber n) = BigNumber $
  if n < zero then (n + (one `BI.shl` nbits)) `BI.and` ((one `BI.shl` nbits) - one)
  else n `BI.and` ((one `BI.shl` nbits) - one)
  where
  nbits = BI.fromInt _nbits

fromTwosComplement :: Int -> BigNumber -> BigNumber
fromTwosComplement _nbits (BigNumber n) = BigNumber $
  if n `BI.and` signBitMask == zero then n
  else
    let
      magnitude = ((BI.not n) `BI.and` ((one `BI.shl` nbits) - one)) + one
    in
      -magnitude
  where
  nbits = BI.fromInt _nbits
  signBitMask = one `BI.shl` (nbits - one)

instance LeftModule BigNumber Int where
  mzeroL = zero
  maddL = add
  msubL = sub
  mmulL a b = embed a * b

instance RightModule BigNumber Int where
  mzeroR = zero
  maddR = add
  msubR = sub
  mmulR a b = a * embed b

class (Ring r, Ring a, LeftModule a r, RightModule a r) <= Algebra a r where
  embed :: r -> a

instance Algebra BigNumber Int where
  embed = BigNumber <<< BI.fromInt