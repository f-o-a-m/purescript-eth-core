module Network.Ethereum.Core.BigNumber
  ( BigNumber(..)
  , class Algebra
  , embed
  , pow
  , toString
  , parseBigNumber
  , toTwosComplement
  , unsafeToInt
  --, floorBigNumber
  , module Int
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.Either (Either(..), either, hush)
import Data.Generic.Rep (class Generic)
import Data.Int (Radix, binary, decimal, floor, fromNumber, hexadecimal) as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, un)
import Data.Ring.Module (class LeftModule, class RightModule)
import Foreign (ForeignError(..), readString, fail)
import JS.BigInt (BigInt)
import JS.BigInt as BI
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Test.QuickCheck (class Arbitrary, arbitrary)

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

-- | Large Integer, needed for handling numbers of up to 32 bytes
newtype BigNumber = BigNumber BigInt

derive instance Newtype BigNumber _
derive instance Generic BigNumber _
derive instance Eq BigNumber
derive instance Ord BigNumber

instance Show BigNumber where
  show = toString Int.decimal

derive newtype instance Semiring BigNumber
derive newtype instance Ring BigNumber
instance CommutativeRing BigNumber
derive newtype instance EuclideanRing BigNumber

toString :: Int.Radix -> BigNumber -> String
toString radix = BI.toStringAs radix <<< un BigNumber

_encode :: BigNumber -> String
_encode = (append "0x") <<< toString Int.hexadecimal

_decode :: String -> Either String BigNumber
_decode str = case BI.fromString str of
  Nothing -> Left $ "Failed to parse as BigNumber: " <> str
  Just n -> Right (BigNumber n)

parseBigNumber :: Int.Radix -> String -> Maybe BigNumber
parseBigNumber _ = hush <<< _decode

instance ReadForeign BigNumber where
  readImpl x = do
    str <- readString x
    either (fail <<< ForeignError) pure $ _decode str

instance WriteForeign BigNumber where
  writeImpl = writeImpl <<< _encode

instance A.DecodeJson BigNumber where
  decodeJson json = do
    str <- A.decodeJson json
    either (const <<< Left $ UnexpectedValue json) Right $ _decode str

instance A.EncodeJson BigNumber where
  encodeJson = A.encodeJson <<< _encode

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

toTwosComplement :: BigNumber -> BigNumber
toTwosComplement n =
  if n < zero then maxBigNumber + n + one
  else n
  where
  maxBigNumber = BigNumber $ unsafePartial $ fromJust $
    BI.fromString "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

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
