module Network.Ethereum.Core.BigNumber
  ( BigNumber
  , class Algebra, embed
  , pow
  , toString
  , parseBigNumber
  , toTwosComplement
  , unsafeToInt
  , floorBigNumber
  , divide
  , module Int
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.Either (Either(..), either)
import Data.Int (Radix, binary, decimal, hexadecimal, floor) as Int
import Data.Maybe (Maybe(..))
import Data.Ring.Module (class LeftModule, class RightModule)
import Foreign (ForeignError(..), readString, fail)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Simple.JSON (class ReadForeign, class WriteForeign)

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

-- | Large Integer, needed for handling numbers of up to 32 bytes
foreign import data BigNumber :: Type

-- | Convert a Big number into a string in the given base
foreign import toString :: Int.Radix -> BigNumber -> String

instance showBigNumber :: Show BigNumber where
  show = toString Int.decimal

foreign import _eqBigNumber :: BigNumber -> BigNumber -> Boolean

instance eqBigNumber :: Eq BigNumber where
  eq = _eqBigNumber

foreign import comparedTo :: BigNumber -> BigNumber -> Int

instance ordBigNumber :: Ord BigNumber where
  compare bn1 bn2 =
    let n = comparedTo bn1 bn2
    in case n of
         0 -> EQ
         1 -> GT
         _ -> LT

foreign import _addBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _mulBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _intToBigNumber :: Int -> BigNumber

embedInt :: Int -> BigNumber
embedInt = _intToBigNumber

foreign import _numberToBigNumber :: Number -> BigNumber

instance semiringBigNumber :: Semiring BigNumber where
  add = _addBigNumber
  mul = _mulBigNumber
  zero = embedInt 0
  one = embedInt 1

foreign import _subBigNumber :: BigNumber -> BigNumber -> BigNumber

instance ringBigNumber :: Ring BigNumber where
  sub = _subBigNumber

instance bigNumberLModule :: LeftModule BigNumber Int where
  mzeroL = embedInt 0
  maddL = add
  msubL = sub
  mmulL a b = embedInt a * b

instance bigNumberRModule :: RightModule BigNumber Int where
  mzeroR = embedInt 0
  maddR = add
  msubR = sub
  mmulR a b = a * embedInt b

class (Ring r, Ring a, LeftModule a r, RightModule a r) <= Algebra a r where
  embed :: r -> a

instance embedInt' :: Algebra BigNumber Int where
  embed = embedInt

foreign import divide :: BigNumber -> BigNumber -> BigNumber

foreign import fromStringAsImpl
  :: (forall a . a -> Maybe a)
  -> (forall a . Maybe a)
  -> Int.Radix
  -> String
  -> Maybe BigNumber

-- | Convert a string in the given base to a `BigNumber`
parseBigNumber :: Int.Radix -> String -> Maybe BigNumber
parseBigNumber = fromStringAsImpl Just Nothing

-- | Take the twos complement of a `BigNumer`
foreign import toTwosComplement :: BigNumber -> BigNumber

-- | Exponentiate a `BigNumber`
foreign import pow :: BigNumber -> Int -> BigNumber

foreign import toNumber :: BigNumber -> Number

-- | Unsafely coerce a BigNumber to an Int.
unsafeToInt :: BigNumber -> Int
unsafeToInt = Int.floor <<< toNumber

-- | Take the integer part of a big number
foreign import floorBigNumber :: BigNumber -> BigNumber

_encode :: BigNumber -> String
_encode = (append "0x") <<< toString Int.hexadecimal

_decode :: String -> Either String BigNumber
_decode str = case parseBigNumber Int.hexadecimal str of
  Nothing -> Left $ "Failed to parse as BigNumber: " <> str
  Just n -> Right n

instance decodeBigNumber :: Decode BigNumber where
  decode x = do
    str <- readString x
    either (fail <<< ForeignError) pure $ _decode str

instance readFBigNumber :: ReadForeign BigNumber where
  readImpl = decode

instance writeFBigNumber :: WriteForeign BigNumber where
  writeImpl = encode

instance encodeBigNumber :: Encode BigNumber where
  encode = encode <<< _encode

instance decodeJsonBigNumber :: A.DecodeJson BigNumber where
  decodeJson json = do
    str <- A.decodeJson json
    either (const <<< Left $ UnexpectedValue json) Right $ _decode str

instance encodeJsonBigNumber :: A.EncodeJson BigNumber where
  encodeJson = A.encodeJson <<< _encode
