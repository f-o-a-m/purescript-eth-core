module Network.Ethereum.Core.BigNumber
  ( BigNumber
  , class Algebra, embed
  , pow
  , toString
  , parseBigNumber
  , unsafeToInt
  , floorBigNumber
  , divide
  , bigNumberToTwosComplementInt256
  , twosComplementInt256ToBigNumber
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

-- TODO(srghma): remove instance names in libs
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

instance commutativeRingBigNumber :: CommutativeRing BigNumber

foreign import _divBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _modBigNumber :: BigNumber -> BigNumber -> BigNumber

instance euclidianRingBigNumber :: EuclideanRing BigNumber where
  degree _ = 1
  div = _divBigNumber
  mod = _modBigNumber

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

-- | Exponentiate a `BigNumber`
foreign import pow :: BigNumber -> Int -> BigNumber

-- Can throw error "Error: Number can only safely store up to 53 bits"
-- TODO(srghma): rename to unsafeToInt (because of https://github.com/throughnothing/purescript-bignum/blob/b9cffc4aa8a5d3e4b688ae34d4ccb01b701e1586/src/Data/BigNum.purs#L28)
foreign import toNumber :: BigNumber -> Number


-- | Unsafely coerce a BigNumber to an Int.
unsafeToInt :: BigNumber -> Int
unsafeToInt = Int.floor <<< toNumber -- TODO(srghma): why floor is needed, BigNumber already cannot be floating number

-- | Take the integer part of a big number
foreign import floorBigNumber :: BigNumber -> BigNumber

-- | Take the twos complement of a `BigNumber`
foreign import bigNumberToTwosComplementInt256 :: BigNumber -> BigNumber

-- | Take the twos complement of a `BigNumber`
foreign import twosComplementInt256ToBigNumber :: BigNumber -> BigNumber

_encode :: BigNumber -> String
_encode = (append "0x") <<< toString Int.hexadecimal

-- Only accepts values with 0x prepended
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

-- `new BN('deed')` to `0xdeed`
instance encodeBigNumber :: Encode BigNumber where
  encode = encode <<< _encode

instance decodeJsonBigNumber :: A.DecodeJson BigNumber where
  decodeJson json = do
    str <- A.decodeJson json
    either (const <<< Left $ UnexpectedValue json) Right $ _decode str

instance encodeJsonBigNumber :: A.EncodeJson BigNumber where
  encodeJson = A.encodeJson <<< _encode
