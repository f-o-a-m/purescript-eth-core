module Network.Ethereum.Utils.HexString
  ( Sign(..)
  , Signed(..)
  , asSigned
  , HexString
  , mkHexString
  , unHex
  , hexLength
  , takeHex
  , nullWord
  , Address
  , unAddress
  , mkAddress
  ) where

import Prelude

import Data.Array (uncons)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (class Monoid)
import Data.Set (fromFoldable, member) as Set
import Data.String (Pattern(..), stripPrefix, toCharArray)
import Data.String as S
import Simple.JSON (class ReadForeign)

--------------------------------------------------------------------------------
-- * Signed Values
--------------------------------------------------------------------------------

data Sign = Pos | Neg

derive instance eqSign :: Eq Sign

-- | Represents values that can be either positive or negative.
data Signed a = Signed Sign a

instance showSigned :: Show a => Show (Signed a) where
  show (Signed s a) = s' <> show a
    where
      s' = case s of
        Pos -> ""
        Neg -> "-"

instance eqSigned :: Eq a => Eq (Signed a) where
  eq (Signed s a) (Signed s' a') = (s `eq` s') && (a `eq` a')

instance mapSigned :: Functor Signed where
  map f (Signed s a) = Signed s (f a)

-- | Coerce a value into a positive signed value
asSigned :: forall a . a -> Signed a
asSigned a = Signed Pos a

--------------------------------------------------------------------------------
-- * HexString
--------------------------------------------------------------------------------

-- | Represents a base16, utf8 encoded bytestring
newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString hx) = "0x" <> hx

instance hexStringEq :: Eq HexString where
  eq (HexString h1) (HexString h2) = S.toLower h1 == S.toLower h2

derive newtype instance hexStringOrd :: Ord HexString
derive newtype instance semigpStringEq :: Semigroup HexString
derive newtype instance monoidStringEq :: Monoid HexString

instance decodeHexString :: Decode HexString where
  decode s = do
    str <- decode s
    case stripPrefix (Pattern "0x") str of
      Nothing -> pure <<< HexString $ str
      Just res -> pure <<< HexString $ res

instance readFHexString :: ReadForeign HexString where
  readImpl = decode

instance encodeHexString :: Encode HexString where
  encode = encode <<< append "0x" <<< unHex

unHex :: HexString -> String
unHex (HexString hx) = hx

mkHexString :: String -> Maybe HexString
mkHexString str | S.length str `mod` 2 /= 0 = Nothing
                | otherwise =
    HexString <$>
      case stripPrefix (Pattern "0x") str of
        Nothing -> if isJust (go <<< toCharArray $ str)
                    then Just $ S.toLower str
                    else Nothing
        Just res -> if isJust (go <<< toCharArray $ res)
                      then Just $ S.toLower res
                      else Nothing
      where
        hexAlph = Set.fromFoldable <<< toCharArray $ "0123456789abcdefABCDEF"
        go s = case uncons s of
          Nothing -> pure unit
          Just {head, tail} ->
            if head `Set.member` hexAlph
              then go tail
              else Nothing

-- | Compute the length of the hex string, which is twice the number of bytes it represents
hexLength :: HexString -> Int
hexLength (HexString hx) = S.length hx

takeHex :: Int -> HexString -> HexString
takeHex n (HexString hx) = HexString $ S.take n hx

nullWord :: HexString
nullWord = HexString "0000000000000000000000000000000000000000000000000000000000000000"

--------------------------------------------------------------------------------
-- * Addresses
--------------------------------------------------------------------------------

-- | Represents and Ethereum address, which is a 20 byte `HexString`
newtype Address = Address HexString

derive newtype instance addressShow :: Show Address
derive newtype instance addressEq :: Eq Address
derive newtype instance addressOrd :: Ord Address
derive newtype instance decodeAddress :: Decode Address
derive newtype instance encodeAddress :: Encode Address

unAddress :: Address -> HexString
unAddress (Address a) = a

mkAddress :: HexString -> Maybe Address
mkAddress hx = if hexLength hx == 40 then Just <<< Address $ hx else Nothing
