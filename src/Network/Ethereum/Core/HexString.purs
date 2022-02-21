module Network.Ethereum.Core.HexString
  ( Sign(..)
  , Signed(..)
  , HexString
  , asSigned
  , mkHexString
  , unHex
  , hexLength
  , dropHex
  , takeHex
  , nullWord
  , getPadLength
  , padLeftSigned
  , padLeft
  , padRightSigned
  , padRight
  , toUtf8
  , fromUtf8
  , toAscii
  , fromAscii
  , toSignedHexString
  , toHexString
  , toBigNumber
  , toBigNumberFromSignedHexString
  , toByteString
  , fromByteString
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.Array (uncons, unsafeIndex, replicate)
import Data.ByteString (ByteString, toString, fromString) as BS
import Data.Either (Either(..), either)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromJust, isJust, fromMaybe)
import Data.Set (fromFoldable, member) as Set
import Data.String (Pattern(..), split, stripPrefix)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Foreign (ForeignError(..), fail)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Network.Ethereum.Core.BigNumber (BigNumber, toString, hexadecimal, twosComplementInt256ToBigNumber)
import Node.Encoding (Encoding(Hex, UTF8, ASCII))
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign)

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

_encode :: HexString -> String
_encode = append "0x" <<< unHex

_decode :: String -> Either String HexString
_decode str = case mkHexString str of
  Just res -> Right res
  Nothing -> Left $ "Failed to parse as HexString: " <> str

instance decodeHexString :: Decode HexString where
  decode s = do
    str <- decode s
    either (fail <<< ForeignError) pure $ _decode str

instance readFHexString :: ReadForeign HexString where
  readImpl = decode

instance writeFHexString :: WriteForeign HexString where
  writeImpl = encode

instance encodeHexString :: Encode HexString where
  encode = encode <<< _encode

instance decodeJsonHexString :: A.DecodeJson HexString where
  decodeJson json = do
    str <- A.decodeJson json
    either (const <<< Left $ UnexpectedValue json) Right $ _decode str

instance encodeJsonHexString :: A.EncodeJson HexString where
  encodeJson = A.encodeJson <<< _encode

unHex :: HexString -> String
unHex (HexString hx) = hx

-- | "00" -> Just "00"
-- | "0x00" -> Just "00"
-- | "ff" -> Just "ff"
-- | "-ff" -> Nothing
-- | "0xff" -> Just "ff"
-- | "0xFF" -> Just "ff"
-- | "" -> Just ""
-- | "0x" -> Just ""
-- | "f" -> Nothing
-- | "0xf" -> Nothing
mkHexString :: String -> Maybe HexString
mkHexString =
  let
    hexAlphhabet = Set.fromFoldable <<< toCharArray $ "0123456789abcdefABCDEF"

    go :: Array Char -> Maybe Unit
    go s = case uncons s of
      Nothing -> pure unit
      Just {head, tail} ->
        if head `Set.member` hexAlphhabet
          then go tail
          else Nothing

    goString :: String -> Maybe String
    goString str = if isJust (go <<< toCharArray $ str)
      then Just $ S.toLower str
      else Nothing
  in \str ->
  let
    str' = fromMaybe str $ stripPrefix (Pattern "0x") str
    length = S.length str'
  in if length `mod` 2 /= 0
    then Nothing
    else HexString <$> goString str'

-- | Compute the length of the hex string, which is twice the number of bytes it represents
hexLength :: HexString -> Int
hexLength (HexString hx) = S.length hx

takeHex :: Int -> HexString -> HexString
takeHex n (HexString hx) = HexString $ S.take n hx

dropHex :: Int -> HexString -> HexString
dropHex n (HexString hx) = HexString $ S.drop n hx

-- 64 characters/hexes, 32 byte value, 256 bit
nullWord :: HexString
nullWord = HexString "0000000000000000000000000000000000000000000000000000000000000000"


--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

-- | Computes the number of 0s needed to pad a bytestring of the input length,
-- | such that the length is 32 bytes (or 64 characters/hexes, 256 bit).
-- | For example used to encode function arguments.
getPadLength :: Int -> Int
getPadLength len =
  let n = len `mod` 64
  in if n == 0 then 0 else 64 - n

getPadding :: Sign -> HexString -> HexString
getPadding s hx =
    let padLength = getPadLength $ hexLength hx
        sgn = if s `eq` Pos then '0' else 'f'
        padding = unsafePartial fromJust <<< mkHexString <<< fromCharArray <<< replicate padLength $ sgn
    in padding

-- | Pad a `Signed HexString` on the left until it has length == 0 mod 64.
padLeftSigned :: Signed HexString -> HexString
padLeftSigned (Signed s hx) = getPadding s hx <> hx

-- | Pad a `Signed HexString` on the right until it has length == 0 mod 64.
padRightSigned :: Signed HexString -> HexString
padRightSigned (Signed s hx) = hx <> getPadding s hx

-- | Pad a `HexString` on the left with '0's until it has length == 0 mod 64.
padLeft :: HexString -> HexString
padLeft = padLeftSigned <<< asSigned


-- | Pad a `HexString` on the right with 0's until it has length 0 mod 64.
padRight :: HexString -> HexString
padRight = padRightSigned <<< asSigned

-- | Takes a hex string and produces the corresponding UTF8-decoded string.
-- | This breaks at the first null octet, following the web3 function `toUft8`.
--   Since 'split' always returns a nonempty list, this index is actually safe.
toUtf8 :: HexString -> String
toUtf8 hx = flip BS.toString UTF8 $ bs (unHex hx)
    where
  bs :: String -> BS.ByteString
  bs hxstr = unsafePartial  fromJust $ BS.fromString hxstr Hex

-- | Takes a hex string and produces the corresponding ASCII decoded string.
toAscii :: HexString -> String
toAscii hx = flip BS.toString ASCII $ unsafePartial $ fromJust $ BS.fromString (unHex hx) Hex

-- | Get the 'HexString' corresponding to the UTF8 encoding.
fromUtf8 :: String -> HexString
fromUtf8 s = unsafePartial fromJust $
  let s' = unsafePartial $ split (Pattern "\x0000") s `unsafeIndex` 0
  in BS.fromString s' UTF8 >>= (pure <<< flip BS.toString Hex) >>=  mkHexString

-- | Get the 'HexString' corresponding to the ASCII encoding.
fromAscii :: String -> HexString
fromAscii s = unsafePartial fromJust $
  BS.fromString s ASCII >>= (pure <<< flip BS.toString Hex) >>= mkHexString

toSignedHexString :: BigNumber -> Signed HexString
toSignedHexString bn =
  let rawStr = toString hexadecimal $ bn
      evenStr = unsafePartial fromJust <<< mkHexString $ if even (S.length rawStr) then rawStr else "0" <> rawStr
      sgn = if bn < zero then Neg else Pos
  in Signed sgn evenStr

toHexString :: BigNumber -> HexString
toHexString bn =
  let Signed _ n = toSignedHexString bn
  in n

foreign import toBigNumber :: HexString -> BigNumber

-- by "signed" we mean NOT that "f0f0f0f0f" can be prepended by minus ('-')
-- but that it can be prepended by zeros (`00000....data`) or ones (`ffffff...data`)
toBigNumberFromSignedHexString :: HexString -> BigNumber
toBigNumberFromSignedHexString = twosComplementInt256ToBigNumber <<< toBigNumber

toByteString :: HexString -> BS.ByteString
toByteString hx = unsafePartial fromJust (BS.fromString (unHex hx) Hex)

fromByteString :: BS.ByteString -> HexString
fromByteString bs = unsafePartial fromJust $ mkHexString (BS.toString bs Hex)
