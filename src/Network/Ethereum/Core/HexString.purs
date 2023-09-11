module Network.Ethereum.Core.HexString
  ( HexString
  , Signed(..)
  , Sign(..)
  , mkHexString
  , unHex
  , numberOfBytes
  , dropBytes
  , takeBytes
  , nullWord
  , getPadLength
  , padLeft
  , padLeftSigned
  , padRight
  , padRightSigned
  , toUtf8
  , fromUtf8
  , toAscii
  , fromAscii
  , toByteString
  , fromByteString
  , genBytes
  , parseByte
  , parseBytes
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.Array (fold, fromFoldable, replicate, unsafeIndex)
import Data.Array.NonEmpty as NEA
import Data.ByteString (ByteString, toString, fromString) as BS
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (fromFoldable, member) as Set
import Data.String (Pattern(..), split, splitAt, stripPrefix)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Foreign (ForeignError(..), fail)
import Node.Encoding (Encoding(Hex, UTF8, ASCII))
import Parsing (ParseState(..), ParserT, Position(..), getParserT, stateParserT)
import Parsing as ParserT
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

--------------------------------------------------------------------------------
-- * Signed Values
--------------------------------------------------------------------------------

data Sign = Pos | Neg

derive instance Eq Sign

-- | Represents values that can be either positive or negative.
data Signed a = Signed Sign a

instance Eq a => Eq (Signed a) where
  eq (Signed s a) (Signed s' a') = (s `eq` s') && (a `eq` a')

instance Functor Signed where
  map f (Signed s a) = Signed s (f a)

-- | Coerce a value into a positive signed value
asSigned :: forall a. a -> Signed a
asSigned a = Signed Pos a

--------------------------------------------------------------------------------
-- * HexString
--------------------------------------------------------------------------------

-- | Represents a base16, utf8 encoded bytestring
newtype HexString = HexString String

instance Show HexString where
  show (HexString s) = "0x" <> s

derive newtype instance Eq HexString
derive newtype instance Ord HexString
derive newtype instance Semigroup HexString
derive newtype instance Monoid HexString

genBytes :: Int -> Gen HexString
genBytes n = fold <$> replicateA n genByte
  where
  genByte :: Gen HexString
  genByte = do
    cs <- Gen.listOf 2 (Gen.oneOf (pure <$> hexAlph))
    pure $ HexString $ fromCharArray (fromFoldable cs)
    where
    hexAlph = NEA.fromNonEmpty $ NonEmpty '0' (toCharArray "123456789abcdefABCDEF")

instance Arbitrary HexString where
  arbitrary = do
    n <- Gen.chooseInt 0 100
    genBytes n

_encode :: HexString -> String
_encode = append "0x" <<< unHex

_decode :: String -> Either String HexString
_decode str = case mkHexString str of
  Just res -> Right res
  Nothing -> Left $ "Failed to parse as HexString: " <> str

instance ReadForeign HexString where
  readImpl f = do
    str <- readImpl f
    either (fail <<< ForeignError) pure $ _decode str

instance WriteForeign HexString where
  writeImpl = writeImpl <<< _encode

instance A.DecodeJson HexString where
  decodeJson json = do
    str <- A.decodeJson json
    either (const <<< Left $ UnexpectedValue json) Right $ _decode str

instance A.EncodeJson HexString where
  encodeJson = A.encodeJson <<< _encode

unHex :: HexString -> String
unHex (HexString hx) = hx

mkHexString :: String -> Maybe HexString
mkHexString str
  | str == "" = Just $ HexString ""
  | otherwise =
      HexString <$>
        let
          a = mkEven case stripPrefix (Pattern "0x") str of
            Nothing -> str
            Just str' -> str'
        in
          fromCharArray <$> (traverse parseHexChar $ toCharArray $ a)
      where
      mkEven x = if S.length x `mod` 2 == 0 then x else "0" <> x
      hexAlph = Set.fromFoldable <<< toCharArray $ "0123456789abcdefABCDEF"
      parseHexChar a = if a `Set.member` hexAlph then pure a else Nothing

numberOfBytes :: HexString -> Int
numberOfBytes (HexString hx) = S.length hx `div` 2

takeBytes :: Int -> HexString -> HexString
takeBytes n (HexString hx) = HexString $ S.take (2 * n) hx

dropBytes :: Int -> HexString -> HexString
dropBytes n (HexString hx) = HexString $ S.drop (2 * n) hx

nullWord :: HexString
nullWord = HexString "0000000000000000000000000000000000000000000000000000000000000000"

-- | Read any number of HexDigits
parseBytes :: forall m. Monad m => Int -> ParserT HexString m HexString
parseBytes n = fold <$> replicateA n parseByte

parseByte :: forall m. Monad m => ParserT HexString m HexString
parseByte = do
  ParseState input@(HexString str) (Position position) _ <- getParserT
  if numberOfBytes input < 1 then
    ParserT.fail "Unexpected EOF"
  else do
    let
      { after, before } = splitAt 2 str
      position' = Position $ position { column = position.column + 1 }
      newState = ParseState (HexString after) position' true
      ret = HexString before
    stateParserT $ const (Tuple ret newState)

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

-- | Takes a hex string and produces the corresponding UTF8-decoded string.
-- | This breaks at the first null octet, following the web3 function `toUft8`.
--   Since 'split' always returns a nonempty list, this index is actually safe.
toUtf8 :: HexString -> String
toUtf8 hx = flip BS.toString UTF8 $ bs (unHex hx)
  where
  bs :: String -> BS.ByteString
  bs hxstr = unsafePartial fromJust $ BS.fromString hxstr Hex

-- | Takes a hex string and produces the corresponding ASCII decoded string.
toAscii :: HexString -> String
toAscii hx = flip BS.toString ASCII $ unsafePartial $ fromJust $ BS.fromString (unHex hx) Hex

-- | Get the 'HexString' corresponding to the UTF8 encoding.
fromUtf8 :: String -> HexString
fromUtf8 s = unsafePartial fromJust $
  let
    s' = unsafePartial $ split (Pattern "\x0000") s `unsafeIndex` 0
  in
    BS.fromString s' UTF8 >>= (pure <<< flip BS.toString Hex) >>= mkHexString

-- | Get the 'HexString' corresponding to the ASCII encoding.
fromAscii :: String -> HexString
fromAscii s = unsafePartial fromJust $
  BS.fromString s ASCII >>= (pure <<< flip BS.toString Hex) >>= mkHexString

toByteString :: HexString -> BS.ByteString
toByteString hx = unsafePartial fromJust (BS.fromString (unHex hx) Hex)

fromByteString :: BS.ByteString -> HexString
fromByteString bs = unsafePartial fromJust $ mkHexString (BS.toString bs Hex)

--------------------------------------------------------------------------------

-- | Computes the number of 0s needed to pad a bytestring of the input length
getPadLength :: Int -> Int
getPadLength len =
  let
    n = len `mod` 32
  in
    if n == 0 then 0 else 32 - n

-- | Pad a `Signed HexString` on the left until it has length == 0 mod 64.
padLeftSigned :: Signed HexString -> HexString
padLeftSigned (Signed s hx) =
  let
    padLength = getPadLength $ numberOfBytes hx
    sgn = if s == Pos then (HexString "00") else (HexString "ff")
    padding = fold $ replicate padLength $ sgn
  in
    padding <> hx

-- | Pad a `Signed HexString` on the right until it has length 0 mod 64.
padRightSigned :: Signed HexString -> HexString
padRightSigned (Signed s hx) =
  let
    padLength = getPadLength $ numberOfBytes hx
    sgn = if s `eq` Pos then '0' else 'f'
    padding = unsafePartial fromJust <<< mkHexString <<< fromCharArray <<< replicate (2 * padLength) $ sgn
  in
    hx <> padding

-- | Pad a `HexString` on the left with '0's until it has length == 0 mod 64.
padLeft :: HexString -> HexString
padLeft = padLeftSigned <<< asSigned

-- | Pad a `HexString` on the right with 0's until it has length 0 mod 64.
padRight :: HexString -> HexString
padRight = padRightSigned <<< asSigned
