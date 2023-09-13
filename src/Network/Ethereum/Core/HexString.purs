module Network.Ethereum.Core.HexString
  ( HexString
  , PadByte(..)
  , mkHexString
  , unHex
  , numberOfBytes
  , dropBytes
  , takeBytes
  , splitAtByteOffset
  , nullWord
  , padLeft
  , padRight
  , toString
  , fromString
  , toUtf8
  , fromUtf8
  , toAscii
  , fromAscii
  , toByteString
  , fromByteString
  , genBytes
  ) where

import Prelude

import Data.Argonaut as A
import Data.Array (fold, fromFoldable, replicate, unsafeIndex)
import Data.Array.NonEmpty as NEA
import Data.ByteString as BS
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (fromFoldable, member) as Set
import Data.String (Pattern(..), split, stripPrefix)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import Foreign (ForeignError(..), fail)
import Node.Encoding (Encoding(Hex, UTF8, ASCII))
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

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
    n <- Gen.chooseInt 0 50
    genBytes n

toString :: HexString -> String
toString = append "0x" <<< unHex

fromString :: String -> Either String HexString
fromString str = case mkHexString str of
  Just res -> Right res
  Nothing -> Left $ "Failed to parse as HexString: " <> str

instance ReadForeign HexString where
  readImpl f = do
    str <- readImpl f
    either (fail <<< ForeignError) pure $ fromString str

instance WriteForeign HexString where
  writeImpl = writeImpl <<< toString

instance A.DecodeJson HexString where
  decodeJson json = do
    str <- A.decodeJson json
    either (const <<< Left $ A.UnexpectedValue json) Right $ fromString str

instance A.EncodeJson HexString where
  encodeJson = A.encodeJson <<< toString

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

splitAtByteOffset :: Int -> HexString -> { before :: HexString, after :: HexString }
splitAtByteOffset n (HexString hx) =
  let
    { before, after } = S.splitAt (2 * n) hx
  in
    { before: HexString before, after: HexString after }

nullWord :: HexString
nullWord = HexString "0000000000000000000000000000000000000000000000000000000000000000"

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

data PadByte = Zero | FF

-- | Pad a `HexString` on the left with '0's until it has length == 0 mod 64.
padLeft :: PadByte -> HexString -> HexString
padLeft b hx =
  let
    padLength = getPadLength $ numberOfBytes hx
    padding = fold $ replicate padLength case b of
      Zero -> HexString "00"
      FF -> HexString "ff"
  in
    padding <> hx

-- | Pad a `HexString` on the right with 0's until it has length 0 mod 64.
padRight :: PadByte -> HexString -> HexString
padRight b hx =
  let
    padLength = getPadLength $ numberOfBytes hx
    padding = fold $ replicate padLength case b of
      Zero -> HexString "00"
      FF -> HexString "ff"
  in
    hx <> padding