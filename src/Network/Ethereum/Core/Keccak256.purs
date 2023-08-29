module Network.Ethereum.Core.Keccak256
  ( class Keccak256
  , keccak256
  , toSelector
  ) where

import Prelude

import Data.ByteString (ByteString, fromString)
import Data.Maybe (fromJust)
import Network.Ethereum.Core.HexString (HexString, fromByteString, takeBytes, toByteString)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------

-- | A class for things which you can hash. Mostly used as a utility for calculating selectors and
-- | event topics
class Keccak256 a where
  keccak256 :: a -> ByteString

foreign import _keccak256 :: ByteString -> ByteString

instance Keccak256 ByteString where
  keccak256 = _keccak256

instance Keccak256 String where
  keccak256 = keccak256 <<< unsafePartial fromJust <<< flip fromString UTF8

instance Keccak256 HexString where
  keccak256 = keccak256 <<< toByteString

-- | convert a string representing a type signature into a selector
toSelector :: String -> HexString
toSelector = takeBytes 4 <<< fromByteString <<< keccak256
