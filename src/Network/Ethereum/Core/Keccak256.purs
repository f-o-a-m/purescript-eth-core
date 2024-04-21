module Network.Ethereum.Core.Keccak256
  ( class Keccak256
  , keccak256
  , toSelector
  ) where

import Prelude

import Node.Buffer.Immutable (ImmutableBuffer, fromString)
import Network.Ethereum.Core.HexString (HexString, fromBuffer, takeBytes, toBuffer)
import Node.Encoding (Encoding(UTF8))

--------------------------------------------------------------------------------

-- | A class for things which you can hash. Mostly used as a utility for calculating selectors and
-- | event topics
class Keccak256 a where
  keccak256 :: a -> ImmutableBuffer

foreign import _keccak256 :: ImmutableBuffer -> ImmutableBuffer

instance Keccak256 ImmutableBuffer where
  keccak256 = _keccak256

instance Keccak256 String where
  keccak256 s = keccak256 $ fromString s UTF8

instance Keccak256 HexString where
  keccak256 = keccak256 <<< toBuffer

-- | convert a string representing a type signature into a selector
toSelector :: String -> HexString
toSelector = takeBytes 4 <<< fromBuffer <<< keccak256
