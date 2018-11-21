module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import CoreSpec.BigNumber (bigNumberSpec)
import CoreSpec.Hex (hexSpec)
import CoreSpec.Keccak256 (keccak256Spec)
import CoreSpec.RLP (rlpSpec)
import CoreSpec.Signatures (signatureSpec)

main :: Effect Unit
main = run [consoleReporter] $ do
  keccak256Spec
  hexSpec
  bigNumberSpec
  rlpSpec
  signatureSpec
