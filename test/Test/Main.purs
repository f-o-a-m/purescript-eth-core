module Test.Main where

import Prelude

import CoreSpec.BigNumber (bigNumberSpec)
import CoreSpec.Hex (hexSpec)
import CoreSpec.Keccak256 (keccak256Spec)
import CoreSpec.RLP (rlpSpec)
import CoreSpec.Signatures (signatureSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $
  runSpec [consoleReporter] $ do
    keccak256Spec
    hexSpec
    bigNumberSpec
    rlpSpec
    signatureSpec
