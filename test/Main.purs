module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import CoreSpec.BigNumber (bigNumberSpec)
import CoreSpec.Hex (hexSpec)
import CoreSpec.Keccak256 (keccak256Spec)
import CoreSpec.RLP (rlpSpec)
import CoreSpec.Signatures (signatureSpec)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  keccak256Spec
  hexSpec
  bigNumberSpec
  rlpSpec
  signatureSpec
