module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import UtilsSpec.BigNumber (bigNumberSpec)
import UtilsSpec.Keccak256 (keccak256Spec)
import UtilsSpec.Hex (hexSpec)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  keccak256Spec
  hexSpec
  bigNumberSpec
