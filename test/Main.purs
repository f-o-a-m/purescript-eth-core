module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import UtilsSpec.Types.BigNumber (bigNumberSpec)
import UtilsSpec.Keccak256 (keccak256Spec)
import UtilsSpec.Utils (utilsSpec)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  keccak256Spec
  utilsSpec
  bigNumberSpec
