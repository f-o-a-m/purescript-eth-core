module Test.Main where

import Prelude

import CoreSpec.BigNumber (bigNumberSpec)
import CoreSpec.Hex (hexSpec)
import CoreSpec.Keccak256 (keccak256Spec)
import CoreSpec.RLP (rlpSpec)
import CoreSpec.Signatures (signatureSpec)
import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ]
  $ do
      keccak256Spec
      hexSpec
      bigNumberSpec
      rlpSpec
      signatureSpec
