module Test.Main where

import Prelude

import AwaitsSpec as AwaitsSpec
import Control.Monad.Eff (Eff)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  AwaitsSpec.spec
