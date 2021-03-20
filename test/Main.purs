module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Calcit.Main

main :: Effect Unit
main = do
  runCalcit
  log "You should add some tests."
