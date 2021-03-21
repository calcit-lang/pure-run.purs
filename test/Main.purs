module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Calcit.Runner

main :: Effect Unit
main = do
  runCalcit "./example/compact.cirru"
  log "You should add some tests."
