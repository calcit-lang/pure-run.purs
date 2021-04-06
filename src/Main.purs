
module Main where

import Calcit.Runner (runCalcit)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard)

foreign import argv :: Array String

main :: Effect Unit
main = do
  case argv !! 2 of
    Nothing -> do
      log "defaults to compact.cirru"
      runCalcit "compact.cirru"
    Just path -> runCalcit path
