module Main where

import Calcit.Runner (runCalcit)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)
import Prelude (Unit, bind, discard)

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Nothing -> do
      log "defaults to compact.cirru"
      runCalcit "compact.cirru"
    Just path -> runCalcit path
