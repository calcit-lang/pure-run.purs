module Calcit.Runner where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception

import Data.Either
import Data.Map
import Data.Map as Map
import Data.Tuple

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Cirru.Edn (CirruEdn(..), parseCirruEdn)

import Calcit.Primes
import Calcit.Snapshot (Snapshot, loadSnapshotData)

import Calcit.Program

evaluateEdn :: CirruEdn -> CalcitScope -> Effect CalcitData
evaluateEdn xs s = do
  log "TODO"
  pure CalcitNil

runCalcit :: String -> Effect Unit
runCalcit filepath = do
  content <- readTextFile UTF8 filepath
  case parseCirruEdn content of
    Right code -> do
      -- log $ "EDN data: " <> (show code)
      let snapshot = loadSnapshotData code
      log ""
      log $ "Snapshot: " <> (show snapshot)
    Left nodes ->
      log $ "Failed to parse" <> (show nodes)
