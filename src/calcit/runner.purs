module Calcit.Runner where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Either

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Cirru.Edn (parseCirruEdn)

import Calcit.Primes
import Calcit.Snapshot (Snapshot, loadSnapshotData)

runCalcit :: String -> Effect Unit
runCalcit filepath = do
  content <- readTextFile UTF8 filepath
  case parseCirruEdn content of
    Right code -> do
      log $ "EDN data: " <> (show code)
      let snapshot = loadSnapshotData code
      log ""
      log $ "Snapshot: " <> (show snapshot)
    Left nodes ->
      log $ "Failed to parse" <> (show nodes)
