module Calcit.Runner where

import Prelude (Unit, bind, discard, pure, show, ($), (<>))

import Effect (Effect)
import Effect.Console (log)

import Data.Either (Either(..))

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Cirru.Edn (CirruEdn, parseCirruEdn)

import Calcit.Primes (CalcitData(..), CalcitScope)
import Calcit.Snapshot (loadSnapshotData)
import Calcit.Program (extractProgramData)

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
      case snapshot of
        Right s -> do
          log ""
          log $ "Snapshot: " <> (show s)
          case extractProgramData s of
            Right a -> log $ "Program data: " <> (show a)
            Left x -> log $ "Failed" <> (show x)
        Left x -> do
          log $ "EDN Failure" <> (show x)
    Left nodes ->
      log $ "Failed to parse" <> (show nodes)
