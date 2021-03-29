module Calcit.Runner where

import Prelude (Unit, bind, discard, pure, show, ($), (<>))

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Array ((!!))

import Data.Either (Either(..))
import Data.Map as Map
import Effect.Exception (throw)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Traversable (traverse)

import Cirru.Edn (parseCirruEdn)

import Calcit.Primes (CalcitData(..), CalcitScope, emptyScope)
import Calcit.Snapshot (loadSnapshotData)
import Calcit.Program (extractProgramData, ProgramCodeData, lookupDef, lookupEvaledDef, writeEvaledDef)
import Calcit.Builtin (coreNsDefs)

evaluateExpr :: CalcitData -> CalcitScope -> String -> ProgramCodeData -> Effect CalcitData
evaluateExpr xs scope ns programData = case xs of
  CalcitNil -> pure xs
  CalcitBool _ -> pure xs
  CalcitNumber n -> pure (CalcitNumber n)
  CalcitSymbol s -> case Map.lookup s coreNsDefs of
    Just v -> pure v
    Nothing -> case Map.lookup s scope of
      Just v -> pure v
      Nothing -> do
        v <- lookupEvaledDef ns s
        case v of
          Just defData -> pure defData
          Nothing -> case lookupDef ns s programData of
            Nothing -> throw $ "Unknown operator: " <> ns <> "/" <> s
            Just code -> do
              newV <- evaluateExpr code emptyScope ns programData
              writeEvaledDef ns s newV
              pure newV
  CalcitKeyword _ -> pure xs
  CalcitString _ -> pure xs
  CalcitFn _ _ -> pure xs
  CalcitSyntax _ _ -> pure xs
  CalcitList ys -> case ys !! 0 of
    Nothing -> throw "cannot eval empty list"
    Just op -> do
      -- log $ "Eval expression: " <> (show ys)
      v <- evaluateExpr op scope ns programData
      case v of
        CalcitFn _ f -> do
          args <- traverse (\x -> evaluateExpr x scope ns programData) (Array.drop 1 ys)
          f args
        CalcitSyntax _ f -> f (Array.drop 1 ys) scope evalFn
          where
            evalFn zs s2 = evaluateExpr zs s2 ns programData
        CalcitSymbol s -> throw "cannot use symbol as function"
        _ -> throw "Unknown type of operation"
  _ -> throw $ "Unexpected structure: " <> (show xs)

runCalcit :: String -> Effect Unit
runCalcit filepath = do
  content <- readTextFile UTF8 filepath
  case parseCirruEdn content of
    Left nodes -> log $ "Failed to parse" <> (show nodes)
    Right code -> do
      -- log $ "EDN data: " <> (show code)
      let snapshot = loadSnapshotData code
      log $ "Snapshot:" <> (show snapshot)
      case snapshot of
        Left x -> log $ "EDN Failure" <> (show x)
        Right s -> do
          case extractProgramData s of
            Left x -> do
              log $ "Failed" <> (show x)

            Right programData -> do
              log $ "Program data: " <> (show programData)
              case lookupDef "app.main" "main!" programData of
                Nothing -> log "no main function"
                Just xs -> do
                  v <- do
                    -- log $ "\nEval: " <> (show xs)
                    evaluateExpr xs emptyScope "app.main" programData
                  case v of
                    CalcitFn name f -> do
                      result <- f [v]
                      log $ "Return value: " <> (show result)
                    _ -> throw "Expected function entry"
