module Calcit.Runner where

import Calcit.Builtin (coreNsDefs)
import Calcit.Primes (CalcitData(..), CalcitScope, coreNs, emptyScope)
import Calcit.Program (ProgramCodeData, extractProgramData, lookupDef, lookupEvaledDef, writeEvaledDef)
import Calcit.Snapshot (Snapshot, loadSnapshotData)
import Cirru.Edn (parseCirruEdn)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, discard, pure, show, ($), (<>))

evaluateNewDef :: CalcitData -> CalcitScope -> String -> String -> ProgramCodeData -> Effect CalcitData
evaluateNewDef xs scope ns def programData = do
  newV <- evaluateExpr xs emptyScope ns programData
  writeEvaledDef ns def newV
  pure newV

evaluateExpr :: CalcitData -> CalcitScope -> String -> ProgramCodeData -> Effect CalcitData
evaluateExpr xs scope ns programData = case xs of
  CalcitNil -> pure xs
  CalcitBool _ -> pure xs
  CalcitNumber n -> pure (CalcitNumber n)
  CalcitSymbol s symbolNs -> case Map.lookup s coreNsDefs of
    Just v -> pure v
    Nothing -> case Map.lookup s scope of
      Just v -> pure v
      Nothing -> do
        v <- lookupEvaledDef ns s
        case v of
          Just defData -> pure defData
          Nothing -> case lookupDef coreNs s programData of
            Just code -> evaluateNewDef code emptyScope coreNs s programData
            Nothing -> case lookupDef symbolNs s programData of
              Just code -> evaluateNewDef code emptyScope symbolNs s programData
              Nothing -> throw $ "Unknown operator: " <> symbolNs <> "/" <> s
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
        CalcitSymbol s _ -> throw "cannot use symbol as function"
        _ -> throw "Unknown type of operation"
  _ -> throw $ "Unexpected structure: " <> (show xs)

loadSnapshotFile :: String -> Effect Snapshot
loadSnapshotFile filepath = do
  content <- readTextFile UTF8 filepath
  case parseCirruEdn content of
    Left nodes -> throw $ "failed to parse edn" <> filepath
    Right codeTree -> do
      let snapshot = loadSnapshotData codeTree
      -- log $ "Snapshot: " <> (show snapshot)
      case snapshot of
        Left s -> throw $ "failed to parse snapshot" <> (show snapshot) <> (show s)
        Right s -> pure s

loadCompactFile :: Snapshot -> Effect ProgramCodeData
loadCompactFile snapshot = do
  case extractProgramData snapshot of
    Left x -> do
      throw $ "failed to extract program: " <> (show x)
    Right v -> do
      pure v

coreFilepath :: String
coreFilepath = "./src/includes/calcit-core.cirru"

extractNsDef :: String -> Effect { ns :: String, def :: String }
extractNsDef s = case split (Pattern "/") s of
    [ns, def] -> pure { ns: ns, def: def }
    _ -> throw "failed to extract ns/def"

runCalcit :: String -> Effect Unit
runCalcit filepath = do
  programSnapshot <- loadSnapshotFile filepath
  programData <- case extractProgramData programSnapshot of
    Left reason -> throw $ "Failed to extract program" <> (show reason)
    Right v -> pure v
  coreSnapshot <- loadSnapshotFile coreFilepath
  coreData <- case extractProgramData coreSnapshot of
    Left reason -> throw $ "Failed to extract core" <> (show reason)
    Right v -> pure v
  initConfig <- extractNsDef programSnapshot.configs.initFn

  let runtimeData = Map.union programData coreData
  -- log $ "\nProgram data: " <> (show runtimeData)
  -- log $  "init fn: " <> initConfig.ns <> "/" <> initConfig.def

  case lookupDef initConfig.ns initConfig.def runtimeData of
    Nothing -> throw "no main function"
    Just xs -> do
      v <- do
        -- log $ "\nEval: " <> (show xs)
        evaluateExpr xs emptyScope initConfig.ns runtimeData
      case v of
        CalcitFn name f -> do
          result <- f [v]
          log $ "Return value: " <> (show result)
        _ -> throw "Expected function entry"
