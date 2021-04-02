module Calcit.Runner where

import Calcit.Builtin.Bool (fnNativeAnd, fnNativeNot, fnNativeOr)
import Calcit.Builtin.Effect (fnNativeEcho, fnNativeRaise)
import Calcit.Builtin.HashMap (fnNativeHashMap)
import Calcit.Builtin.List (fnNativeConcat, fnNativeCount, fnNativeFoldl, fnNativeList, fnNativeMap, fnNativeNth, fnNativeSlice)
import Calcit.Builtin.Number (fnNativeAdd, fnNativeEq, fnNativeGt, fnNativeLt, fnNativeMinus)
import Calcit.Builtin.Symbol (fnNativeGensym, fnNativeResetGensymIndex)
import Calcit.Builtin.Syntax (coreNsSyntaxes)
import Calcit.Primes (CalcitData(..), CalcitScope, coreNs, emptyScope)
import Calcit.Program (ProgramCodeData, extractProgramData, lookupDef, lookupDefTargetInImport, lookupEvaledDef, lookupNsTargetInImport, writeEvaledDef)
import Calcit.Snapshot (Snapshot, loadSnapshotData)
import Cirru.Edn (parseCirruEdn)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, split)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile)
import Node.Globals (__dirname)
import Node.Path (concat)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (&&), (-), (<), (<>), (>), (>=))


evaluateNewDef :: CalcitData -> CalcitScope -> String -> String -> ProgramCodeData -> Effect CalcitData
evaluateNewDef xs scope ns def programData = do
  newV <- evaluateExpr xs emptyScope ns programData
  writeEvaledDef ns def newV
  pure newV


evalSymbolFromProgram :: String -> CalcitScope -> String -> ProgramCodeData -> Effect CalcitData
evalSymbolFromProgram s scope symbolNs programData = do
  -- log $ "handling: " <> s <> " " <> symbolNs
  v <- lookupEvaledDef symbolNs s
  case v of
    Just defData -> pure defData
    Nothing -> case lookupDef symbolNs s programData of
      Just code -> evaluateNewDef code emptyScope symbolNs s programData
      Nothing -> throw $ "Unknown operator: " <> symbolNs <> "/" <> s

parseManualNs :: String -> Maybe (Tuple String String)
parseManualNs s = do
  idx <- String.indexOf (Pattern "/") s
  let size = String.length s
  if size >= 3 && idx > 0 && idx < (size - 1)
    then case String.split (Pattern "/") s of
      [ns, def] -> Just (Tuple ns def)
      _ -> Nothing
    else Nothing

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.union coreNsSyntaxes coreDefs
  where
    coreDefs = Map.fromFoldable [
      (Tuple "&+" (CalcitFn "&+" fnNativeAdd))
    , (Tuple "&-" (CalcitFn "&-" fnNativeMinus))
    , (Tuple "&<" (CalcitFn "&<" fnNativeLt))
    , (Tuple "&>" (CalcitFn "&>" fnNativeGt))
    , (Tuple "&=" (CalcitFn "&=" fnNativeEq))
    , (Tuple "echo" (CalcitFn "echo" fnNativeEcho))
    , (Tuple "[]" (CalcitFn "[]" fnNativeList))
    , (Tuple "nth" (CalcitFn "[]" fnNativeNth))
    , (Tuple "count" (CalcitFn "count" fnNativeCount))
    , (Tuple "slice" (CalcitFn "slice" fnNativeSlice))
    , (Tuple "foldl" (CalcitFn "foldl" fnNativeFoldl))
    , (Tuple "map" (CalcitFn "map" fnNativeMap))
    , (Tuple "concat" (CalcitFn "concat" fnNativeConcat))
    , (Tuple "raise" (CalcitFn "raise" fnNativeRaise))
    , (Tuple "gensym" (CalcitFn "gensym" fnNativeGensym))
    , (Tuple "reset-gensym-index!" (CalcitFn "reset-gensym-index!" fnNativeResetGensymIndex))
    , (Tuple "&and" (CalcitFn "&and" fnNativeAnd))
    , (Tuple "&or" (CalcitFn "&or" fnNativeOr))
    , (Tuple "not" (CalcitFn "not" fnNativeNot))
    , (Tuple "&{}" (CalcitFn "&{}" fnNativeHashMap))
    ]

evaluateExpr :: CalcitData -> CalcitScope -> String -> ProgramCodeData -> Effect CalcitData
evaluateExpr xs scope ns programData = case xs of
  CalcitNil -> pure xs
  CalcitBool _ -> pure xs
  CalcitNumber n -> pure (CalcitNumber n)
  CalcitSymbol "&" _ -> pure xs -- special syntax does eval
  CalcitSymbol s symbolNs -> case parseManualNs s of
    Just (Tuple nsAlias def) -> case lookupNsTargetInImport ns nsAlias programData of
      Just target -> evalSymbolFromProgram def scope target programData
      Nothing -> throw $ "cannot find target " <> s
    Nothing -> case Map.lookup s coreNsDefs of
      Just v -> pure v
      Nothing -> case lookupDef coreNs s programData of
        Just code -> evaluateNewDef code emptyScope coreNs s programData
        Nothing -> case Map.lookup s scope of
          Just v -> pure v
          Nothing -> case lookupDefTargetInImport symbolNs s programData of
            Just target -> do
              -- log $ "from imported ns: " <> target <> " " <> (show programData)
              evalSymbolFromProgram s scope target programData
            Nothing -> do
              -- log $ "from local ns:" <> s
              evalSymbolFromProgram s scope symbolNs programData
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
        CalcitMacro _ f -> do
          expr <- f (Array.drop 1 ys)
          case expr of
            CalcitNil -> pure CalcitNil
            CalcitBool _ -> pure expr
            CalcitNumber _ -> pure expr
            CalcitString _ -> pure expr
            CalcitKeyword _ -> pure expr
            CalcitSymbol s symbolNs -> evaluateExpr expr scope ns programData
            CalcitList code -> do
              -- log $ "the code:" <> show code
              evaluateExpr expr scope ns programData
            _ -> throw "unknown data from defmacro"

        CalcitFn _ f -> do
          args <- traverse (\x -> evaluateExpr x scope ns programData) (Array.drop 1 ys)
          spreadedArgs <- spreadArgs args []
          f spreadedArgs
        CalcitSyntax _ f -> f (Array.drop 1 ys) scope evalFn
          where
            evalFn zs s2 = evaluateExpr zs s2 ns programData
        CalcitSymbol s _ -> throw "cannot use symbol as function"
        _ -> throw "Unknown type of operation"
  _ -> throw $ "Unexpected structure: " <> (show xs)

spreadArgs :: Array CalcitData -> Array CalcitData -> Effect (Array CalcitData)
spreadArgs xs acc = case (xs !! 0), (xs !! 1) of
  Nothing, _ -> pure acc
  Just (CalcitSymbol "&" _), Just (CalcitList ys) -> pure (Array.concat [acc, ys])
  Just (CalcitSymbol "&" _), Just a -> throw $ "cannot spread: " <> (show a)
  Just (CalcitSymbol "&" _), Nothing -> throw "nothing to spread"
  Just x, _ -> spreadArgs (Array.drop 1 xs) (Array.concat [acc, [x]])

loadSnapshotFile :: String -> Effect Snapshot
loadSnapshotFile filepath = do
  hasFile <- exists filepath
  if hasFile then pure unit else throw $ filepath <> " does not exist"
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
coreFilepath = concat [__dirname,
  -- dirty hack since bundled js may have different paths
  if contains (Pattern "pure-run.purs/output") __dirname
  then "../../src/includes/calcit-core.cirru"
  else "../src/includes/calcit-core.cirru"
]

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
  log $ "loading core" <> __dirname <> " " <> coreFilepath
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
