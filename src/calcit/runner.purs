module Calcit.Runner where

import Prelude (Unit, bind, discard, pure, show, ($), (<>))

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Data.Array as Array
import Data.Array ((!!))

import Data.Either (Either(..))
import Data.Map as Map
import Effect.Exception (throw)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Traversable (traverse)

import Cirru.Edn (parseCirruEdn)

import Calcit.Primes (CalcitData(..), CalcitScope)
import Calcit.Snapshot (loadSnapshotData)
import Calcit.Program (extractProgramData, lookupDef)
import Calcit.Builtin (nativeAdd, nativeEcho, nativeDefn)

evaluateEdn :: CalcitData -> CalcitScope -> String -> Effect CalcitData
evaluateEdn xs scope ns = case xs of
  CalcitNil -> pure xs
  CalcitBool _ -> pure xs
  CalcitNumber n -> pure (CalcitNumber n)
  CalcitSymbol s -> case Map.lookup s scope of
    Nothing -> throw $ "Cannot find variable: " <> s
    Just v -> pure v
  CalcitKeyword _ -> pure xs
  CalcitString _ -> pure xs
  CalcitList ys -> case ys !! 0 of
    Nothing -> throw "cannot eval empty list"
    Just op -> case op of
      CalcitSymbol s -> case s of
        "+" -> do
          args <- traverse (\x -> evaluateEdn x scope ns) (Array.drop 1 ys)
          case nativeAdd args scope of
            Left x -> throw (show x)
            Right x -> pure (x)
        "echo" -> do
          args <- traverse (\x -> evaluateEdn x scope ns) (Array.drop 1 ys)
          nativeEcho args
        "defn" -> case nativeDefn (Array.drop 1 ys) scope of
          Right x -> pure x
          Left x -> throw $ "Failed to construct a function" <> (show x)
        z -> throw $ "Unknown operation: " <> z
      CalcitFn name args body s2 ->
        let
          callLines :: Array CalcitData -> Effect CalcitData
          callLines zs = case zs !! 0 of
            Just z0 -> do
              v <- evaluateEdn z0 s2 ns
              callLines (Array.drop 1 zs)
            Nothing -> pure CalcitNil
        in
          callLines body
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
                  let emptyScope = Map.fromFoldable []
                  v <- evaluateEdn xs emptyScope "app.main"
                  case v of
                    CalcitFn _ _ _ _ -> do
                      result <- evaluateEdn (CalcitList [v]) emptyScope "app.main"
                      log $ "Return value: " <> (show v)
                    _ -> throw "Expected function entry"
