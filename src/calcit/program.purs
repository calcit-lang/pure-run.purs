
module Calcit.Program where

import Data.Unit

import Calcit.Primes (CalcitData(..), CalcitFailure, CalcitScope, cirruToCalcit)
import Calcit.Snapshot (Snapshot)
import Cirru.Node (CirruNode(..), isCirruLeaf)
import Data.Array (length, (!!), all)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Map.Internal as MapInternal
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prelude (bind, pure, (==), (<>))

-- defRule: ns def
data ImportRule = ImportNsRule String | ImportDefRule String String

-- | information extracted from snapshot
type ProgramFileData = {
  importMap :: Map.Map String ImportRule,
  defs :: Map.Map String CalcitData
}

type ProgramCodeData = Map.Map String ProgramFileData

type EvalFn = CalcitData -> CalcitScope -> String -> ProgramCodeData -> Effect CalcitData

instance showImportRule :: Show ImportRule where
  show (ImportNsRule ns) = "(import ns: " <> ns <> ")"
  show (ImportDefRule ns def) = "(import def: " <> ns <> " " <> def <> ")"

-- | real program state
-- ditry https://wiki.haskell.org/Top_level_mutable_state
programEvaledDataRef :: Ref.Ref (Map.Map String (Map.Map String CalcitData))
programEvaledDataRef = unsafePerformEffect (Ref.new (Map.fromFoldable []))

filterLeaf :: CirruNode -> Maybe String
filterLeaf node = case node of
  CirruLeaf x -> Just x
  _ -> Nothing

-- | parse (n.a :as b) and (n.a :refer (b c d))
extractImportRule :: CirruNode -> String -> Either CalcitFailure (Array (Tuple String ImportRule))
extractImportRule node ns = case node of
  CirruLeaf s -> Left { message: "expected import rule in list", data: CalcitSymbol s ns }
  CirruList xs -> if (length xs) == 3
    then case xs !! 1 of
      Just (CirruLeaf ":as") -> case (xs !! 0), (xs !! 2) of
        Just (CirruLeaf target), Just (CirruLeaf alias) -> Right [Tuple alias (ImportNsRule target)]
        _, _ -> Left { message: "invalid :as rule", data: cirruToCalcit node ns }
      Just (CirruLeaf ":refer") -> case (xs !! 0), (xs !! 2) of
        Just (CirruLeaf target), Just (CirruList ys) -> if all isCirruLeaf ys
          then Right (Array.mapMaybe (\alias ->
              Just (Tuple alias (ImportDefRule target alias))
            ) (Array.mapMaybe filterLeaf ys))
          else Left { message: "invalid :refer names, expected all leaves", data: cirruToCalcit node ns }
        _, _ -> Left { message: "invalid :refer rule", data: cirruToCalcit node ns}
      _ -> Left { message: "unknown import rule", data: cirruToCalcit node ns }
    else Left { message: "expected import rule in length 3", data: cirruToCalcit node ns }

-- | parse (ns a.b (:require ...))
extractImportMap :: CirruNode -> String -> Either CalcitFailure (Map.Map String ImportRule)
extractImportMap node ns = case node of
  CirruLeaf s -> Left { message: "ns rule expects a list", data: cirruToCalcit node ns }
  CirruList xs -> case (xs !! 0), (xs !! 1), (xs !! 2) of
    Just (CirruLeaf _), Just (CirruLeaf _), Just (CirruList ys) ->
      if (ys !! 0) == Just (CirruLeaf ":require")
      then case traverse (\line -> extractImportRule line ns) (Array.drop 1 ys) of
        Right zs -> Right (Map.fromFoldable (Array.concat zs))
        Left failure -> Left failure
      else Left { message: "expected :require field", data: cirruToCalcit (CirruList ys) ns}
    Just (CirruLeaf _), Just (CirruLeaf _), Nothing -> Right (Map.fromFoldable [])
    _, _, _ -> Left { message: "invalid ns format", data: cirruToCalcit node ns}


extractProgramData :: Snapshot -> Either CalcitFailure ProgramCodeData
extractProgramData s =
  let
    getFileTuple :: String -> Either CalcitFailure (Tuple String ProgramFileData)
    getFileTuple ns = do
      fileInfo <- case Map.lookup ns s.files of
        Just file -> Right file
        Nothing -> Left { message: "cannot find ns in map", data: CalcitNil }
      importMap <- extractImportMap fileInfo.ns ns
      let file = {
        importMap: importMap,
        defs: Map.mapMaybe (\x -> Just (cirruToCalcit x ns)) fileInfo.defs
      }
      pure (Tuple ns file)
  in
    -- use internal for list
    case traverse getFileTuple (MapInternal.keys s.files) of
      Right xs -> Right (Map.fromFoldable xs)
      Left x -> Left x

lookupDef :: String -> String -> ProgramCodeData -> Maybe (CalcitData)
lookupDef ns def p = do
  file <- Map.lookup ns p
  Map.lookup def file.defs

lookupDefTargetInImport :: String -> String -> ProgramCodeData -> Maybe String
lookupDefTargetInImport ns def p = do
  file <- Map.lookup ns p
  importRule <- Map.lookup def file.importMap
  case importRule of
    ImportDefRule target _ -> Just target
    ImportNsRule _ -> Nothing

lookupNsTargetInImport :: String -> String -> ProgramCodeData -> Maybe String
lookupNsTargetInImport ns name p = do
  file <- Map.lookup ns p
  importRule <- Map.lookup name file.importMap
  case importRule of
    ImportDefRule _ _ -> Nothing
    ImportNsRule target -> Just target

lookupEvaledDef :: String -> String -> Effect (Maybe CalcitData)
lookupEvaledDef ns def = do
  program <- Ref.read programEvaledDataRef
  case Map.lookup ns program of
    Nothing -> pure Nothing
    Just defs -> case Map.lookup def defs of
      Nothing -> pure Nothing
      Just v -> pure (Just v)

writeEvaledDef :: String -> String -> CalcitData -> Effect Unit
writeEvaledDef ns def v = do
  program <- Ref.read programEvaledDataRef
  case Map.lookup ns program of
    Nothing -> Ref.write newProgram programEvaledDataRef
      where
        newProgram = Map.fromFoldable [Tuple ns (Map.fromFoldable [Tuple def v])]
    Just defs -> Ref.write newProgram programEvaledDataRef
      where
        newProgram = Map.insert ns (Map.insert def v defs) program

