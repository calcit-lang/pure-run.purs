
module Calcit.Program where

import Calcit.Primes (CalcitData(..), CalcitFailure, cirruToCalcit, CalcitScope)
import Calcit.Snapshot (Snapshot)
import Cirru.Edn (CirruEdn)
import Cirru.Node (CirruNode)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Map.Internal as MapInternal
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unit
import Effect (Effect)
import Effect.Ref as Ref
import Prelude (bind, pure)

data ImportRule = ImportNsRule String | ImportDefRule String String

-- | information extracted from snapshot
type ProgramFileData = {
  importMap :: Map.Map String ImportRule,
  defs :: Map.Map String CalcitData
}

type ProgramCodeData = Map.Map String ProgramFileData

type EvalFn = CalcitData -> CalcitScope -> String -> ProgramCodeData -> Effect CalcitData

instance showImportRule :: Show ImportRule where
  show x = "TODO import"

-- | real program state
programEvaledData :: Effect (Ref.Ref (Map.Map String (Map.Map String CalcitData)))
programEvaledData = Ref.new (Map.fromFoldable [])

-- | TODO crossing namespaces
extractImportRule :: CirruEdn -> Either CalcitFailure (Array (Tuple String ImportRule))
extractImportRule edn = Right [(Tuple "TODO" (ImportNsRule "TODO"))]

-- | TODO
extractImportMap :: CirruEdn -> Either CalcitFailure (Map.Map String ImportRule)
extractImportMap edn = Right (Map.fromFoldable [])

extractProgramData :: Snapshot -> Either CalcitFailure ProgramCodeData
extractProgramData s =
  let
    getFileTuple :: String -> Either CalcitFailure (Tuple String ProgramFileData)
    getFileTuple ns = do
      fileInfo <- case Map.lookup ns s.files of
        Just file -> Right file
        Nothing -> Left { message: "cannot find ns in map", data: CalcitNil }
      let file = {
        -- TODO parse from rules
        importMap: Map.fromFoldable [],
        defs: Map.mapMaybe cirruToMaybeCalcit fileInfo.defs
      }
      Right (Tuple ns file)

    cirruToMaybeCalcit :: CirruNode -> Maybe CalcitData
    cirruToMaybeCalcit x = Just (cirruToCalcit x)
  in
    -- use internal for list
    case traverse getFileTuple (MapInternal.keys s.files) of
      Right xs -> Right (Map.fromFoldable xs)
      Left x -> Left x

lookupDef :: String -> String -> ProgramCodeData -> Maybe (CalcitData)
lookupDef ns def p = do
  file <- Map.lookup ns p
  Map.lookup def file.defs

lookupEvaledDef :: String -> String -> Effect (Maybe CalcitData)
lookupEvaledDef ns def = do
  programRef <- programEvaledData
  program <- Ref.read programRef
  case Map.lookup ns program of
    Nothing -> pure Nothing
    Just defs -> case Map.lookup def defs of
      Nothing -> pure Nothing
      Just v -> pure (Just v)

writeEvaledDef :: String -> String -> CalcitData -> Effect Unit
writeEvaledDef ns def v = do
  programRef <- programEvaledData
  program <- Ref.read programRef
  case Map.lookup ns program of
    Nothing -> Ref.write newProgram programRef
      where
        newProgram = Map.fromFoldable [Tuple ns (Map.fromFoldable [Tuple def v])]
    Just defs -> Ref.write newProgram programRef
      where
        newProgram = Map.insert ns (Map.insert def v defs) program

