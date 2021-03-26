
module Calcit.Program where

import Data.Map as Map
import Data.Map.Internal as MapInternal
import Data.Either (Either(..))

import Prelude (bind)

import Effect (Effect)
import Effect.Ref as Ref
import Data.Traversable (traverse)

import Data.Tuple (Tuple(..))
import Data.Show (class Show)

import Data.Maybe (Maybe(..))

import Cirru.Node (CirruNode)
import Cirru.Edn (CirruEdn)
import Calcit.Primes (CalcitData(..), CalcitFailure, cirruToCalcit)
import Calcit.Snapshot (Snapshot)

data ImportRule = ImportNsRule String | ImportDefRule String String

-- | information extracted from snapshot
type ProgramFileData = {
  importMap :: Map.Map String ImportRule,
  defs :: Map.Map String CalcitData
}

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

extractProgramData :: Snapshot -> Either CalcitFailure (Map.Map String ProgramFileData)
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

lookupDef :: String -> String -> (Map.Map String ProgramFileData) -> Maybe (CalcitData)
lookupDef ns def p = do
  file <- Map.lookup ns p
  Map.lookup def file.defs
