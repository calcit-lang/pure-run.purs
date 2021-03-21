module Calcit.Snapshot where

import Data.Either
import Effect
import Prelude

import Cirru.Edn (CirruEdn(..))
import Cirru.Node (CirruNode(..))
import Data.Map (Map)
import Data.Map as DataMap
import Data.Maybe
import Data.Set as DataSet
import Data.Tuple
import Data.Traversable (traverse)
import Data.Foldable

import Data.Array as DataArray

import Partial.Unsafe (unsafePartial)

-- Data Types

type FileInSnapShot = {
  ns :: CirruNode,
  defs :: Map String CirruNode
}

type SnapshotConfigs = {
  initFn :: String,
  reloadFn :: String,
  modules :: Array String,
  version :: String
}

type Snapshot = {
  package :: String,
  configs :: SnapshotConfigs,
  files :: Map String FileInSnapShot
}

-- working with EDN

type EdnFailure = { message :: String, edn :: CirruEdn }

isEdnMap :: CirruEdn -> Boolean
isEdnMap d = case d of
  CrEdnMap dict -> true
  _ -> false

isEdnString :: CirruEdn -> Boolean
isEdnString d = case d of
  CrEdnString s -> true
  _ -> false

ednMapGet :: CirruEdn -> CirruEdn -> Either EdnFailure CirruEdn
ednMapGet d k = case d of
  CrEdnMap x -> note
    { message: "not found in map", edn: d }
    (DataMap.lookup k x)
  _ -> Left { message: "not a map", edn: d }

ednAsString :: CirruEdn -> Either EdnFailure String
ednAsString x = case x of
  CrEdnString s -> Right s
  _ -> Left { message: "not string", edn: x }

ednMapGetAsString :: CirruEdn -> String -> Either EdnFailure String
ednMapGetAsString d k = do
  b <- ednMapGet d (CrEdnKeyword k)
  ednAsString b

ednAsArrayString :: CirruEdn -> Either EdnFailure (Array String)
ednAsArrayString xs = case xs of
  CrEdnList ys -> traverse ednAsString ys
  _ -> Left { message: "not list", edn: xs }

extractDefs :: CirruEdn -> Either EdnFailure (Map String CirruNode)
extractDefs d = case d of
  CrEdnMap x ->
    let
      nameToTuple :: CirruEdn -> Tuple String CirruNode
      nameToTuple name = Tuple k v
        where
          -- TODO not safe...
          k = unsafePartial $ fromJust $ hush $ ednAsString name
          v = unsafePartial $ fromJust $ hush $ ednAsQuote $ fromJust $ DataMap.lookup name x

      pairs = map nameToTuple (DataArray.fromFoldable (DataMap.keys x))
    in
      Right $ DataMap.fromFoldable pairs
  _ -> Left { message: "not a map", edn: d }

extractEdnToFile :: CirruEdn -> Either EdnFailure FileInSnapShot
extractEdnToFile d = do
  nsEdn <- ednMapGet d (CrEdnKeyword "ns")
  ns <- ednAsQuote nsEdn
  defsEdn <- ednMapGet d (CrEdnKeyword "defs")
  defs <- extractDefs defsEdn
  Right { ns: ns, defs: defs }

extractFiles :: CirruEdn -> Either EdnFailure (Map String FileInSnapShot)
extractFiles d = case d of
  CrEdnMap x ->
    let
      extractFilePair name = Tuple k v
        where
          -- TODO not safe
          k = unsafePartial $ fromJust $ hush $ ednAsString name
          v = unsafePartial $ fromJust $ hush $ extractEdnToFile $ fromJust $ DataMap.lookup name x
      pairs = map extractFilePair (DataArray.fromFoldable (DataMap.keys x))
    in
      Right $ DataMap.fromFoldable pairs
  _ -> Left { message: "not a map", edn: d }

ednAsQuote :: CirruEdn -> Either EdnFailure CirruNode
ednAsQuote x = case x of
  CrEdnQuote y -> Right y
  _ -> Left { message: "not a quote data", edn: x }

loadSnapshotData :: CirruEdn -> Either EdnFailure Snapshot
loadSnapshotData edn = do
  pkg <- ednMapGetAsString edn "package"
  configsEdn <- ednMapGet edn (CrEdnKeyword "configs")
  initFn <- ednMapGetAsString configsEdn "init-fn"
  reloadFn <- ednMapGetAsString configsEdn "reload-fn"
  version <- ednMapGetAsString configsEdn "version"
  modulesEdn <- ednMapGet configsEdn (CrEdnKeyword "modules")
  modules <- ednAsArrayString modulesEdn
  filesEdn <- ednMapGet edn (CrEdnKeyword "files")
  files <- extractFiles filesEdn

  Right { package: pkg
    , configs: {
      initFn: initFn,
      reloadFn: reloadFn,
      modules: modules,
      version: version
    }
    , files: files
    }