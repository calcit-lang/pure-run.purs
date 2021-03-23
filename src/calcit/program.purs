
module Calcit.Program where

import Data.Map (Map)
import Data.Map as DataMap

import Effect
import Effect.Ref as EffectRef
import Effect.Ref (Ref)

import Data.Tuple

import Cirru.Edn
import Calcit.Primes
import Calcit.Builtin

data ImportRule = ImportNsRule String Boolean | ImportDefRule String String

-- | TODO
extractImportMap :: CirruEdn -> Map String ImportRule
extractImportMap edn = DataMap.fromFoldable []

type ProgramFileData = {
  importMap :: Map String ImportRule,
  defs :: Map String CalcitData,
  data :: Map String CalcitData
}

programData :: Effect (Ref (Map String ProgramFileData))
programData = EffectRef.new (DataMap.fromFoldable [])

coreFns :: Map String CalcitProc
coreFns = DataMap.fromFoldable [
  (Tuple "&+" fn_NativeAdd)
]
