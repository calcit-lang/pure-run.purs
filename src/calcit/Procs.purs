module Calcit.Procs where

import Calcit.Builtin.Bool (procNot)
import Calcit.Builtin.Effect (procEcho, procRaise)
import Calcit.Builtin.File (procReadFile, procWriteFile)
import Calcit.Builtin.HashMap (procAssoc, procDissoc, procHashMap, procMapKv, procMerge, procToPairs)
import Calcit.Builtin.List (procConcat, procCount, procFoldl, procList, procMap, procMapMaybe, procNth, procSlice)
import Calcit.Builtin.Number (procAdd, procEq, procGt, procLt, procMinus, procMod)
import Calcit.Builtin.Ref (procDeref, procRef, procReset)
import Calcit.Builtin.String (procEndsWith, procSplit, procStartsWith, procStr, procStrConcat, procStrFind, procTrim, procTurnString)
import Calcit.Builtin.Symbol (procGensym, procRecur, procResetGensymIndex, procTypeOf)
import Calcit.Builtin.Syntax (coreNsSyntaxes)
import Calcit.Globals (uidSeed)
import Calcit.Primes (CalcitData(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.UUID (genv3UUID)

builtinRecurFn :: CalcitData
builtinRecurFn = CalcitFn "recur" (genv3UUID "faked_recur" uidSeed) procRecur

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.union coreNsSyntaxes coreDefs
  where
  coreDefs =
    Map.fromFoldable
      [ (Tuple "&+" (CalcitFn "&+" (genv3UUID "faked_&+" uidSeed) procAdd))
      , (Tuple "&-" (CalcitFn "&-" (genv3UUID "faked_&-" uidSeed) procMinus))
      , (Tuple "&<" (CalcitFn "&<" (genv3UUID "faked_&<" uidSeed) procLt))
      , (Tuple "&>" (CalcitFn "&>" (genv3UUID "faked_&>" uidSeed) procGt))
      , (Tuple "&=" (CalcitFn "&=" (genv3UUID "faked_&=" uidSeed) procEq))
      , (Tuple "echo" (CalcitFn "echo" (genv3UUID "faked_echo" uidSeed) procEcho))
      , (Tuple "concat" (CalcitFn "concat" (genv3UUID "faked_concat" uidSeed) procConcat))
      , (Tuple "raise" (CalcitFn "raise" (genv3UUID "faked_raise" uidSeed) procRaise))
      , (Tuple "gensym" (CalcitFn "gensym" (genv3UUID "faked_gensym" uidSeed) procGensym))
      , (Tuple "reset-gensym-index!" (CalcitFn "reset-gensym-index!" (genv3UUID "faked_reset-gensym-index!" uidSeed) procResetGensymIndex))
      , (Tuple "not" (CalcitFn "not" (genv3UUID "faked_not" uidSeed) procNot))
      , (Tuple "ref" (CalcitFn "ref" (genv3UUID "faked_ref" uidSeed) procRef))
      , (Tuple "deref" (CalcitFn "deref" (genv3UUID "faked_deref" uidSeed) procDeref))
      , (Tuple "reset!" (CalcitFn "reset!" (genv3UUID "faked_reset!" uidSeed) procReset))
      , (Tuple "type-of" (CalcitFn "type-of" (genv3UUID "faked_type-of" uidSeed) procTypeOf))
      , (Tuple "recur" builtinRecurFn)
      , (Tuple "mod" (CalcitFn "mod" (genv3UUID "faked_mod" uidSeed) procMod))
      -- list
      , (Tuple "[]" (CalcitFn "[]" (genv3UUID "faked_[]" uidSeed) procList))
      , (Tuple "nth" (CalcitFn "[]" (genv3UUID "faked_nth" uidSeed) procNth))
      , (Tuple "count" (CalcitFn "count" (genv3UUID "faked_count" uidSeed) procCount))
      , (Tuple "slice" (CalcitFn "slice" (genv3UUID "faked_slice" uidSeed) procSlice))
      , (Tuple "foldl" (CalcitFn "foldl" (genv3UUID "faked_foldl" uidSeed) procFoldl))
      , (Tuple "map" (CalcitFn "map" (genv3UUID "faked_map" uidSeed) procMap))
      , (Tuple "map-maybe" (CalcitFn "map-maybe" (genv3UUID "faked_map-maybe" uidSeed) procMapMaybe))
      -- strings
      , (Tuple "&str" (CalcitFn "&str" (genv3UUID "faked_&str" uidSeed) procStr))
      , (Tuple "&str-concat" (CalcitFn "&str-concat" (genv3UUID "faked_&str-concat" uidSeed) procStrConcat))
      , (Tuple "turn-string" (CalcitFn "turn-string" (genv3UUID "faked_turn-string" uidSeed) procTurnString))
      , (Tuple "split" (CalcitFn "split" (genv3UUID "faked_split" uidSeed) procSplit))
      , (Tuple "trim" (CalcitFn "trim" (genv3UUID "faked_trim" uidSeed) procTrim))
      , (Tuple "str-find" (CalcitFn "str-find" (genv3UUID "faked_str-find" uidSeed) procStrFind))
      , (Tuple "starts-with?" (CalcitFn "starts-with?" (genv3UUID "faked_starts-with?" uidSeed) procStartsWith))
      , (Tuple "ends-with?" (CalcitFn "ends-with?" (genv3UUID "faked_ends-with?" uidSeed) procEndsWith))
      -- maps
      , (Tuple "&{}" (CalcitFn "&{}" (genv3UUID "faked_&{}" uidSeed) procHashMap))
      , (Tuple "assoc" (CalcitFn "assoc" (genv3UUID "faked_assoc" uidSeed) procAssoc))
      , (Tuple "dissoc" (CalcitFn "dissoc" (genv3UUID "faked_dissoc" uidSeed) procDissoc))
      , (Tuple "&merge" (CalcitFn "&merge" (genv3UUID "faked_&merge" uidSeed) procMerge))
      , (Tuple "to-pairs" (CalcitFn "to-pairs" (genv3UUID "faked_to-pairs" uidSeed) procToPairs))
      , (Tuple "map-kv" (CalcitFn "map-kv" (genv3UUID "faked_map-kv" uidSeed) procMapKv))
      -- file
      , (Tuple "read-file" (CalcitFn "read-file" (genv3UUID "faked_read-file" uidSeed) procReadFile))
      , (Tuple "write-file" (CalcitFn "write-file" (genv3UUID "faked_write-file" uidSeed) procWriteFile))
      ]
