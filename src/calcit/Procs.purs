module Calcit.Procs where

import Calcit.Builtin.Bool (fnNativeNot)
import Calcit.Builtin.Effect (fnNativeEcho, fnNativeRaise)
import Calcit.Builtin.File (fnNativeReadFile, fnNativeWriteFile)
import Calcit.Builtin.HashMap (fnNativeAssoc, fnNativeDissoc, fnNativeHashMap, fnNativeMapKv, fnNativeMerge, fnNativeToPairs)
import Calcit.Builtin.List (fnNativeConcat, fnNativeCount, fnNativeFoldl, fnNativeList, fnNativeMap, fnNativeMapMaybe, fnNativeNth, fnNativeSlice)
import Calcit.Builtin.Number (fnNativeAdd, fnNativeEq, fnNativeGt, fnNativeLt, fnNativeMinus, fnNativeMod)
import Calcit.Builtin.Ref (fnNativeDeref, fnNativeRef, fnNativeReset)
import Calcit.Builtin.String (fnNativeEndsWith, fnNativeSplit, fnNativeStartsWith, fnNativeStr, fnNativeStrConcat, fnNativeStrFind, fnNativeTrim, fnNativeTurnString)
import Calcit.Builtin.Symbol (fnNativeGensym, fnNativeRecur, fnNativeResetGensymIndex, fnNativeTypeOf)
import Calcit.Builtin.Syntax (coreNsSyntaxes)
import Calcit.Globals (uidSeed)
import Calcit.Primes (CalcitData(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.UUID (genv3UUID)

builtinRecurFn :: CalcitData
builtinRecurFn = CalcitFn "recur" (genv3UUID "faked_recur" uidSeed) fnNativeRecur

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.union coreNsSyntaxes coreDefs
  where
  coreDefs =
    Map.fromFoldable
      [ (Tuple "&+" (CalcitFn "&+" (genv3UUID "faked_&+" uidSeed) fnNativeAdd))
      , (Tuple "&-" (CalcitFn "&-" (genv3UUID "faked_&-" uidSeed) fnNativeMinus))
      , (Tuple "&<" (CalcitFn "&<" (genv3UUID "faked_&<" uidSeed) fnNativeLt))
      , (Tuple "&>" (CalcitFn "&>" (genv3UUID "faked_&>" uidSeed) fnNativeGt))
      , (Tuple "&=" (CalcitFn "&=" (genv3UUID "faked_&=" uidSeed) fnNativeEq))
      , (Tuple "echo" (CalcitFn "echo" (genv3UUID "faked_echo" uidSeed) fnNativeEcho))
      , (Tuple "concat" (CalcitFn "concat" (genv3UUID "faked_concat" uidSeed) fnNativeConcat))
      , (Tuple "raise" (CalcitFn "raise" (genv3UUID "faked_raise" uidSeed) fnNativeRaise))
      , (Tuple "gensym" (CalcitFn "gensym" (genv3UUID "faked_gensym" uidSeed) fnNativeGensym))
      , (Tuple "reset-gensym-index!" (CalcitFn "reset-gensym-index!" (genv3UUID "faked_reset-gensym-index!" uidSeed) fnNativeResetGensymIndex))
      , (Tuple "not" (CalcitFn "not" (genv3UUID "faked_not" uidSeed) fnNativeNot))
      , (Tuple "ref" (CalcitFn "ref" (genv3UUID "faked_ref" uidSeed) fnNativeRef))
      , (Tuple "deref" (CalcitFn "deref" (genv3UUID "faked_deref" uidSeed) fnNativeDeref))
      , (Tuple "reset!" (CalcitFn "reset!" (genv3UUID "faked_reset!" uidSeed) fnNativeReset))
      , (Tuple "type-of" (CalcitFn "type-of" (genv3UUID "faked_type-of" uidSeed) fnNativeTypeOf))
      , (Tuple "recur" builtinRecurFn)
      , (Tuple "mod" (CalcitFn "mod" (genv3UUID "faked_mod" uidSeed) fnNativeMod))
      -- list
      , (Tuple "[]" (CalcitFn "[]" (genv3UUID "faked_[]" uidSeed) fnNativeList))
      , (Tuple "nth" (CalcitFn "[]" (genv3UUID "faked_nth" uidSeed) fnNativeNth))
      , (Tuple "count" (CalcitFn "count" (genv3UUID "faked_count" uidSeed) fnNativeCount))
      , (Tuple "slice" (CalcitFn "slice" (genv3UUID "faked_slice" uidSeed) fnNativeSlice))
      , (Tuple "foldl" (CalcitFn "foldl" (genv3UUID "faked_foldl" uidSeed) fnNativeFoldl))
      , (Tuple "map" (CalcitFn "map" (genv3UUID "faked_map" uidSeed) fnNativeMap))
      , (Tuple "map-maybe" (CalcitFn "map-maybe" (genv3UUID "faked_map-maybe" uidSeed) fnNativeMapMaybe))
      -- strings
      , (Tuple "&str" (CalcitFn "&str" (genv3UUID "faked_&str" uidSeed) fnNativeStr))
      , (Tuple "&str-concat" (CalcitFn "&str-concat" (genv3UUID "faked_&str-concat" uidSeed) fnNativeStrConcat))
      , (Tuple "turn-string" (CalcitFn "turn-string" (genv3UUID "faked_turn-string" uidSeed) fnNativeTurnString))
      , (Tuple "split" (CalcitFn "split" (genv3UUID "faked_split" uidSeed) fnNativeSplit))
      , (Tuple "trim" (CalcitFn "trim" (genv3UUID "faked_trim" uidSeed) fnNativeTrim))
      , (Tuple "str-find" (CalcitFn "str-find" (genv3UUID "faked_str-find" uidSeed) fnNativeStrFind))
      , (Tuple "starts-with?" (CalcitFn "starts-with?" (genv3UUID "faked_starts-with?" uidSeed) fnNativeStartsWith))
      , (Tuple "ends-with?" (CalcitFn "ends-with?" (genv3UUID "faked_ends-with?" uidSeed) fnNativeEndsWith))
      -- maps
      , (Tuple "&{}" (CalcitFn "&{}" (genv3UUID "faked_&{}" uidSeed) fnNativeHashMap))
      , (Tuple "assoc" (CalcitFn "assoc" (genv3UUID "faked_assoc" uidSeed) fnNativeAssoc))
      , (Tuple "dissoc" (CalcitFn "dissoc" (genv3UUID "faked_dissoc" uidSeed) fnNativeDissoc))
      , (Tuple "&merge" (CalcitFn "&merge" (genv3UUID "faked_&merge" uidSeed) fnNativeMerge))
      , (Tuple "to-pairs" (CalcitFn "to-pairs" (genv3UUID "faked_to-pairs" uidSeed) fnNativeToPairs))
      , (Tuple "map-kv" (CalcitFn "map-kv" (genv3UUID "faked_map-kv" uidSeed) fnNativeMapKv))
      -- file
      , (Tuple "read-file" (CalcitFn "read-file" (genv3UUID "faked_read-file" uidSeed) fnNativeReadFile))
      , (Tuple "write-file" (CalcitFn "write-file" (genv3UUID "faked_write-file" uidSeed) fnNativeWriteFile))
      ]
