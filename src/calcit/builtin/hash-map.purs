
module Calcit.Builtin.HashMap where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (bind, pure, ($), (<>), show)

fnNativeHashMap :: (Array CalcitData) -> Effect CalcitData
fnNativeHashMap xs =
  do
    ys <- foldTuples [] xs
    pure (CalcitMap (Map.fromFoldable ys))
  where

    foldTuples :: Array (Tuple CalcitData CalcitData) -> Array CalcitData
       -> Effect (Array (Tuple CalcitData CalcitData))
    foldTuples acc ys = case (ys !! 0), (ys !! 1) of
      Just a, Just b -> foldTuples (Array.snoc acc (Tuple a b)) (Array.drop 2 ys)
      Nothing, _ -> pure acc
      Just _, Nothing -> throw "&{} got odd arguments"

fnNativeAssoc :: (Array CalcitData) -> Effect CalcitData
fnNativeAssoc xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just (CalcitNumber n), Just v -> case Int.fromNumber n of
    Just idx -> case Array.updateAt idx v ys of
      Just ys2 -> pure (CalcitList ys2)
      Nothing -> throw "failed to update"
    Nothing -> throw "failed to call assoc"

  Just (CalcitMap ys), Just k, Just v -> pure (CalcitMap (Map.insert k v ys))
  Just a, _, _ -> throw $ "unexpected data: " <> (show a)
  Nothing, _, _ -> throw "assoc expected arguments"

fnNativeDissoc :: (Array CalcitData) -> Effect CalcitData
fnNativeDissoc xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitNumber n) -> case Int.fromNumber n of
    Just idx -> case Array.deleteAt idx ys of
      Just ys2 -> pure (CalcitList ys2)
      Nothing -> throw "index out of bound in dissoc"
    Nothing -> throw "failed to call dissoc"
  Just (CalcitMap ys), Just k -> pure (CalcitMap (Map.delete k ys))
  Just _, _ -> throw "dissoc expected data structure"
  _, _ -> throw "dissoc expected arguments"

-- TODO merge

-- TODO to-pairs
-- TODO pairs-map

-- TODO map-kv

-- TODO map-maybe

-- TODO keys

-- TODO vals

-- TODO pick-keys
