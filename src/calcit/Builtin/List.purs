module Calcit.Builtin.List where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!), length)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Prelude (bind, discard, pure, ($), (<>), (/=))

procList :: (Array CalcitData) -> Effect CalcitData
procList xs = pure (CalcitList xs)

procNth :: (Array CalcitData) -> Effect CalcitData
procNth xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "nth expected index in int"
    Just index -> case ys !! index of
      Just x -> pure x
      Nothing -> pure CalcitNil
  Just (CalcitString s), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "nth expected index in int"
    Just index -> case charAt index s of
      Just x -> pure (CalcitString (CodeUnits.singleton x))
      Nothing -> pure CalcitNil
  _, Just (CalcitNumber _) -> throw "nth expected list"
  Just (CalcitList _), _ -> throw "nth expected index"
  _, _ -> throw "failed call nth"

procCount :: (Array CalcitData) -> Effect CalcitData
procCount xs = case (xs !! 0) of
  Just (CalcitList ys) -> pure (CalcitNumber (toNumber (length ys)))
  Just (CalcitString s) -> pure (CalcitNumber (toNumber (String.length s)))
  Just _ -> throw "count expected a List"
  Nothing -> throw "count expected an argument"

procSlice :: (Array CalcitData) -> Effect CalcitData
procSlice xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just (CalcitNumber from), Just (CalcitNumber to) -> case (Int.fromNumber from), (Int.fromNumber to) of
    Just fromIdx, Just toIdx -> pure (CalcitList (Array.slice fromIdx toIdx ys))
    _, _ -> throw "failed to convert int"
  Just (CalcitList ys), Just (CalcitNumber from), Nothing -> case (Int.fromNumber from) of
    Just fromIdx -> pure (CalcitList (Array.slice fromIdx (Array.length ys) ys))
    _ -> throw "failed to convert int of from"
  -- TODO
  _, _, _ -> throw "failed to call slice"

procFoldl :: (Array CalcitData) -> Effect CalcitData
procFoldl xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just x0, Just (CalcitFn _ _ f) -> callItems x0 ys
    where
    callItems :: CalcitData -> Array CalcitData -> Effect CalcitData
    callItems a0 zs = case zs !! 0 of
      Nothing -> pure a0
      Just z0 -> do
        acc <- f [ a0, z0 ]
        callItems acc (Array.drop 1 zs)
  a1, a2, a3 -> do
    log $ "a1: " <> (show a1)
    log $ "a2: " <> (show a2)
    log $ "a3: " <> (show a3)
    throw "expected list, a0, and function for foldl"

procMap :: (Array CalcitData) -> Effect CalcitData
procMap xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitFn _ _ f) -> do
    ret <- traverse (\y -> f [ y ]) ys
    pure (CalcitList ret)
  a1, a2 -> do
    log $ "a1: " <> (show a1)
    log $ "a2: " <> (show a2)
    throw "expected list and function for map"

procConcat :: (Array CalcitData) -> Effect CalcitData
procConcat ys = do
  ret <-
    traverse
      ( \y -> case y of
          CalcitList zs -> pure zs
          _ -> throw "expected list inside list for concat"
      )
      ys
  pure (CalcitList (Array.concat ret))

procMapMaybe :: (Array CalcitData) -> Effect CalcitData
procMapMaybe xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitFn _ _ f) -> do
    ret <- traverse (\y -> f [ y ]) ys
    let
      retNonNil = Array.filter (\x -> x /= CalcitNil) ret
    pure (CalcitList retNonNil)
  -- | map-maybe for map, returns a ([] k v) as valid branch, nil as Nothing
  Just (CalcitMap ys), Just (CalcitFn _ _ f) -> do
    ys2 <- traverse (\(Tuple k v) -> f [ k, v ]) (Map.toUnfoldable ys)
    let
      ys3 = Array.mapMaybe extractListToTuple ys2
    pure (CalcitMap (Map.fromFoldable ys3))
  a1, a2 -> do
    log $ "a1: " <> (show a1)
    log $ "a2: " <> (show a2)
    throw "map-maybe expected list and function"
  where
  extractListToTuple :: CalcitData -> Maybe (Tuple CalcitData CalcitData)
  extractListToTuple v = case v of
    CalcitList ys -> case (ys !! 0), (ys !! 1) of
      Just a, Just b -> Just (Tuple a b)
      _, _ -> Nothing
    _ -> Nothing

-- TODO range
-- TODO reverse
-- TODO repeat
-- TODO sort
-- TODO take
-- TODO drop
-- TODO find
-- TODO find-index
-- TODO fold-compare
-- TODO group-by
-- TODO interleave
-- TODO zip
-- TODO map-maybe
-- TODO mapcat
-- TODO map-indexed
