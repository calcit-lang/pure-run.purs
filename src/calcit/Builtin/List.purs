module Calcit.Builtin.List where

import Calcit.Primes (CalcitData(..))
import Data.Array (cons, length, range, replicate, reverse, snoc, take, (!!))
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

procRange :: (Array CalcitData) -> Effect CalcitData
procRange xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitNumber from), Just (CalcitNumber to) -> case (Int.fromNumber from), (Int.fromNumber to) of
    Just n1, Just n2 -> do
      ys <- traverse buildNumber (range n1 n2)
      pure (CalcitList ys)
    _, _ -> throw "range expected numbers"
  Just (CalcitNumber to), Nothing -> case Int.fromNumber to of
    Just n -> do
      ys <- (traverse buildNumber (range 0 n))
      pure (CalcitList ys)
    Nothing -> throw "range expected integers"
  Just a, _ -> throw "range expected numbers"
  Nothing, _ -> throw "range expected 1~2 arguments"
  where
  buildNumber x = pure (CalcitNumber (Int.toNumber x))

procRepeat :: Array CalcitData -> Effect CalcitData
procRepeat xs = case xs !! 0, xs !! 1 of
  Just a, Just (CalcitNumber x) -> case Int.fromNumber x of
    Just n -> pure (CalcitList (replicate n a))
    Nothing -> throw "repeat expecter number in integer"
  Just a, Just b -> throw "repeat expected a number in second argument"
  _, _ -> throw "repeat expected 2 arguments"

procReverse :: Array CalcitData -> Effect CalcitData
procReverse xs = case xs !! 0 of
  Just (CalcitList ys) -> pure (CalcitList (reverse ys))
  Just a -> throw "reverse expected a list"
  Nothing -> throw "reverse expected 1 argument"

procAppend :: Array CalcitData -> Effect CalcitData
procAppend xs = case xs !! 0, xs !! 1 of
  Just (CalcitList ys), Just a -> pure (CalcitList (snoc ys a))
  Just a, Just b -> throw "append-expanded a list"
  _, _ -> throw "append expected 2 arguments"

procPrepend :: Array CalcitData -> Effect CalcitData
procPrepend xs = case xs !! 0, xs !! 1 of
  Just (CalcitList ys), Just a -> pure (CalcitList (cons a ys))
  Just a, Just b -> throw "prepend-expanded a list"
  _, _ -> throw "prepend expected 2 arguments"

procTake :: (Array CalcitData) -> Effect CalcitData
procTake xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "take expected index in int"
    Just index -> pure (CalcitList (take index ys))
  Just (CalcitString s), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "take expected index in int"
    Just index -> pure (CalcitString (String.take index s))
  _, Just (CalcitNumber _) -> throw "take expected list or string"
  Just (CalcitList _), _ -> throw "take expected index"
  _, _ -> throw "take expected 2 arguments"

procDrop :: (Array CalcitData) -> Effect CalcitData
procDrop xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "drop expected index in int"
    Just index -> pure (CalcitList (Array.drop index ys))
  Just (CalcitString s), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "drop expected index in int"
    Just index -> pure (CalcitString (String.drop index s))
  _, Just (CalcitNumber _) -> throw "drop expected list or string"
  Just (CalcitList _), _ -> throw "drop expected index"
  _, _ -> throw "drop expected 2 arguments"

-- TODO sort
-- TODO find
-- TODO find-index
-- TODO group-by
-- TODO interleave
-- TODO zip
-- TODO map-maybe
-- TODO mapcat
-- TODO map-indexed
