
module Calcit.Builtin.List where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!), length)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Prelude (bind, discard, pure, (<>), ($))


fnNativeList :: (Array CalcitData) -> Effect CalcitData
fnNativeList xs = pure (CalcitList xs)

fnNativeNth :: (Array CalcitData) -> Effect CalcitData
fnNativeNth xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "nth expected index in int"
    Just index -> case ys !! index of
      Just x -> pure x
      Nothing -> pure CalcitNil
  _, Just (CalcitNumber _) -> throw "nth expected list"
  Just (CalcitList _), _ -> throw "nth expected index"
  _, _ -> throw "failed call nth"

fnNativeCount :: (Array CalcitData) -> Effect CalcitData
fnNativeCount xs = case (xs !! 0) of
  Just (CalcitList ys) -> pure (CalcitNumber (toNumber (length ys)))
  Just _ -> throw "count expected a List"
  Nothing -> throw "count expected an argument"

fnNativeSlice :: (Array CalcitData) -> Effect CalcitData
fnNativeSlice xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just (CalcitNumber from), Just (CalcitNumber to) ->
    case (Int.fromNumber from), (Int.fromNumber to) of
      Just fromIdx, Just toIdx -> pure (CalcitList (Array.slice fromIdx toIdx ys))
      _, _ -> throw "failed to convert int"
  Just (CalcitList ys), Just (CalcitNumber from), Nothing ->
    case (Int.fromNumber from) of
      Just fromIdx -> pure (CalcitList (Array.slice fromIdx (Array.length ys) ys))
      _ -> throw "failed to convert int of from"
  -- TODO
  _, _, _ -> throw "failed to call slice"

fnNativeFoldl :: (Array CalcitData) -> Effect CalcitData
fnNativeFoldl xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just x0, Just (CalcitFn _ _ f) ->
    callItems x0 ys
    where
      callItems :: CalcitData -> Array CalcitData -> Effect CalcitData
      callItems a0 zs = case zs !! 0 of
        Nothing -> pure a0
        Just z0 -> do
          acc <- f [a0, z0]
          callItems acc (Array.drop 1 zs)
  a1, a2, a3 -> do
    log $ "a1: " <> (show a1)
    log $ "a2: " <> (show a2)
    log $ "a3: " <> (show a3)
    throw "expected list, a0, and function for foldl"

fnNativeMap :: (Array CalcitData) -> Effect CalcitData
fnNativeMap xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitFn _ _ f) -> do
    ret <- traverse (\y -> f [y]) ys
    pure (CalcitList ret)
  a1, a2 -> do
    log $ "a1: " <> (show a1)
    log $ "a2: " <> (show a2)
    throw "expected list and function for map"

fnNativeConcat :: (Array CalcitData) -> Effect CalcitData
fnNativeConcat xs = case (xs !! 0) of
  Just (CalcitList ys) -> do
    ret <- traverse (\y -> case y of
      CalcitList zs -> pure zs
      _ -> throw "expected list inside list for concat"
    ) ys
    pure (CalcitList (Array.concat ret))
  a1 -> do
    log $ "a1: " <> (show a1)
    throw "expected list and function for concat"
