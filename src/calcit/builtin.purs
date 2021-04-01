
module Calcit.Builtin where

import Data.Show

import Calcit.Primes (CalcitData(..))
import Calcit.Syntax (coreNsSyntaxes)
import Data.Array (length, (!!))
import Data.Array as Array
import Data.Functor as Functor
import Data.Int (toNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Prelude (bind, discard, pure, ($), (+), (-), (<), (<>), (==), (>))

calcitAsNumber :: CalcitData -> Effect Number
calcitAsNumber x = case x of
  CalcitNumber n -> pure n
  _ -> throw $ (show x) <> " is not a number"

fnNativeAdd :: (Array CalcitData) -> Effect CalcitData
fnNativeAdd xs = do
  -- log $ "+ "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitNumber (n1 + n2))

fnNativeMinus :: (Array CalcitData) -> Effect CalcitData
fnNativeMinus xs = do
  -- log $ "- "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  -- log $ "&- " <> (show n1) <> " " <> (show n2) <> " " <> (show (n1 - n2))
  pure (CalcitNumber (n1 - n2))

fnNativeLt :: (Array CalcitData) -> Effect CalcitData
fnNativeLt xs = do
  -- log $ "< "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitBool (n1 < n2))

fnNativeGt :: (Array CalcitData) -> Effect CalcitData
fnNativeGt xs = do
  -- log $ "> "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitBool (n1 > n2))

fnNativeEq :: (Array CalcitData) -> Effect CalcitData
fnNativeEq xs = do
  -- log $ "> "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  pure (CalcitBool (a1 == a2))

calcitToString :: CalcitData -> String
calcitToString x = show x

fnNativeEcho :: (Array CalcitData) -> Effect CalcitData
fnNativeEcho xs = do
  log $ String.joinWith " " (Functor.map calcitToString xs)
  pure CalcitNil

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

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.union coreNsSyntaxes coreDefs
  where
    coreDefs = Map.fromFoldable [
      (Tuple "&+" (CalcitFn "&+" fnNativeAdd)),
      (Tuple "&-" (CalcitFn "&-" fnNativeMinus)),
      (Tuple "&<" (CalcitFn "&<" fnNativeLt)),
      (Tuple "&>" (CalcitFn "&>" fnNativeGt)),
      (Tuple "&=" (CalcitFn "&=" fnNativeEq)),
      (Tuple "echo" (CalcitFn "echo" fnNativeEcho)),
      (Tuple "[]" (CalcitFn "[]" fnNativeList)),
      (Tuple "nth" (CalcitFn "[]" fnNativeNth)),
      (Tuple "count" (CalcitFn "count" fnNativeCount)),
      (Tuple "slice" (CalcitFn "slice" fnNativeSlice))
    ]