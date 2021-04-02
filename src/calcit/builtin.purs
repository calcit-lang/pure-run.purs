
module Calcit.Builtin where

import Data.Show

import Calcit.Primes (CalcitData(..))
import Calcit.Syntax (coreNsSyntaxes)
import Data.Array (foldl, length, (!!))
import Data.Array as Array
import Data.Functor as Functor
import Data.Int (toNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
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
calcitToString x = case x of
  CalcitString s -> s
  _ -> show x

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

fnNativeFoldl :: (Array CalcitData) -> Effect CalcitData
fnNativeFoldl xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just x0, Just (CalcitFn _ f) ->
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
  Just (CalcitList ys), Just (CalcitFn _ f) -> do
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

fnNativeRaise :: (Array CalcitData) -> Effect CalcitData
fnNativeRaise xs = case (xs !! 0) of
  Just (CalcitString s) -> throw s
  Just a -> throw $ "unknown argument for raise" <> (show a)
  Nothing -> throw "missing argument for raise"

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.union coreNsSyntaxes coreDefs
  where
    coreDefs = Map.fromFoldable [
      (Tuple "&+" (CalcitFn "&+" fnNativeAdd))
    , (Tuple "&-" (CalcitFn "&-" fnNativeMinus))
    , (Tuple "&<" (CalcitFn "&<" fnNativeLt))
    , (Tuple "&>" (CalcitFn "&>" fnNativeGt))
    , (Tuple "&=" (CalcitFn "&=" fnNativeEq))
    , (Tuple "echo" (CalcitFn "echo" fnNativeEcho))
    , (Tuple "[]" (CalcitFn "[]" fnNativeList))
    , (Tuple "nth" (CalcitFn "[]" fnNativeNth))
    , (Tuple "count" (CalcitFn "count" fnNativeCount))
    , (Tuple "slice" (CalcitFn "slice" fnNativeSlice))
    , (Tuple "foldl" (CalcitFn "foldl" fnNativeFoldl))
    , (Tuple "map" (CalcitFn "map" fnNativeMap))
    , (Tuple "concat" (CalcitFn "concat" fnNativeConcat))
    , (Tuple "raise" (CalcitFn "raise" fnNativeRaise))
    ]
