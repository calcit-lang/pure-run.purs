
module Calcit.Builtin.Number where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Int (rem)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (bind, pure, ($), (+), (-), (<), (>), (==), (<>), show)

calcitAsNumber :: CalcitData -> Effect Number
calcitAsNumber x = case x of
  CalcitNumber n -> pure n
  _ -> throw $ (show x) <> " is not a number"

calcitAsInt :: CalcitData -> Effect Int
calcitAsInt x = case x of
  CalcitNumber n -> case Int.fromNumber n of
    Just i -> pure i
    Nothing -> throw $ (show x) <> " is not a int"
  _ -> throw $ (show x) <> " is not a int"

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

fnNativeMod :: (Array CalcitData) -> Effect CalcitData
fnNativeMod xs = do
  -- log $ "> "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsInt a1
  n2 <- calcitAsInt a2
  pure (CalcitNumber (Int.toNumber (rem n1 n2)))
