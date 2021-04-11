module Calcit.Builtin.Number where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Int (rem)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Math (pow)
import Prelude (bind, pure, ($), (+), (-), (<), (>), (==), (<>), (*), (/), show)

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

nthNum :: (Array CalcitData) -> Int -> Effect Number
nthNum xs n = case xs !! n of
  Nothing -> throw $ "cannot access " <> (show n) <> " in array"
  Just x -> calcitAsNumber x

procAdd :: (Array CalcitData) -> Effect CalcitData
procAdd xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  pure (CalcitNumber (n1 + n2))

procMinus :: (Array CalcitData) -> Effect CalcitData
procMinus xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  -- log $ "&- " <> (show n1) <> " " <> (show n2) <> " " <> (show (n1 - n2))
  pure (CalcitNumber (n1 - n2))

procMultiply :: (Array CalcitData) -> Effect CalcitData
procMultiply xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  pure (CalcitNumber (n1 * n2))

procDivide :: (Array CalcitData) -> Effect CalcitData
procDivide xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  pure (CalcitNumber (n1 / n2))

procPow :: (Array CalcitData) -> Effect CalcitData
procPow xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  pure (CalcitNumber (pow n1 n2))

procLt :: (Array CalcitData) -> Effect CalcitData
procLt xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  pure (CalcitBool (n1 < n2))

procGt :: (Array CalcitData) -> Effect CalcitData
procGt xs = do
  n1 <- nthNum xs 0
  n2 <- nthNum xs 1
  pure (CalcitBool (n1 > n2))

procEq :: (Array CalcitData) -> Effect CalcitData
procEq xs = do
  -- log $ "> "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  pure (CalcitBool (a1 == a2))

procMod :: (Array CalcitData) -> Effect CalcitData
procMod xs = do
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
