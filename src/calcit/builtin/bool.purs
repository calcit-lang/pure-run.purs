
module Calcit.Builtin.Bool where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (pure, not, (||), (&&))

fnNativeAnd :: (Array CalcitData) -> Effect CalcitData
fnNativeAnd xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitBool a), Just (CalcitBool b) -> pure (CalcitBool (a && b))
  Just (CalcitBool a), Nothing -> pure (CalcitBool a)
  Just _, _ -> throw "&and expected bool values"
  Nothing, _ -> throw "insufficient arguments"

fnNativeOr :: (Array CalcitData) -> Effect CalcitData
fnNativeOr xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitBool a), Just (CalcitBool b) -> pure (CalcitBool (a || b))
  Just (CalcitBool a), Nothing -> pure (CalcitBool a)
  Just _, _ -> throw "&or expected bool values"
  Nothing, _ -> throw "insufficient arguments"

fnNativeNot :: (Array CalcitData) -> Effect CalcitData
fnNativeNot xs = case (xs !! 0) of
  Just (CalcitBool a) -> pure (CalcitBool (not a))
  Just _ -> throw "not expected a bool"
  Nothing -> throw "insufficient arguments"