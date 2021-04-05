
module Calcit.Builtin.Bool where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (pure, not, (||), (&&))

fnNativeNot :: (Array CalcitData) -> Effect CalcitData
fnNativeNot xs = case (xs !! 0) of
  Just (CalcitBool a) -> pure (CalcitBool (not a))
  Just _ -> throw "not expected a bool"
  Nothing -> throw "insufficient arguments"