
module Calcit.Builtin.String where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (pure, show, (<>), ($))

fnNativeStr :: (Array CalcitData) -> Effect CalcitData
fnNativeStr xs = case xs !! 0 of
  Just x -> pure (CalcitString (show x))
  Nothing -> throw "&str expected 1 argument"

fnNativeStrConcat :: (Array CalcitData) -> Effect CalcitData
fnNativeStrConcat xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitString s1), Just (CalcitString s2) -> pure (CalcitString (s1 <> s2))
  Just (CalcitString s1), Just a2 -> throw $ "&str-concat expected 2 strings, got: " <> (show a2)
  Just a1, Just (CalcitString s2) -> throw $ "&str-concat expected 2 strings, got: " <> (show a1)
  Just a1, Just a2 -> throw $ "&str-concat expected 2 strings, got: " <> (show a1) <> " " <> (show a2)
  Nothing, _ -> throw "&str-concat expected 2 arguments"
  _, Nothing -> throw "&str-concat expected 2 arguments"

fnNativeTurnString :: (Array CalcitData) -> Effect CalcitData
fnNativeTurnString xs = case xs !! 0 of
  Just (CalcitNil) -> pure (CalcitString "")
  Just (CalcitString s) -> pure (CalcitString s)
  Just (CalcitKeyword s) -> pure (CalcitString s)
  Just (CalcitSymbol s ns) -> pure (CalcitString s)
  Nothing -> throw "turn-string expected 1 argument"
  a -> throw $ "failed to turn string: " <> (show a)


-- TODO split

-- TODO trim

-- TODO str-find

-- TODO macro: starts-with? ends-with?

-- TODO regex
