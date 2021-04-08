module Calcit.Builtin.String where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (pure, show, ($), (<>), (==), (+))
import Prelude as Functor

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

fnNativeSplit :: (Array CalcitData) -> Effect CalcitData
fnNativeSplit xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitString s), Just (CalcitString sep) -> pure (CalcitList (Functor.map (\x -> CalcitString x) ys))
    where
    ys = String.split (String.Pattern sep) s
  Just a, Just b -> throw "split expected 2 strings"
  _, _ -> throw "split expected 2 arguments"

-- TODO trim specific character
fnNativeTrim :: (Array CalcitData) -> Effect CalcitData
fnNativeTrim xs = case (xs !! 0) of
  Just (CalcitString s) -> pure (CalcitString (String.trim s))
  Just a -> throw "trim expected string"
  Nothing -> throw "trim expected a argument"

fnNativeStrFind :: (Array CalcitData) -> Effect CalcitData
fnNativeStrFind xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitString s), Just (CalcitString piece) -> case String.indexOf (String.Pattern piece) s of
    Just idx -> pure (CalcitNumber (Int.toNumber idx))
    Nothing -> pure CalcitNil
  Just a, Just b -> throw "str-find expected 2 strings"
  _, _ -> throw "str-find expected 2 arguments"

fnNativeStartsWith :: (Array CalcitData) -> Effect CalcitData
fnNativeStartsWith xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitString s), Just (CalcitString piece) -> case String.indexOf (String.Pattern piece) s of
    Just 0 -> pure (CalcitBool true)
    _ -> pure (CalcitBool false)
  Just a, Just b -> throw "starts-with? expected 2 strings"
  _, _ -> throw "starts-with? expected 2 arguments"

fnNativeEndsWith :: (Array CalcitData) -> Effect CalcitData
fnNativeEndsWith xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitString s), Just (CalcitString piece) -> case String.lastIndexOf (String.Pattern piece) s of
    Just n -> pure (CalcitBool ((n + (String.length piece)) == (String.length s)))
    _ -> pure (CalcitBool false)
  Just a, Just b -> throw "ends-with? expected 2 strings"
  _, _ -> throw "ends-with? expected 2 arguments"
