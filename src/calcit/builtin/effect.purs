module Calcit.Builtin.Effect where

import Data.Show

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Prelude (discard, pure, ($), (<>))

calcitToString :: CalcitData -> String
calcitToString x = case x of
  CalcitString s -> s
  _ -> show x

fnNativeEcho :: (Array CalcitData) -> Effect CalcitData
fnNativeEcho xs = do
  log $ String.joinWith " " (Functor.map calcitToString xs)
  pure CalcitNil

fnNativeRaise :: (Array CalcitData) -> Effect CalcitData
fnNativeRaise xs = case (xs !! 0) of
  Just (CalcitString s) -> throw s
  Just a -> throw $ "unknown argument for raise" <> (show a)
  Nothing -> throw "missing argument for raise"
