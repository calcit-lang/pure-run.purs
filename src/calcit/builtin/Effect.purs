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
import Prelude (discard, pure, ($), (<>), bind)

calcitToString :: CalcitData -> String
calcitToString x = case x of
  CalcitString s -> s
  _ -> show x

procEcho :: (Array CalcitData) -> Effect CalcitData
procEcho xs = do
  log $ String.joinWith " " (Functor.map calcitToString xs)
  pure CalcitNil

procRaise :: (Array CalcitData) -> Effect CalcitData
procRaise xs = case (xs !! 0) of
  Just (CalcitString s) -> throw s
  Just a -> throw $ "unknown argument for raise" <> (show a)
  Nothing -> throw "missing argument for raise"

foreign import requireEvalImpl :: String -> String -> Effect CalcitData

procEvalCommonjsFile :: (Array CalcitData) -> Effect CalcitData
procEvalCommonjsFile xs = case xs !! 0, xs !! 1 of
  Just (CalcitString s), Just (CalcitString name) -> do
    o <- requireEvalImpl s name
    pure CalcitNil
  Just a, Just b -> throw "eval-commonjs-file expected 2 strings"
  _, _ -> throw "eval-commonjs-file expected 2 arguments"
