module Calcit.Builtin.Ref where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Prelude (bind, discard, pure, ($), (<>), show)

procRef :: (Array CalcitData) -> Effect CalcitData
procRef xs = case xs !! 0 of
  Nothing -> throw "ref expected 1 argument"
  Just x0 -> do
    r <- Ref.new x0
    uid <- genUUID
    pure (CalcitRef uid r)

procDeref :: (Array CalcitData) -> Effect CalcitData
procDeref xs = case xs !! 0 of
  Just (CalcitRef _ r) -> do
    v <- Ref.read r
    pure v
  Just a -> throw $ "expected a ref, got: " <> (show a)
  Nothing -> throw "expected an argument"

procReset :: (Array CalcitData) -> Effect CalcitData
procReset xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitRef _ r), Just v -> do
    Ref.write v r
    pure CalcitNil
  _, Nothing -> throw "expected a new value"
  Nothing, _ -> throw "expected an argument"
  Just _, _ -> throw "expected a ref argument"

-- TODO add-watch remove-watch
