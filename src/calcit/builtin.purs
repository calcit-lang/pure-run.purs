
module Calcit.Builtin where

import Data.Either
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Tuple

import Effect

import Prelude (bind, (+))

import Calcit.Primes

calcitAsNumber :: CalcitData -> Either CalcitFailure Number
calcitAsNumber x = case x of
  CalcitNumber n -> Right n
  _ -> Left { message: "not a Number", data: x }

type CalcitProc = (Array CalcitData) -> CalcitScope -> Either CalcitFailure CalcitData

fn_NativeAdd :: CalcitProc
fn_NativeAdd xs scope = do
  a1 <- case xs !! 0 of
    Nothing -> Left { message: "cannot access 0 in array", data: (CalcitList xs) }
    Just x -> Right x
  a2 <- case xs !! 1 of
    Nothing -> Left { message: "cannot access 1 in array", data: (CalcitList xs) }
    Just x -> Right x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  Right (CalcitNumber (n1 + n2))
