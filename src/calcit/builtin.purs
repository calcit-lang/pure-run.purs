
module Calcit.Builtin where

import Data.Show
import Effect
import Effect

import Calcit.Primes (CalcitData(..), CalcitFailure, CalcitScope)
import Cirru.Node (CirruNode(..))
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Class.Console (log)
import Prelude (bind, (+), ($))
import Prelude (discard, pure)

calcitAsNumber :: CalcitData -> Either CalcitFailure Number
calcitAsNumber x = case x of
  CalcitNumber n -> Right n
  _ -> Left { message: "not a Number", data: x }

nativeAdd :: (Array CalcitData) -> CalcitScope -> Either CalcitFailure CalcitData
nativeAdd xs scope = do
  a1 <- case xs !! 0 of
    Nothing -> Left { message: "cannot access 0 in array", data: (CalcitList xs) }
    Just x -> Right x
  a2 <- case xs !! 1 of
    Nothing -> Left { message: "cannot access 1 in array", data: (CalcitList xs) }
    Just x -> Right x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  Right (CalcitNumber (n1 + n2))

calcitToString :: CalcitData -> String
calcitToString x = show x

nativeEcho :: (Array CalcitData) -> Effect CalcitData
nativeEcho xs = do
  log $ String.joinWith " " (Functor.map calcitToString xs)
  pure CalcitNil


nativeDefn :: (Array CalcitData) -> CalcitScope -> Either CalcitFailure CalcitData
nativeDefn xs scope = do
  nameNode <- case xs !! 0 of
    Just v -> Right v
    Nothing -> Left { message: "function name not found" , data: CalcitList xs }
  name <- case nameNode of
    CalcitSymbol s -> Right s
    _ -> Left { message: "function name not a symbol", data: nameNode }
  argsNode <- case xs !! 1 of
    Just v -> Right v
    Nothing -> Left { message: "function args not found", data: CalcitList xs }
  args <- case argsNode of
    CalcitList ys -> Right ys
    _ -> Left { message: "function args not list", data: argsNode }
  Right (CalcitFn name args (Array.drop 2 xs) scope)
