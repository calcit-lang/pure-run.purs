
module Calcit.Builtin where

import Data.Show

import Calcit.Primes (CalcitData(..), CalcitFailure, FnEvalFn, CalcitScope)
import Cirru.Node (CirruNode(..))
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Functor as Functor
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect(..))
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Prelude (bind, (+), ($), discard, pure, (==), (||), (<), (>))

calcitAsNumber :: CalcitData -> Effect Number
calcitAsNumber x = case x of
  CalcitNumber n -> pure n
  _ -> throw "not a Number"

fnNativeAdd :: (Array CalcitData) -> Effect CalcitData
fnNativeAdd xs = do
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitNumber (n1 + n2))

fnNativeLt :: (Array CalcitData) -> Effect CalcitData
fnNativeLt xs = do
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitBool (n1 < n2))

fnNativeGt :: (Array CalcitData) -> Effect CalcitData
fnNativeGt xs = do
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitBool (n1 > n2))

calcitToString :: CalcitData -> String
calcitToString x = show x

procEcho :: (Array CalcitData) -> Effect CalcitData
procEcho xs = do
  log $ String.joinWith " " (Functor.map calcitToString xs)
  pure CalcitNil

syntaxDefn :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxDefn xs scope evalFn =
  do
    nameNode <- case xs !! 0 of
      Just v -> pure v
      Nothing -> throw "function name not found"
    name <- case nameNode of
      CalcitSymbol s -> pure s
      _ -> throw "function name not a symbol"
    argsNode <- case xs !! 1 of
      Just v -> pure v
      Nothing -> throw "function args not found"
    args <- case argsNode of
      CalcitList ys -> pure ys
      _ -> throw "function args not list"
    pure (CalcitFn name f)
  where
    callLines :: Array CalcitData -> Effect CalcitData
    callLines zs = case zs !! 0 of
      Just z0 -> do
        v <- evalFn z0 scope
        callLines (Array.drop 1 zs)
      Nothing -> pure CalcitNil
    f ys = callLines (Array.drop 2 xs)

syntaxIf :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxIf xs scope evalFn = do
  cond <- case xs !! 0 of
    Just v -> evalFn v scope
    Nothing -> throw "cond not found"
  if cond == CalcitNil || cond == CalcitBool false
    then case xs !! 2 of
      Just falseBranch -> evalFn falseBranch scope
      Nothing -> pure CalcitNil
    else case xs !! 1 of
      Just trueBranch -> evalFn trueBranch scope
      Nothing -> pure CalcitNil

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.fromFoldable [
  (Tuple "&+" (CalcitFn "&+" fnNativeAdd)),
  (Tuple "&<" (CalcitFn "&<" fnNativeLt)),
  (Tuple "&>" (CalcitFn "&>" fnNativeGt)),
  (Tuple "echo" (CalcitFn "echo" procEcho)),
  (Tuple "defn" (CalcitSyntax "defn" syntaxDefn)),
  (Tuple "if" (CalcitSyntax "if" syntaxIf))
]