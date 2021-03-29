
module Calcit.Builtin where

import Data.Show

import Calcit.Primes (CalcitData(..), CalcitFailure, FnEvalFn, CalcitScope)
import Cirru.Node (CirruNode(..))
import Data.Array ((!!), zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Functor as Functor
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect (Effect(..))
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Prelude (bind, (+), (-), ($), discard, pure, (==), (||), (<), (>), (<>))

calcitAsNumber :: CalcitData -> Effect Number
calcitAsNumber x = case x of
  CalcitNumber n -> pure n
  _ -> throw $ (show x) <> " is not a number"

fnNativeAdd :: (Array CalcitData) -> Effect CalcitData
fnNativeAdd xs = do
  -- log $ "+ "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  pure (CalcitNumber (n1 + n2))

fnNativeMinus :: (Array CalcitData) -> Effect CalcitData
fnNativeMinus xs = do
  -- log $ "- "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  n1 <- calcitAsNumber a1
  n2 <- calcitAsNumber a2
  -- log $ "&- " <> (show n1) <> " " <> (show n2) <> " " <> (show (n1 - n2))
  pure (CalcitNumber (n1 - n2))

fnNativeLt :: (Array CalcitData) -> Effect CalcitData
fnNativeLt xs = do
  -- log $ "< "  <> (show xs)
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
  -- log $ "> "  <> (show xs)
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
    argNames <- traverse extractArgName args
    let f = \ys -> callLines (Array.drop 2 xs) (Map.unions [(Map.fromFoldable (zip argNames ys)), scope])
    pure (CalcitFn name f)
  where
    extractArgName :: CalcitData -> Effect String
    extractArgName arg = case arg of
      CalcitSymbol s -> pure s
      _ -> throw "expected symbol"
    callLines :: Array CalcitData -> CalcitScope -> Effect CalcitData
    callLines zs s2 = case zs !! 0 of
      Just z0 -> do
        -- log $ "call line: " <> (show z0)
        v <- evalFn z0 s2
        let follows = Array.drop 1 zs
        if (Array.length follows) == 0
        then pure v
        else callLines (Array.drop 1 zs) s2
      Nothing -> pure CalcitNil

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

syntaxComment :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxComment _ _ _ = pure CalcitNil

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.fromFoldable [
  (Tuple "&+" (CalcitFn "&+" fnNativeAdd)),
  (Tuple "&-" (CalcitFn "&-" fnNativeMinus)),
  (Tuple "&<" (CalcitFn "&<" fnNativeLt)),
  (Tuple "&>" (CalcitFn "&>" fnNativeGt)),
  (Tuple "echo" (CalcitFn "echo" procEcho)),
  (Tuple "defn" (CalcitSyntax "defn" syntaxDefn)),
  (Tuple "if" (CalcitSyntax "if" syntaxIf)),
  (Tuple ";" (CalcitSyntax ";" syntaxComment)),
  (Tuple "--" (CalcitSyntax "--" syntaxComment))
]