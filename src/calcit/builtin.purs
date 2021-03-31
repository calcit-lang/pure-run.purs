
module Calcit.Builtin where

import Data.Show

import Calcit.Primes (CalcitData(..), CalcitScope, FnEvalFn)
import Data.Array (length, zip, (!!))
import Data.Array as Array
import Data.Functor as Functor
import Data.Int (toNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Prelude (bind, discard, pure, unit, ($), (+), (-), (<), (<>), (==), (>), (||))

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

fnNativeEq :: (Array CalcitData) -> Effect CalcitData
fnNativeEq xs = do
  -- log $ "> "  <> (show xs)
  a1 <- case xs !! 0 of
    Nothing -> throw "cannot access 0 in array"
    Just x -> pure x
  a2 <- case xs !! 1 of
    Nothing -> throw "cannot access 1 in array"
    Just x -> pure x
  pure (CalcitBool (a1 == a2))

calcitToString :: CalcitData -> String
calcitToString x = show x

fnNativeEcho :: (Array CalcitData) -> Effect CalcitData
fnNativeEcho xs = do
  log $ String.joinWith " " (Functor.map calcitToString xs)
  pure CalcitNil

fnNativeList :: (Array CalcitData) -> Effect CalcitData
fnNativeList xs = pure (CalcitList xs)

fnNativeNth :: (Array CalcitData) -> Effect CalcitData
fnNativeNth xs = case (xs !! 0), (xs !! 1) of
  Just (CalcitList ys), Just (CalcitNumber n) -> case Int.fromNumber n of
    Nothing -> throw "nth expected index in int"
    Just index -> case ys !! index of
      Just x -> pure x
      Nothing -> pure CalcitNil
  _, Just (CalcitNumber _) -> throw "nth expected list"
  Just (CalcitList _), _ -> throw "nth expected index"
  _, _ -> throw "failed call nth"

fnNativeCount :: (Array CalcitData) -> Effect CalcitData
fnNativeCount xs = case (xs !! 0) of
  Just (CalcitList ys) -> pure (CalcitNumber (toNumber (length ys)))
  Just _ -> throw "count expected a List"
  Nothing -> throw "count expected an argument"

fnNativeSlice :: (Array CalcitData) -> Effect CalcitData
fnNativeSlice xs = case (xs !! 0), (xs !! 1), (xs !! 2) of
  Just (CalcitList ys), Just (CalcitNumber from), Just (CalcitNumber to) ->
    case (Int.fromNumber from), (Int.fromNumber to) of
      Just fromIdx, Just toIdx -> pure (CalcitList (Array.slice fromIdx toIdx ys))
      _, _ -> throw "failed to convert int"
  Just (CalcitList ys), Just (CalcitNumber from), Nothing ->
    case (Int.fromNumber from) of
      Just fromIdx -> pure (CalcitList (Array.slice fromIdx (Array.length ys) ys))
      _ -> throw "failed to convert int of from"
  -- TODO
  _, _, _ -> throw "failed to call slice"

-- | evaluate lines and return value of last line
evaluateLines :: Array CalcitData -> CalcitScope -> FnEvalFn -> Effect CalcitData
evaluateLines xs scope evalFn = case xs !! 0 of
  Just xs0 -> do
    -- log $ "call line: " <> (show xs0)
    v <- evalFn xs0 scope
    let follows = Array.drop 1 xs
    if (Array.length follows) == 0
    then pure v
    else evaluateLines (Array.drop 1 xs) scope evalFn
  Nothing -> pure CalcitNil

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
    let f = \ys -> evaluateLines (Array.drop 2 xs) (Map.unions [(Map.fromFoldable (zip argNames ys)), scope]) evalFn
    pure (CalcitFn name f)
  where
    extractArgName :: CalcitData -> Effect String
    extractArgName arg = case arg of
      CalcitSymbol s -> pure s
      _ -> throw "expected symbol"

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

syntaxNativeLet :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxNativeLet xs scope evalFn = do
  pair <- case xs !! 0 of
    Just (CalcitList ys) -> if (Array.length ys) == 2
      then case (ys !! 0), (ys !! 1) of
        Just (CalcitSymbol s), Just v ->
          pure { k: s, v: v }
        _, _ -> throw "expected symbol in &let"
      else throw "expected pair length of 2"
    Just _ -> throw "expected a pair"
    Nothing -> throw "expected pair in first argument"
  v <- evalFn pair.v scope
  let bodyScope = Map.insert pair.k v scope
  evaluateLines (Array.drop 1 xs) bodyScope evalFn

syntaxComment :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxComment _ _ _ = pure CalcitNil

coreNsDefs :: Map.Map String CalcitData
coreNsDefs = Map.fromFoldable [
  (Tuple "&+" (CalcitFn "&+" fnNativeAdd)),
  (Tuple "&-" (CalcitFn "&-" fnNativeMinus)),
  (Tuple "&<" (CalcitFn "&<" fnNativeLt)),
  (Tuple "&>" (CalcitFn "&>" fnNativeGt)),
  (Tuple "&=" (CalcitFn "&=" fnNativeEq)),
  (Tuple "echo" (CalcitFn "echo" fnNativeEcho)),
  (Tuple "[]" (CalcitFn "[]" fnNativeList)),
  (Tuple "nth" (CalcitFn "[]" fnNativeNth)),
  (Tuple "count" (CalcitFn "count" fnNativeCount)),
  (Tuple "slice" (CalcitFn "slice" fnNativeSlice)),

  (Tuple "defn" (CalcitSyntax "defn" syntaxDefn)),
  (Tuple "if" (CalcitSyntax "if" syntaxIf)),
  (Tuple ";" (CalcitSyntax ";" syntaxComment)),
  (Tuple "&let" (CalcitSyntax ";" syntaxNativeLet)),
  (Tuple "--" (CalcitSyntax "--" syntaxComment))
]