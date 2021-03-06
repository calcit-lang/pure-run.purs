module Calcit.Builtin.Syntax where

import Calcit.Primes (CalcitData(..), CalcitScope, FnEvalFn)
import Data.Array ((!!))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (bind, pure, (==), (||), ($), (<>), show)

-- | evaluate lines and return value of last line
evaluateLines :: Array CalcitData -> CalcitScope -> FnEvalFn -> Effect CalcitData
evaluateLines xs scope evalFn = case xs !! 0 of
  Just xs0 -> do
    -- log $ "call line: " <> (show xs0)
    v <- evalFn xs0 scope
    let
      follows = Array.drop 1 xs
    if (Array.length follows) == 0 then
      pure v
    else
      evaluateLines (Array.drop 1 xs) scope evalFn
  Nothing -> pure CalcitNil

syntaxDefn :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxDefn xs scope evalFn = do
  nameNode <- case xs !! 0 of
    Just v -> pure v
    Nothing -> throw "function name not found"
  name <- case nameNode of
    CalcitSymbol s ns -> pure s
    _ -> throw "function name not a symbol"
  argsNode <- case xs !! 1 of
    Just v -> pure v
    Nothing -> throw "function args not found"
  args <- case argsNode of
    CalcitList ys -> pure ys
    _ -> throw "function args not list"
  argNames <- traverse extractArgName args
  let
    f = \ys -> evaluateLines (Array.drop 2 xs) (Map.unions [ (foldArgsToScope argNames ys), scope ]) evalFn
  uid <- genUUID
  pure (CalcitFn name uid f)
  where
  extractArgName :: CalcitData -> Effect String
  extractArgName arg = case arg of
    CalcitSymbol s ns -> pure s
    _ -> throw "expected symbol"

syntaxDefmacro :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxDefmacro xs scope evalFn = do
  nameNode <- case xs !! 0 of
    Just v -> pure v
    Nothing -> throw "macro name not found"
  name <- case nameNode of
    CalcitSymbol s ns -> pure s
    _ -> throw "macro name not a symbol"
  argsNode <- case xs !! 1 of
    Just v -> pure v
    Nothing -> throw "macro args not found"
  args <- case argsNode of
    CalcitList ys -> pure ys
    _ -> throw "macro args not list"
  argNames <- traverse extractArgName args
  let
    f =
      ( \ys ->
          let
            bodyScope = Map.unions [ (foldArgsToScope argNames ys), scope ]
          in
            do
              -- log $ "bodyScope: " <> (show bodyScope)
              evaluateLines (Array.drop 2 xs) bodyScope evalFn
      )
  uid <- genUUID
  pure (CalcitMacro name uid f)
  where
  extractArgName :: CalcitData -> Effect String
  extractArgName arg = case arg of
    CalcitSymbol s ns -> pure s
    _ -> throw "expected symbol"

firstOrNil :: Array CalcitData -> CalcitData
firstOrNil ys = case ys !! 0 of
  Just y -> y
  Nothing -> CalcitNil

foldArgsToScope :: Array String -> Array CalcitData -> Map.Map String CalcitData
foldArgsToScope args values = Map.fromFoldable (foldArgs args values [])

foldArgs :: Array String -> Array CalcitData -> Array (Tuple String CalcitData) -> Array (Tuple String CalcitData)
foldArgs args values acc = case (args !! 0), (args !! 1) of
  Nothing, _ -> acc
  Just "&", Just name -> Array.insert (Tuple name (CalcitList values)) acc
  Just s, _ -> foldArgs (Array.drop 1 args) (Array.drop 1 values) (Array.insert (Tuple s (firstOrNil values)) acc)

syntaxIf :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxIf xs scope evalFn = do
  cond <- case xs !! 0 of
    Just v -> evalFn v scope
    Nothing -> throw "cond not found"
  if cond == CalcitNil || cond == CalcitBool false then case xs !! 2 of
    Just falseBranch -> evalFn falseBranch scope
    Nothing -> pure CalcitNil
  else case xs !! 1 of
    Just trueBranch -> evalFn trueBranch scope
    Nothing -> pure CalcitNil

syntaxNativeLet :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxNativeLet xs scope evalFn = do
  pair <- case xs !! 0 of
    Just (CalcitList ys) ->
      if (Array.length ys) == 2 then case (ys !! 0), (ys !! 1) of
        Just (CalcitSymbol s ns), Just v -> pure { k: s, v: v }
        _, _ -> throw "expected symbol in &let"
      else
        throw "expected pair length of 2"
    Just CalcitNil -> pure { k: "_", v: CalcitNil }
    Just a -> throw $ "&let expected a pair, got: " <> (show a)
    Nothing -> throw "&let expected pair in first argument"
  if pair.k == "_" then
    evaluateLines (Array.drop 1 xs) scope evalFn
  else do
    v <- evalFn pair.v scope
    let
      bodyScope = Map.insert pair.k v scope
    evaluateLines (Array.drop 1 xs) bodyScope evalFn

syntaxComment :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxComment _ _ _ = pure CalcitNil

syntaxQuote :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxQuote xs scope evalFn = case xs !! 0 of
  Just x0 -> pure x0
  Nothing -> throw "expected an argument for quote"

syntaxQuasiquote :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxQuasiquote xs scope evalFn = case xs !! 0 of
  Nothing -> throw "quasiquote expected a node"
  Just code -> do
    ret <- replaceCode code
    case ret !! 0 of
      Just v -> pure v
      Nothing -> throw "missing quote expr"
    where
    replaceCode :: CalcitData -> Effect (Array CalcitData)
    replaceCode c = case c of
      CalcitList ys -> case (ys !! 0), (ys !! 1) of
        Just (CalcitSymbol "~" _), Just expr -> do
          v <- evalFn expr scope
          pure [ v ]
        Just (CalcitSymbol "~@" _), Just expr -> do
          ret <- evalFn expr scope
          case ret of
            CalcitList zs -> pure zs
            _ -> throw "unknown result from unquite-slice"
        _, _ -> do
          vv <- traverse replaceCode ys
          -- log $ "vv: " <> (show vv) <> " " <> (show (Array.concat vv))
          pure [ CalcitList (Array.concat vv) ]
      _ -> pure [ c ]

-- | handles tail recursion, only function need this. macros are not supposed to recurse
callFnWithRecur :: (Array CalcitData -> Effect CalcitData) -> Array CalcitData -> Effect CalcitData
callFnWithRecur f xs = do
  ret <- f xs
  case ret of
    CalcitRecur args -> callFnWithRecur f args
    _ -> pure ret

-- | usage `macroexpand-1 (quote (f 1 2))`
-- | notice: preprocess is not implemented here, so, NO macroexpand-all
syntaxMacroexpand1 :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxMacroexpand1 xs scope evalFn = case xs !! 0 of
  Just x0 -> do
    e <- evalFn x0 scope
    case e of
      CalcitList ys -> case ys !! 0 of
        Nothing -> throw "macroexpand-1 expected an expression"
        Just y -> do
          v <- evalFn y scope
          case v of
            CalcitMacro _ _ f -> f (Array.drop 1 ys)
            _ -> throw "macroexpand-1 expected macro operation"
      a -> pure a
  Nothing -> throw "macroexpand-1 expected 1 argument"

syntaxMacroexpand :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxMacroexpand xs scope evalFn = case xs !! 0 of
  Just x0 -> do
    e <- evalFn x0 scope
    case e of
      CalcitList ys -> case ys !! 0 of
        Nothing -> throw "macroexpand expected an expression"
        Just y -> do
          v <- evalFn y scope
          case v of
            CalcitMacro _ _ f -> callFnWithRecur f (Array.drop 1 ys)
            _ -> throw "macroexpand expected macro operation"
      a -> pure a
  Nothing -> throw "macroexpand expected 1 argument"

syntaxEval :: (Array CalcitData) -> CalcitScope -> FnEvalFn -> Effect CalcitData
syntaxEval xs scope evalFn = case xs !! 0 of
  Just x0 -> do
    e <- evalFn x0 scope
    evalFn e scope
  Nothing -> throw "eval expected 1 argument"

coreNsSyntaxes :: Map.Map String CalcitData
coreNsSyntaxes =
  Map.fromFoldable
    [ (Tuple "defmacro" (CalcitSyntax "defmacro" syntaxDefmacro))
    , (Tuple "defn" (CalcitSyntax "defn" syntaxDefn))
    , (Tuple "if" (CalcitSyntax "if" syntaxIf))
    , (Tuple "quote" (CalcitSyntax "quote" syntaxQuote))
    , (Tuple "quasiquote" (CalcitSyntax "quasiquote" syntaxQuasiquote))
    , (Tuple ";" (CalcitSyntax ";" syntaxComment))
    , (Tuple "&let" (CalcitSyntax ";" syntaxNativeLet))
    , (Tuple "--" (CalcitSyntax "--" syntaxComment))
    , (Tuple "macroexpand-1" (CalcitSyntax "macroexpand-1" syntaxMacroexpand1))
    , (Tuple "macroexpand" (CalcitSyntax "macroexpand" syntaxMacroexpand))
    , (Tuple "eval" (CalcitSyntax "eval" syntaxEval))
    ]
