module Calcit.Builtin.Symbol where

import Calcit.Globals (symbolGenCounterRef)
import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Prelude ((+), bind, pure, ($), (<>), show, discard)

mockedNs :: String
mockedNs = "calcit.gen"

takeGenCounter :: Effect Int
takeGenCounter = do
  idx <- Ref.modify (\x -> x + 1) symbolGenCounterRef
  pure idx

procGensym :: (Array CalcitData) -> Effect CalcitData
procGensym xs = case xs !! 0 of
  Just (CalcitString s) -> do
    idx <- takeGenCounter
    pure (CalcitSymbol (s <> "__" <> (show idx)) mockedNs)
  Just (CalcitKeyword s) -> do
    idx <- takeGenCounter
    pure (CalcitSymbol (s <> "__" <> (show idx)) mockedNs)
  Just (CalcitSymbol s ns) -> do
    idx <- takeGenCounter
    pure (CalcitSymbol (s <> "__" <> (show idx)) ns)
  Just a -> throw $ "unexpected alias for gensym" <> (show a)
  Nothing -> do
    idx <- takeGenCounter
    pure (CalcitSymbol ("G__" <> (show idx)) mockedNs)

procResetGensymIndex :: (Array CalcitData) -> Effect CalcitData
procResetGensymIndex xs = do
  Ref.write 0 symbolGenCounterRef
  pure CalcitNil

procTypeOf :: (Array CalcitData) -> Effect CalcitData
procTypeOf xs = case xs !! 0 of
  Nothing -> throw "type-of expected 1 argument"
  Just x -> case x of
    CalcitNil -> pure (CalcitKeyword "nil")
    CalcitBool _ -> pure (CalcitKeyword "bool")
    CalcitNumber _ -> pure (CalcitKeyword "number")
    CalcitSymbol _ _ -> pure (CalcitKeyword "symbol")
    CalcitString _ -> pure (CalcitKeyword "string")
    CalcitKeyword _ -> pure (CalcitKeyword "keyword")
    CalcitRef _ _ -> pure (CalcitKeyword "ref")
    CalcitRecur _ -> pure (CalcitKeyword "recur")
    CalcitList _ -> pure (CalcitKeyword "list")
    CalcitMap _ -> pure (CalcitKeyword "map")
    CalcitSet _ -> pure (CalcitKeyword "set")
    CalcitRecord _ _ _ -> pure (CalcitKeyword "record")
    CalcitMacro _ _ _ -> pure (CalcitKeyword "macro")
    CalcitFn _ _ _ -> pure (CalcitKeyword "fn")
    CalcitSyntax _ _ -> pure (CalcitKeyword "syntax")

procRecur :: (Array CalcitData) -> Effect CalcitData
procRecur xs = pure (CalcitRecur xs)
