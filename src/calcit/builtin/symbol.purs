
module Calcit.Builtin.Symbol where


import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prelude ((+), bind, pure, ($), (<>), show, discard)

mockedNs :: String
mockedNs = "calcit.gen"

-- ditry https://wiki.haskell.org/Top_level_mutable_state
symbolGenCounterRef :: Ref Int
symbolGenCounterRef = unsafePerformEffect (Ref.new 0)

takeGenCounter :: Effect Int
takeGenCounter = do
  idx <- Ref.modify (\x -> x + 1) symbolGenCounterRef
  pure idx

fnNativeGensym :: (Array CalcitData) -> Effect CalcitData
fnNativeGensym xs = case xs !! 0 of
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

fnNativeResetGensymIndex :: (Array CalcitData) -> Effect CalcitData
fnNativeResetGensymIndex xs = do
  Ref.write 0 symbolGenCounterRef
  pure CalcitNil

fnNativeTypeOf :: (Array CalcitData) -> Effect CalcitData
fnNativeTypeOf xs = case xs !! 0 of
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

fnNativeRecur :: (Array CalcitData) -> Effect CalcitData
fnNativeRecur xs = pure (CalcitRecur xs)
