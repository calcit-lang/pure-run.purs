
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