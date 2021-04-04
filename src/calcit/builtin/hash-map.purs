
module Calcit.Builtin.HashMap where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prelude (bind, pure)

fnNativeHashMap :: (Array CalcitData) -> Effect CalcitData
fnNativeHashMap xs =
  do
    ys <- foldTuples [] xs
    pure (CalcitMap (Map.fromFoldable ys))
  where

    foldTuples :: Array (Tuple CalcitData CalcitData) -> Array CalcitData
       -> Effect (Array (Tuple CalcitData CalcitData))
    foldTuples acc ys = case (ys !! 0), (ys !! 1) of
      Just a, Just b -> pure (Array.snoc acc (Tuple a b))
      Nothing, _ -> pure acc
      Just _, Nothing -> throw "&{} got odd arguments"

-- TODO assoc

-- TODO dissoc

-- TODO merge

-- TODO to-pairs
-- TODO pairs-map

-- TODO map-kv

-- TODO map-maybe

-- TODO keys

-- TODO vals

-- TODO pick-keys
