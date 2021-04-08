-- | Global Refs relies on unsafe ops, so maintaining in a special place
module Calcit.Globals where

import Calcit.Primes (CalcitData)
import Data.Map as Map
import Data.UUID (UUID, genUUID)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

-- | dirty way creating an UUID to be used at top level
uidSeed :: UUID
uidSeed = unsafePerformEffect (genUUID)

-- | real program state
-- ditry https://wiki.haskell.org/Top_level_mutable_state
programEvaledDataRef :: Ref.Ref (Map.Map String (Map.Map String CalcitData))
programEvaledDataRef = unsafePerformEffect (Ref.new (Map.fromFoldable []))

-- | hold environment information, globally
programRuntimeEnvsRef ::
  Ref.Ref
    { sourcePath :: String
    }
programRuntimeEnvsRef = unsafePerformEffect (Ref.new { sourcePath: "." })

-- ditry https://wiki.haskell.org/Top_level_mutable_state
symbolGenCounterRef :: Ref.Ref Int
symbolGenCounterRef = unsafePerformEffect (Ref.new 0)
