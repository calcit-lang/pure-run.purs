
module Calcit.Primes where

import Data.Tuple
import Data.Map

import Cirru.Edn (CirruEdn(..))
import Cirru.Node (CirruNode(..))

data CalcitData = CalcitNil
                 | CalcitBool Boolean
                 | CalcitNumber Number
                 | CalcitSymbol String
                 | CalcitKeyword String
                 | CalcitString String
                 | CalcitList (Array CalcitData)
                 | CalcitMap (Map CalcitData CalcitData)
                 -- | CalcitAtom CalcitData
                 -- | CalcitSet (Set CalcitData)
                 -- | CalcitRecord String (Array String) (Array CalcitData)
                 | CalcitFn (Array CirruNode) (Array CirruNode)
