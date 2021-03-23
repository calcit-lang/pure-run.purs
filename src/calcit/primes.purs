
module Calcit.Primes where

import Data.Tuple
import Data.Map
import Data.Show
import Data.Ord
import Data.Eq
import Data.Set as DataSet
import Data.Functor as DataFunctor
import Data.Set (Set)
import Data.Semigroup

import Prelude ((&&))

import Data.Number.Format (toString)

import Cirru.Edn (CirruEdn(..))
import Cirru.Node (CirruNode(..))

data CalcitData = CalcitNil
                 | CalcitBool Boolean
                 | CalcitNumber Number
                 | CalcitSymbol String
                 | CalcitKeyword String
                 | CalcitString String
                 -- TODO use sequence later
                 | CalcitList (Array CalcitData)
                 | CalcitMap (Map CalcitData CalcitData)
                 -- | CalcitAtom CalcitData
                 | CalcitSet (Set CalcitData)
                 | CalcitRecord String (Array String) (Array CalcitData)
                 -- TODO scope
                 | CalcitFn String (Array CirruNode) (Array CirruNode)

instance showCalcitData :: Show CalcitData where
  show CalcitNil = "nil"
  show (CalcitBool true) = "true"
  show (CalcitBool false) = "false"
  show (CalcitNumber n) = toString n
  show (CalcitSymbol s) = "'" <> s
  show (CalcitKeyword s) = ":" <> s
  show (CalcitString s) = s
  show (CalcitList xs) = "TODO List"
  show (CalcitMap xs) = "TODO Map"
  -- show (CalcitAtom a) = "TODO"
  show (CalcitSet xs) = "TODO Set"
  show (CalcitRecord name fields values) = "TODO Record"
  show (CalcitFn name args body) = "TODO fn"

ednToCalcit :: CirruEdn -> CalcitData
ednToCalcit d = case d of
  CrEdnNil -> CalcitNil
  CrEdnBool x -> CalcitBool x
  CrEdnNumber x -> CalcitNumber x
  CrEdnSymbol x -> CalcitSymbol x
  CrEdnKeyword x -> CalcitKeyword x
  CrEdnString x -> CalcitString x
  CrEdnList xs -> CalcitList (DataFunctor.map ednToCalcit xs)
  CrEdnSet xs -> CalcitSet (DataSet.map ednToCalcit xs)
  CrEdnMap xs -> CalcitNil -- TODO
  CrEdnRecord name fields values -> CalcitRecord name fields (DataFunctor.map ednToCalcit values)
  CrEdnQuote xs -> cirruToCalcit xs

cirruToCalcit :: CirruNode -> CalcitData
cirruToCalcit node = case node of
  CirruLeaf s -> CalcitSymbol s
  CirruList xs -> CalcitList (DataFunctor.map cirruToCalcit xs)

instance eqCalcitData :: Eq CalcitData where
  eq CalcitNil CalcitNil = true
  eq (CalcitBool x) (CalcitBool y) = x == y
  eq (CalcitNumber x) (CalcitNumber y) = x == y
  eq (CalcitSymbol x) (CalcitSymbol y) = x == y
  eq (CalcitKeyword x) (CalcitKeyword y) = x == y
  eq (CalcitString x) (CalcitString y) = x == y
  eq (CalcitList x) (CalcitList y) = x == y
  eq (CalcitSet x) (CalcitSet y) = x == y
  eq (CalcitMap x) (CalcitMap y) = x == y
  eq (CalcitRecord name1 fields1 values1) (CalcitRecord name2 fields2 values2) = name1 == name2 &&
    fields1 == fields2 && values1 == values2
  eq (CalcitFn name1 args1 body1) (CalcitFn name2 args2 body2) = name1 == name2 && args1 == args2 && body1 == body2

  eq _ _ = false

instance ordCalcitData :: Ord CalcitData where
  compare CalcitNil CalcitNil = EQ
  compare CalcitNil _         = LT
  compare _ CalcitNil         = GT

  compare (CalcitBool false) (CalcitBool true)  = LT
  compare (CalcitBool true) (CalcitBool false)  = LT
  compare (CalcitBool _) (CalcitBool _)         = EQ
  compare (CalcitBool _) _                      = LT
  compare _ (CalcitBool _)                      = GT

  compare (CalcitNumber x) (CalcitNumber y) = compare x y
  compare (CalcitNumber x) _                = LT
  compare _ (CalcitNumber x)                = GT

  compare (CalcitSymbol x) (CalcitSymbol y) = compare x y
  compare (CalcitSymbol x) _                = LT
  compare _ (CalcitSymbol x)                = GT

  compare (CalcitKeyword x) (CalcitKeyword y) = compare x y
  compare (CalcitKeyword x) _                = LT
  compare _ (CalcitKeyword x)                = GT

  compare (CalcitString x) (CalcitString y) = compare x y
  compare (CalcitString x) _                = LT
  compare _ (CalcitString x)                = GT

  compare (CalcitList xs) (CalcitList ys) = compare xs ys
  compare (CalcitList xs) _               = LT
  compare _ (CalcitList xs)               = GT

  compare (CalcitSet xs) (CalcitSet ys) = compare xs ys
  compare (CalcitSet xs) _              = LT
  compare _ (CalcitSet xs)              = GT

  compare (CalcitMap xs) (CalcitMap ys) = compare xs ys
  compare (CalcitMap xs) _              = LT
  compare _ (CalcitMap xs)              = GT

  compare (CalcitRecord name1 fields1 values1) (CalcitRecord name2 fields2 values2) = case compare name1 name2 of
    LT -> LT
    GT -> GT
    EQ -> case compare fields1 fields2 of
      LT -> LT
      GT -> GT
      EQ -> compare values1 values2
  compare (CalcitRecord _ _ _) _ = LT
  compare _ (CalcitRecord _ _ _) = GT

  compare (CalcitFn name1 args1 body1) (CalcitFn name2 args2 body2) = case compare name2 name2 of
    LT -> LT
    GT -> GT
    EQ -> case compare args1 args2 of
      LT -> LT
      GT -> GT
      EQ -> compare body1 body2

type CalcitScope = Map String CalcitData
