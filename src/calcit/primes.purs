
module Calcit.Primes where

import Data.Eq
import Data.Map
import Data.Ord
import Data.Semigroup
import Data.Show
import Data.Tuple
import Cirru.Edn (CirruEdn(..))
import Cirru.Node (CirruNode(..))
import Data.Either (Either(..))
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Number.Format (toString)
import Data.Set (Set)
import Data.Set as DataSet
import Data.String as String
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect (Effect(..))
import Prelude ((&&))

type FnEvalFn = CalcitData -> CalcitScope -> Effect CalcitData

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
                 | CalcitFn String (Array CalcitData -> Effect CalcitData)
                 | CalcitSyntax String (Array CalcitData -> CalcitScope -> FnEvalFn -> Effect CalcitData)

instance showCalcitData :: Show CalcitData where
  show CalcitNil = "nil"
  show (CalcitBool true) = "true"
  show (CalcitBool false) = "false"
  show (CalcitNumber n) = toString n
  show (CalcitSymbol s) = "'" <> s
  show (CalcitKeyword s) = ":" <> s
  show (CalcitString s) = "|" <> s -- TODO handle formatting with spaces
  show (CalcitList xs) = "([] " <> (String.joinWith " " (Functor.map show xs))  <> ")"
  show (CalcitMap xs) = "(TODO Map)"
  -- show (CalcitAtom a) = "TODO"
  show (CalcitSet xs) = "(TODO Set)"
  show (CalcitRecord name fields values) = "(TODO Record)"
  show (CalcitFn _ _) = "(TODO Fn)"
  show (CalcitSyntax _ _) = "(TODO Syntax)"

ednToCalcit :: CirruEdn -> CalcitData
ednToCalcit d = case d of
  CrEdnNil -> CalcitNil
  CrEdnBool x -> CalcitBool x
  CrEdnNumber x -> CalcitNumber x
  CrEdnSymbol x -> CalcitSymbol x
  CrEdnKeyword x -> CalcitKeyword x
  CrEdnString x -> CalcitString x
  CrEdnList xs -> CalcitList (Functor.map ednToCalcit xs)
  CrEdnSet xs -> CalcitSet (DataSet.map ednToCalcit xs)
  CrEdnMap xs -> CalcitNil -- TODO
  CrEdnRecord name fields values -> CalcitRecord name fields (Functor.map ednToCalcit values)
  CrEdnQuote xs -> cirruToCalcit xs

-- | tests if thats a float
matchFloat :: String -> Boolean
matchFloat s = case (regex "^-?(\\d+)(\\.\\d*)?$" noFlags) of
  Right pattern -> test pattern s
  Left failure -> false

cirruToCalcit :: CirruNode -> CalcitData
cirruToCalcit node = case node of
  CirruLeaf "nil" -> CalcitNil
  CirruLeaf "true" -> CalcitBool true
  CirruLeaf "false" -> CalcitBool false
  CirruLeaf s -> case String.take 1 s of
    ":" -> CalcitKeyword (String.drop 1 s)
    "|" -> CalcitString (String.drop 1 s)
    "\"" -> CalcitString (String.drop 1 s)
    "'" -> CalcitList [CalcitSymbol "quote", (cirruToCalcit (CirruLeaf (String.drop 1 s)))]
    _ -> if matchFloat s
      then case Number.fromString s of
        Just n -> CalcitNumber n
        Nothing -> CalcitSymbol s
      else CalcitSymbol s
  CirruList xs -> CalcitList (Functor.map cirruToCalcit xs)

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
  eq (CalcitFn name1 _) (CalcitFn name2 _) = name1 == name2 -- TODO inaccurate
  eq (CalcitSyntax name1 _) (CalcitSyntax name2 _) = name1 == name2 -- TODO skip fn comparing

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

  compare (CalcitFn name1 _) (CalcitFn name2 _) = compare name1 name2 -- TODO inaccurate
  compare (CalcitFn name1 _) _  = LT
  compare _ (CalcitFn name1 _)  = GT

  compare (CalcitSyntax name1 _) (CalcitSyntax name2 _) = compare name1 name2 -- skip fn comparing
  compare (CalcitSyntax name1 _) _  = LT
  compare _ (CalcitSyntax name1 _)  = GT

type CalcitScope = Map String CalcitData

type EdnFailure = { message :: String, edn :: CirruEdn }

type CalcitFailure = { message :: String, data :: CalcitData }

type ProgramOverview = Map String (Set String)