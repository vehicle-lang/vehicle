-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Core.AST.Builtin
  ( Builtin(..)
  , builtinFromSymbol
  , symbolFromBuiltin
  ) where


import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Standard builtins

-- |Builtins to the Vehicle language
data Builtin
  -- Types
  = Bool
  | Prop
  | Nat
  | Int
  | Real
  | List
  | Tensor
  -- Type classes
  | HasEq
  | HasOrd
  | IsTruth
  | IsNatural
  | IsIntegral
  | IsRational
  | IsReal
  | IsContainer
  | IsQuantifiable
  -- Expressions
  | If
  | Impl
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Le
  | Lt
  | Ge
  | Gt
  | Mul
  | Div
  | Add
  | Sub
  | Neg
  | Cons
  | At
  | Map
  | Fold
  | Quant Quantifier
  deriving (Eq, Ord, Show, Generic, NFData)

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols =
  -- Types
  [ "Bool"         |-> Bool
  , "Prop"         |-> Prop
  , "Nat"          |-> Nat
  , "Int"          |-> Int
  , "Real"         |-> Real
  , "List"         |-> List
  , "Tensor"       |-> Tensor
  -- Type classes
  , "HasEq"        |-> HasEq
  , "HasOrd"       |-> HasOrd
  , "IsTruth"      |-> IsTruth
  , "IsContainer"  |-> IsContainer
  , "IsNatural"    |-> IsNatural
  , "IsIntegral"   |-> IsIntegral
  , "IsRational"   |-> IsRational
  , "IsReal"       |-> IsReal
  , "IsQuantify"   |-> IsQuantifiable
  -- Operations
  , "if"           |-> If
  , "=>"           |-> Impl
  , "and"          |-> And
  , "or"           |-> Or
  , "not"          |-> Not
  , "=="           |-> Eq
  , "!="           |-> Neq
  , "<="           |-> Le
  , "<"            |-> Lt
  , ">="           |-> Ge
  , ">"            |-> Gt
  , "+"            |-> Add
  , "*"            |-> Mul
  , "/"            |-> Div
  , "-"            |-> Sub
  , "~"            |-> Neg -- Negation is changed from "-" to "~" during elaboration.
  , "!"            |-> At
  , "::"           |-> Cons
  , "every"        |-> Quant All
  , "some"         |-> Quant Any
  , "map"          |-> Map
  , "fold"         |-> Fold
  ]

builtinFromSymbol :: Symbol -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Maybe Symbol
symbolFromBuiltin builtin = lookup' builtin builtinSymbols

lookup' :: (Eq b) => b -> [(a,b)] -> Maybe a
lookup' _ [] =  Nothing
lookup' key ((x,y):xys)
  | key == y  =  Just x
  | otherwise =  lookup' key xys