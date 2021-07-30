-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Core.AST.Builtin
  ( Builtin(..)
  , pattern PrimitiveNumber
  , pattern PrimitiveTruth
  , builtinFromSymbol
  , symbolFromBuiltin
  ) where

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Standard builtins

-- |Builtins to the Vehicle language
data Builtin
  -- Types
  = PrimitiveType PrimitiveType
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
  | All
  | Any
  deriving (Eq, Ord, Show)

pattern PrimitiveNumber :: NumberType -> Builtin
pattern PrimitiveNumber prim = PrimitiveType (TNumber prim)

pattern PrimitiveTruth :: TruthType -> Builtin
pattern PrimitiveTruth prim = PrimitiveType (TTruth prim)

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols =
  -- Types
  [ "Bool"         |-> PrimitiveTruth TBool
  , "Prop"         |-> PrimitiveTruth TProp
  , "Nat"          |-> PrimitiveNumber TNat
  , "Int"          |-> PrimitiveNumber TInt
  , "Real"         |-> PrimitiveNumber TReal
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
  , "*"            |-> Mul
  , "/"            |-> Div
  , "-"            |-> Sub
  , "~"            |-> Neg -- Negation is changed from "-" to "~" during elaboration.
  , "!"            |-> At
  , "::"           |-> Cons
  , "all"          |-> All
  , "any"          |-> Any
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