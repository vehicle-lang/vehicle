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
  = Kind
  | Type
  | Constraint
  | PrimitiveType PrimitiveType
  | List
  | Tensor
  | Implements Constraint
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

pattern PrimitiveNumber :: PrimitiveNumber -> Builtin
pattern PrimitiveNumber prim = PrimitiveType (Number prim)

pattern PrimitiveTruth :: PrimitiveTruth -> Builtin
pattern PrimitiveTruth prim = PrimitiveType (Truth prim)

data Constraint
  = HasEq
  | HasOrd
  | IsContainer
  | IsNumber
  deriving (Eq, Ord, Show)

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols =
  -- Types
  [ "Kind"       |-> Kind
  , "Type"       |-> Type
  , "Constraint" |-> Constraint
  , "Bool"       |-> PrimitiveTruth Bool
  , "Prop"       |-> PrimitiveTruth Prop
  , "Nat"        |-> PrimitiveNumber Nat
  , "Int"        |-> PrimitiveNumber Int
  , "Real"       |-> PrimitiveNumber Real
  , "List"       |-> List
  , "Tensor"     |-> Tensor
  -- Constraints
  , "Eq"         |-> Implements HasEq
  , "Ord"        |-> Implements HasOrd
  , "Container"  |-> Implements IsContainer
  , "Number"     |-> Implements IsNumber
  -- Operations
  , "if"         |-> If
  , "=>"         |-> Impl
  , "and"        |-> And
  , "or"         |-> Or
  , "not"        |-> Not
  , "=="         |-> Eq
  , "!="         |-> Neq
  , "<="         |-> Le
  , "<"          |-> Lt
  , ">="         |-> Ge
  , ">"          |-> Gt
  , "*"          |-> Mul
  , "/"          |-> Div
  , "-"          |-> Sub
  , "~"          |-> Neg -- Negation is changed from "-" to "~" during elaboration.
  , "!"          |-> At
  , "::"         |-> Cons
  , "all"        |-> All
  , "any"        |-> Any
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