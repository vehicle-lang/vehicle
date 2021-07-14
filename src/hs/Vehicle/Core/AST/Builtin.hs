-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Core.AST.Builtin
  ( Builtin(..)
  , AbstractBuiltinOp(..)
  , ConcreteBuiltinOp(..)
  , pattern PrimitiveNumber
  , pattern PrimitiveTruth
  , builtinFromSymbol
  , symbolFromBuiltin
  , symbolFromConcreteBuiltin
  ) where

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Standard builtins

-- |Builtins to the Vehicle language
data Builtin builtinOp
  = PrimitiveType PrimitiveType
  | List
  | Tensor
  | Op builtinOp
  deriving (Eq, Ord, Show)

pattern PrimitiveNumber :: PrimitiveNumber -> Builtin builtinOp
pattern PrimitiveNumber prim = PrimitiveType (Number prim)

pattern PrimitiveTruth :: PrimitiveTruth -> Builtin builtinOp
pattern PrimitiveTruth prim = PrimitiveType (Truth prim)

-- |Builtin ops which we do not yet know what concrete type
-- they are being applied to.
data AbstractBuiltinOp
  = If
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

builtinSymbols :: [(Symbol, Builtin AbstractBuiltinOp)]
builtinSymbols =
  [ "Bool"   |-> PrimitiveTruth Bool
  , "Prop"   |-> PrimitiveTruth Prop
  , "Nat"    |-> PrimitiveNumber Nat
  , "Int"    |-> PrimitiveNumber Int
  , "Real"   |-> PrimitiveNumber Real
  , "List"   |-> List
  , "Tensor" |-> Tensor
  , "if"     |-> Op If
  , "=>"     |-> Op Impl
  , "and"    |-> Op And
  , "or"     |-> Op Or
  , "not"    |-> Op Not
  , "=="     |-> Op Eq
  , "!="     |-> Op Neq
  , "<="     |-> Op Le
  , "<"      |-> Op Lt
  , ">="     |-> Op Ge
  , ">"      |-> Op Gt
  , "*"      |-> Op Mul
  , "/"      |-> Op Div
  , "-"      |-> Op Sub
  -- Negation is changed from "-" to "~" during elaboration.
  , "~"      |-> Op Neg
  , "!"      |-> Op At
  , "::"     |-> Op Cons
  , "all"    |-> Op All
  , "any"    |-> Op Any
  ]


builtinFromSymbol :: Symbol -> Maybe (Builtin AbstractBuiltinOp)
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin AbstractBuiltinOp -> Maybe Symbol
symbolFromBuiltin builtin = lookup' builtin builtinSymbols

lookup' :: (Eq b) => b -> [(a,b)] -> Maybe a
lookup' _ [] =  Nothing
lookup' key ((x,y):xys)
  | key == y  =  Just x
  | otherwise =  lookup' key xys



--------------------------------------------------------------------------------
-- Concrete builtins

-- TODO Wen, I'm less and less sure these are needed now that we have type
-- classes but keeping them for now.

-- |Builtin operations for which it is known what type they're
-- being applied to.
data ConcreteBuiltinOp
  = ConcIf
  | ConcImpl PrimitiveTruth
  | ConcAnd  PrimitiveTruth
  | ConcOr   PrimitiveTruth
  | ConcNot  PrimitiveTruth
  | ConcEq   PrimitiveType   PrimitiveTruth
  | ConcNeq  PrimitiveType   PrimitiveTruth
  | ConcLe   PrimitiveNumber PrimitiveTruth
  | ConcLt   PrimitiveNumber PrimitiveTruth
  | ConcGe   PrimitiveNumber PrimitiveTruth
  | ConcGt   PrimitiveNumber PrimitiveTruth
  | ConcMul  PrimitiveNumber
  | ConcDiv  PrimitiveNumber
  | ConcAdd  PrimitiveNumber
  | ConcSub  PrimitiveNumber
  | ConcNeg  PrimitiveNumber
  | ConcCons
  | ConcAt   PrimitiveContainer
  | ConcAll  PrimitiveContainer PrimitiveTruth
  | ConcAny  PrimitiveContainer PrimitiveTruth
  deriving (Eq, Ord, Show)

abstractBuiltin :: Builtin ConcreteBuiltinOp -> Builtin AbstractBuiltinOp
abstractBuiltin = \case
  PrimitiveType t -> PrimitiveType t
  List            -> List
  Tensor          -> Tensor
  Op builtinOp    -> Op $ abstractOp builtinOp

abstractOp :: ConcreteBuiltinOp -> AbstractBuiltinOp
abstractOp = \case
  ConcIf       -> If
  ConcImpl _   -> Impl
  ConcAnd  _   -> And
  ConcOr   _   -> Or
  ConcNot  _   -> Not
  ConcEq   _ _ -> Eq
  ConcNeq  _ _ -> Neq
  ConcLe   _ _ -> Le
  ConcLt   _ _ -> Lt
  ConcGe   _ _ -> Ge
  ConcGt   _ _ -> Gt
  ConcMul  _   -> Mul
  ConcDiv  _   -> Div
  ConcAdd  _   -> Add
  ConcSub  _   -> Sub
  ConcNeg  _   -> Neg
  ConcCons     -> Cons
  ConcAt   _   -> At
  ConcAll  _ _ -> All
  ConcAny  _ _ -> Any

symbolFromConcreteBuiltin :: Builtin ConcreteBuiltinOp -> Maybe Symbol
symbolFromConcreteBuiltin = symbolFromBuiltin . abstractBuiltin