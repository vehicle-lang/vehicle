
module Vehicle.Prelude.Primitive where

data PrimitiveNumber
  = Nat
  | Int
  | Real
  deriving (Eq, Ord, Show, Read)

data PrimitiveTruth
  = Bool
  | Prop
  deriving (Eq, Ord, Show, Read)

data PrimitiveType
  = Number PrimitiveNumber
  | Truth  PrimitiveTruth
  deriving (Eq, Ord, Show)

data PrimitiveContainer
  = ListContainer
  | TensorContainer
  | SetContainer
  deriving (Eq, Ord, Show)