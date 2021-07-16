
module Vehicle.Prelude.Language where

import Data.Text (Text)

-- |Symbols in the language are represented by the `Text` type.
type Symbol = Text

-- | Visibility of function arguments
data Visibility = Explicit | Implicit
  deriving (Eq, Ord, Show)

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
