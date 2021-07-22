
module Vehicle.Prelude.Language where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, braces)

-- | Symbols in the language are represented by the `Text` type.
type Symbol = Text

-- | Visibility of function arguments
data Visibility = Explicit | Implicit
  deriving (Eq, Ord, Show)

visBrackets :: Visibility -> (Doc a -> Doc a)
visBrackets Explicit = id
visBrackets Implicit = braces

-- | Literals in the language
data Literal
  = LNat  Natural
  | LInt  Integer
  | LReal Double
  | LBool Bool
  deriving (Eq, Ord, Show)

data PrimitiveNumberType
  = TNat
  | TInt
  | TReal
  deriving (Eq, Ord, Show, Read, Enum)

data PrimitiveTruthType
  = TBool
  | TProp
  deriving (Eq, Ord, Show, Read, Enum)

data PrimitiveType
  = TNumber PrimitiveNumberType
  | TTruth  PrimitiveTruthType
  deriving (Eq, Ord, Show)

data ContainerType
  = TListContainer
  | TTensorContainer
  | TSetContainer
  deriving (Eq, Ord, Show, Enum)
