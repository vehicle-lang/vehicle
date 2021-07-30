
module Vehicle.Prelude.Language where

import Numeric.Natural (Natural)
import Prettyprinter (Pretty(..), Doc, braces)

import Vehicle.Prelude.Provenance (HasProvenance(..), Provenance, expandProvenance)
import Vehicle.Prelude.Token (Symbol)

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

-- | Identifiers for top-level declerations
data WithProvenance a = WithProvenance Provenance a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasProvenance (WithProvenance a) where
  prov (WithProvenance p _) = p

-- | Universe levels
type Level = Natural

-- | Visibility of function arguments
data Visibility = Explicit | Implicit
  deriving (Eq, Ord, Show)

visBrackets :: Visibility -> Doc a -> Doc a
visBrackets Explicit = id
visBrackets Implicit = braces

visProv :: Visibility -> Provenance -> Provenance
visProv Explicit = id
visProv Implicit = expandProvenance (1,1)

-- | Literals in the language
data Literal
  = LNat  Natural
  | LInt  Integer
  | LReal Double
  | LBool Bool
  deriving (Eq, Ord, Show)

data NumberType
  = TNat
  | TInt
  | TReal
  deriving (Eq, Ord, Show, Read, Enum)

data TruthType
  = TBool
  | TProp
  deriving (Eq, Ord, Show, Read, Enum)

data ContainerType
  = TList
  | TTensor
  | TSet
  deriving (Eq, Ord, Show, Enum)

data PrimitiveType
  = TNumber NumberType
  | TTruth  TruthType
  deriving (Eq, Ord, Show)
