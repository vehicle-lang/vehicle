
module Vehicle.Prelude.Language where

import Numeric.Natural (Natural)
import Prettyprinter (Doc, braces)

import Vehicle.Prelude.Provenance (HasProvenance(..), Provenance, expandProvenance)
import Vehicle.Prelude.Token (Symbol)

-- | Identifiers for top-level declerations
data DeclIdentifier = DeclIdentifier Provenance Symbol
  deriving (Eq, Ord, Show)

instance HasProvenance DeclIdentifier where
  prov (DeclIdentifier p _) = p

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
