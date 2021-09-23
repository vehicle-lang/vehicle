module Vehicle.Prelude.Language where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty(..))

import Vehicle.Prelude.Provenance (HasProvenance(..), Provenance)
import Vehicle.Prelude.Token (Symbol)

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData Identifier

-- | Identifiers for top-level declerations
data WithProvenance a = WithProvenance Provenance a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (WithProvenance a)

deProv :: WithProvenance a -> a
deProv (WithProvenance _ a) = a

instance HasProvenance (WithProvenance a) where
  prov (WithProvenance p _) = p

-- | Universe levels
type Level = Natural


-- | Literals in the language
data Literal
  = LNat  Int
  | LInt  Int
  | LRat  Double
  | LBool Bool
  deriving (Eq, Ord, Show, Generic)

instance NFData Literal

-- | Property-level quantifiers supported
data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Show, Generic)

instance NFData Quantifier

