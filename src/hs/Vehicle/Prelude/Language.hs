
module Vehicle.Prelude.Language where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty(..), Doc, braces)

import Vehicle.Prelude.Provenance (HasProvenance(..), Provenance, expandProvenance)
import Vehicle.Prelude.Token (Symbol)

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show, Generic, NFData)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

-- | Identifiers for top-level declerations
data WithProvenance a = WithProvenance Provenance a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

instance HasProvenance (WithProvenance a) where
  prov (WithProvenance p _) = p

-- | Universe levels
type Level = Natural


-- | Literals in the language
data Literal
  = LNat  Natural
  | LInt  Integer
  | LReal Double
  | LBool Bool
  deriving (Eq, Ord, Show, Generic, NFData)