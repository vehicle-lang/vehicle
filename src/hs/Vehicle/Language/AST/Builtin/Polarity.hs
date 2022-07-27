module Vehicle.Language.AST.Builtin.Polarity where

import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Language.AST.Provenance
import Vehicle.Language.AST.Builtin.Core

--------------------------------------------------------------------------------
-- PolarityProvenance

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance Provenance
  | NegateProvenance     Provenance PolarityProvenance
  | LHSImpliesProvenance Provenance PolarityProvenance
  | EqProvenance         EqualityOp Provenance PolarityProvenance
  deriving (Generic)

instance Show PolarityProvenance where
  show _x = ""

instance Eq PolarityProvenance where
  _x == _y = True

instance NFData PolarityProvenance where
  rnf _x = ()

instance Hashable PolarityProvenance where
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- Polarity

-- | Used to annotate boolean types, represents what sort of pattern of
-- quantifiers it contains.
data Polarity
  = Unquantified
  | Quantified Quantifier PolarityProvenance
  -- | Stores the provenance of the `Forall` first followed by the `Exists`.
  | MixedParallel PolarityProvenance PolarityProvenance
  -- | Stores the type and provenance of the top-most quantifier first.
  | MixedSequential Quantifier Provenance PolarityProvenance
  deriving (Eq, Generic)

instance NFData Polarity
instance Hashable Polarity

instance Pretty Polarity where
  pretty = \case
    Unquantified      -> "Unquantified"
    Quantified q _    -> "Quantified" <+> pretty q
    MixedParallel{}   -> "MixedParallel"
    MixedSequential{} -> "MixedSequential"

instance Show Polarity where
  show = layoutAsString . pretty
