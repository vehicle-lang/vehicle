module Vehicle.Language.AST.Builtin.Polarity where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)

import Vehicle.Language.AST.Builtin.Core
import Vehicle.Language.AST.Provenance
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- PolarityProvenance

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance     Provenance
  | NegateProvenance         Provenance PolarityProvenance
  | LHSImpliesProvenance     Provenance PolarityProvenance
  | EqProvenance             Provenance PolarityProvenance EqualityOp
  | PolFunctionProvenance    Provenance PolarityProvenance FunctionPosition
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

mapPolarityProvenance :: (PolarityProvenance -> PolarityProvenance) -> Polarity -> Polarity
mapPolarityProvenance f = \case
  Unquantified           -> Unquantified
  Quantified q pp        -> Quantified q (f pp)
  MixedParallel pp1 pp2  -> MixedParallel (f pp1) (f pp2)
  -- At the moment we don't change non-linear provenance because we
  -- want the minimal example.
  MixedSequential q p pp -> MixedSequential q p pp

--------------------------------------------------------------------------------
-- Polarity constraint

data PolarityTypeClass
  = NegPolarity
  | AddPolarity Quantifier
  | EqPolarity EqualityOp
  | ImpliesPolarity
  | MaxPolarity
  | FunctionPolarity FunctionPosition
  | IfCondPolarity
  deriving (Eq, Generic, Show)

instance NFData   PolarityTypeClass
instance Hashable PolarityTypeClass

instance Pretty PolarityTypeClass where
  pretty = \case
    NegPolarity        -> "NegPolarity"
    AddPolarity q      -> "AddPolarity" <+> pretty q
    EqPolarity eq      -> "EqPolarity" <+> pretty eq
    ImpliesPolarity    -> "ImpliesPolarity"
    MaxPolarity        -> "MaxPolarity"
    IfCondPolarity     -> "IfCondPolarity"
    FunctionPolarity p -> "FunctionPolarity" <> pretty p