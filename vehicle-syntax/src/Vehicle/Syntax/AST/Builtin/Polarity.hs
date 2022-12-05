module Vehicle.Syntax.AST.Builtin.Polarity where

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))

import Vehicle.Syntax.AST.Builtin.Core
import Vehicle.Syntax.AST.Provenance

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

instance ToJSON   PolarityProvenance
instance FromJSON PolarityProvenance

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
  deriving (Eq, Generic, Show)

instance NFData   Polarity
instance Hashable Polarity
instance ToJSON   Polarity
instance FromJSON Polarity

instance Pretty Polarity where
  pretty = \case
    Unquantified      -> "Unquantified"
    Quantified q _    -> "Quantified" <+> pretty q
    MixedParallel{}   -> "MixedParallel"
    MixedSequential{} -> "MixedSequential"

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

instance ToJSON   PolarityTypeClass
instance FromJSON PolarityTypeClass
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
