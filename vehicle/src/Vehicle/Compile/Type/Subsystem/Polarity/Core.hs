module Vehicle.Compile.Type.Subsystem.Polarity.Core where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Compile.Type.Core
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Polarity types

data PolarityType
  = Polarity Polarity
  | PolarityTypeClass PolarityTypeClass
  deriving (Show, Eq)

instance Pretty PolarityType where
  pretty = \case
    PolarityTypeClass tc -> pretty tc
    Polarity l -> pretty l

type PolarityBuiltin = NormalisableBuiltin PolarityType

--------------------------------------------------------------------------------
-- PolarityProvenance

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance Provenance
  | NegateProvenance Provenance PolarityProvenance
  | LHSImpliesProvenance Provenance PolarityProvenance
  | EqProvenance Provenance PolarityProvenance EqualityOp
  | PolFunctionProvenance Provenance PolarityProvenance FunctionPosition
  deriving (Generic)

instance ToJSON PolarityProvenance

instance Serialize PolarityProvenance

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
  | -- | Stores the provenance of the `Forall` first followed by the `Exists`.
    MixedParallel PolarityProvenance PolarityProvenance
  | -- | Stores the type and provenance of the top-most quantifier first.
    MixedSequential Quantifier Provenance PolarityProvenance
  deriving (Eq, Generic, Show)

instance NFData Polarity

instance Hashable Polarity

instance ToJSON Polarity

instance Serialize Polarity

instance Pretty Polarity where
  pretty = \case
    Unquantified -> "Unquantified"
    Quantified q _ -> "Quantified" <+> pretty q
    MixedParallel {} -> "MixedParallel"
    MixedSequential {} -> "MixedSequential"

mapPolarityProvenance :: (PolarityProvenance -> PolarityProvenance) -> Polarity -> Polarity
mapPolarityProvenance f = \case
  Unquantified -> Unquantified
  Quantified q pp -> Quantified q (f pp)
  MixedParallel pp1 pp2 -> MixedParallel (f pp1) (f pp2)
  -- At the moment we don't change non-linear provenance because we
  -- want the minimal example.
  MixedSequential q p pp -> MixedSequential q p pp

--------------------------------------------------------------------------------
-- Polarity constraint

data PolarityTypeClass
  = NegPolarity
  | QuantifierPolarity Quantifier
  | AddPolarity Quantifier
  | EqPolarity EqualityOp
  | ImpliesPolarity
  | IfPolarity
  | MaxPolarity
  | FunctionPolarity FunctionPosition
  deriving (Eq, Generic, Show)

instance ToJSON PolarityTypeClass

instance Serialize PolarityTypeClass

instance NFData PolarityTypeClass

instance Hashable PolarityTypeClass

instance Pretty PolarityTypeClass where
  pretty = \case
    NegPolarity -> "NegPolarity"
    AddPolarity q -> "AddPolarity" <+> pretty q
    QuantifierPolarity q -> "QuantifierPolarity" <+> pretty q
    EqPolarity eq -> "EqPolarity" <+> pretty eq
    ImpliesPolarity -> "ImpliesPolarity"
    MaxPolarity -> "MaxPolarity"
    IfPolarity -> "IfPolarity"
    FunctionPolarity p -> "FunctionPolarity" <> pretty p

-----------------------------------------------------------------------------
-- Type synonyms

-- Constraint
type PolarityConstraintProgress = ConstraintProgress PolarityType

type PolarityTypeClassConstraint = TypeClassConstraint PolarityType

type PolarityUnificationConstraint = UnificationConstraint PolarityType

type PolarityConstraintContext = ConstraintContext PolarityType

type PolarityConstraint = Constraint PolarityType

-- Value
type PolarityNormExpr = Value PolarityType

type PolarityNormBinder = VBinder PolarityType

type PolarityNormArg = VArg PolarityType

type PolarityNormType = VType PolarityType

type PolaritySpine = Spine PolarityType

type PolarityEnv = Env PolarityType

pattern PolarityExpr :: Provenance -> Polarity -> Expr var (NormalisableBuiltin PolarityType)
pattern PolarityExpr p pol = Builtin p (CType (Polarity pol))

pattern VPolarityExpr :: Polarity -> PolarityNormExpr
pattern VPolarityExpr l <- VBuiltin (CType (Polarity l)) []
  where
    VPolarityExpr l = VBuiltin (CType (Polarity l)) []
