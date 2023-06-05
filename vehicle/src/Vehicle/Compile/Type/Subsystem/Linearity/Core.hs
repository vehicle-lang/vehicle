module Vehicle.Compile.Type.Subsystem.Linearity.Core
  ( module Vehicle.Compile.Type.Subsystem.Linearity.Core,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Compile.Type.Core
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Linearity type

data LinearityType
  = LinearityTypeClass LinearityTypeClass
  | Linearity Linearity
  deriving (Show, Eq)

instance Pretty LinearityType where
  pretty = \case
    LinearityTypeClass tc -> pretty tc
    Linearity l -> pretty l

--------------------------------------------------------------------------------
-- LinearityProvenance

-- TODO
-- 1) rename LinearityProvenance to LinearityProof
-- 2) mimic AST nodes names
data LinearityProvenance
  = QuantifiedVariableProvenance Provenance Text
  | NetworkOutputProvenance Provenance Text
  | LinFunctionProvenance Provenance LinearityProvenance FunctionPosition
  deriving (Generic)

instance ToJSON LinearityProvenance

instance Serialize LinearityProvenance

instance Show LinearityProvenance where
  show _x = ""

instance Eq LinearityProvenance where
  _x == _y = True

instance NFData LinearityProvenance where
  rnf _x = ()

instance Hashable LinearityProvenance where
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- Linearity

-- | Used to annotate numeric types, representing whether it represents a
-- constant, linear or non-linear expression.
data Linearity
  = Constant
  | Linear LinearityProvenance
  | NonLinear Provenance LinearityProvenance LinearityProvenance
  deriving (Eq, Show, Generic)

instance Ord Linearity where
  Constant <= _ = True
  Linear {} <= Linear {} = True
  Linear {} <= NonLinear {} = True
  NonLinear {} <= NonLinear {} = True
  _ <= _ = False

instance NFData Linearity

instance Hashable Linearity

instance ToJSON Linearity

instance Serialize Linearity

instance Pretty Linearity where
  pretty = \case
    Constant -> "Constant"
    Linear {} -> "Linear"
    NonLinear {} -> "NonLinear"

mapLinearityProvenance :: (LinearityProvenance -> LinearityProvenance) -> Linearity -> Linearity
mapLinearityProvenance f = \case
  Constant -> Constant
  Linear lp -> Linear (f lp)
  -- At the moment we don't change non-linear provenance because we
  -- want the minimal example.
  NonLinear p lp lp' -> NonLinear p lp lp'

--------------------------------------------------------------------------------
-- Linearity constraints

data LinearityTypeClass
  = MaxLinearity
  | MulLinearity
  | FunctionLinearity FunctionPosition
  | QuantifierLinearity Quantifier
  deriving (Eq, Generic, Show)

instance ToJSON LinearityTypeClass

instance Serialize LinearityTypeClass

instance NFData LinearityTypeClass

instance Hashable LinearityTypeClass

instance Pretty LinearityTypeClass where
  pretty = \case
    MaxLinearity -> "MaxLinearity"
    MulLinearity -> "MulLinearity"
    QuantifierLinearity q -> "QuantifierLinearity" <> pretty q
    FunctionLinearity p -> "FunctionLinearity" <> pretty p

-----------------------------------------------------------------------------
-- Type synonyms

type LinearityBuiltin = NormalisableBuiltin LinearityType

-- Value
type LinearityNormExpr = Value LinearityType

type LinearityNormBinder = VBinder LinearityType

type LinearityNormArg = VArg LinearityType

type LinearityNormType = VType LinearityType

type LinearitySpine = Spine LinearityType

type LinearityEnv = Env LinearityType

-- Constraint
type LinearityConstraintProgress = ConstraintProgress LinearityType

type LinearityTypeClassConstraint = TypeClassConstraint LinearityType

type LinearityUnificationConstraint = UnificationConstraint LinearityType

type LinearityConstraintContext = ConstraintContext LinearityType

type LinearityConstraint = Constraint LinearityType

-----------------------------------------------------------------------------
-- Patterns

pattern LinearityExpr :: Provenance -> Linearity -> Expr var (NormalisableBuiltin LinearityType)
pattern LinearityExpr p lin = Builtin p (CType (Linearity lin))

pattern VLinearityExpr :: Linearity -> LinearityNormExpr
pattern VLinearityExpr l <- VBuiltin (CType (Linearity l)) []
  where
    VLinearityExpr l = VBuiltin (CType $ Linearity l) []
