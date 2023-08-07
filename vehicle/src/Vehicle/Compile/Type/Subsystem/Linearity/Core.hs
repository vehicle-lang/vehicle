module Vehicle.Compile.Type.Subsystem.Linearity.Core where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DSL
import Vehicle.Expr.Normalised
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin hiding (Builtin (BuiltinConstructor, BuiltinFunction))

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

data LinearityRelation
  = MaxLinearity
  | MulLinearity
  | FunctionLinearity FunctionPosition
  | QuantifierLinearity Quantifier
  deriving (Eq, Generic, Show)

instance Serialize LinearityRelation

instance NFData LinearityRelation

instance Hashable LinearityRelation

instance Pretty LinearityRelation where
  pretty = \case
    MaxLinearity -> "MaxLinearity"
    MulLinearity -> "MulLinearity"
    QuantifierLinearity q -> "QuantifierLinearity" <> pretty q
    FunctionLinearity p -> "FunctionLinearity" <> pretty p

-----------------------------------------------------------------------------
-- Full builtin

data LinearityBuiltin
  = BuiltinConstructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | Linearity Linearity
  | LinearityRelation LinearityRelation
  deriving (Show, Eq)

instance Pretty LinearityBuiltin where
  pretty = \case
    BuiltinConstructor c -> pretty c
    BuiltinFunction f -> pretty f
    Linearity l -> pretty l
    LinearityRelation tc -> pretty tc

instance HasStandardData LinearityBuiltin where
  mkBuiltinFunction = BuiltinFunction
  getBuiltinFunction = \case
    BuiltinFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = BuiltinConstructor
  getBuiltinConstructor = \case
    BuiltinConstructor c -> Just c
    _ -> Nothing

  isTypeClassOp = const False

-----------------------------------------------------------------------------
-- Patterns

pattern LinearityExpr :: Provenance -> Linearity -> Expr var LinearityBuiltin
pattern LinearityExpr p lin = Builtin p (Linearity lin)

pattern VLinearityExpr :: Linearity -> Value LinearityBuiltin
pattern VLinearityExpr l <- VBuiltin (Linearity l) []
  where
    VLinearityExpr l = VBuiltin (Linearity l) []

--------------------------------------------------------------------------------
-- DSL

type LinearityDSLExpr = DSLExpr LinearityBuiltin

forAllLinearities :: (LinearityDSLExpr -> LinearityDSLExpr) -> LinearityDSLExpr
forAllLinearities f = forAll "l" tLin $ \l -> f l

forAllLinearityTriples :: (LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr) -> LinearityDSLExpr
forAllLinearityTriples f =
  forAll "l1" tLin $ \l1 ->
    forAll "l2" tLin $ \l2 ->
      forAll "l3" tLin $ \l3 -> f l1 l2 l3

constant :: LinearityDSLExpr
constant = builtin (Linearity Constant)

linearityRelation :: LinearityRelation -> NonEmpty LinearityDSLExpr -> LinearityDSLExpr
linearityRelation tc args = builtin (LinearityRelation tc) @@ args

maxLinearity :: LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
maxLinearity l1 l2 l3 = linearityRelation MaxLinearity [l1, l2, l3]

mulLinearity :: LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
mulLinearity l1 l2 l3 = linearityRelation MulLinearity [l1, l2, l3]

quantLinearity :: Quantifier -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
quantLinearity q l1 l2 = linearityRelation (QuantifierLinearity q) [l1, l2]

linear :: LinearityDSLExpr
linear = DSL $ \p _ -> Builtin p (Linearity (Linear $ prov p ""))
  where
    prov = QuantifiedVariableProvenance

tLin :: LinearityDSLExpr
tLin = type0
