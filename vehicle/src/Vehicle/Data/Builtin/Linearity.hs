module Vehicle.Data.Builtin.Linearity where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core hiding (Builtin (BuiltinConstructor, BuiltinFunction))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL

--------------------------------------------------------------------------------
-- LinearityProvenance

-- TODO
-- 1) rename LinearityProvenance to LinearityProof
-- 2) mimic AST nodes names
data LinearityProvenance
  = QuantifiedVariableProvenance Provenance Text
  | NetworkOutputProvenance Provenance Text
  | LinFunctionProvenance Provenance LinearityProvenance FunctionPosition
  deriving (Show, Generic)

instance Serialize LinearityProvenance

instance Eq LinearityProvenance where
  _x == _y = True

instance NFData LinearityProvenance where
  rnf _x = ()

instance Hashable LinearityProvenance where
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- NonLinearity

-- | Possible sources of non-linearity in the program
data NonLinearitySource
  = -- | A multiplication where both arguments are linear
    LinearTimesLinear Provenance LinearityProvenance LinearityProvenance
  | -- | A division where the divisor is linear.
    DivideByLinear Provenance LinearityProvenance
  | -- | An power where the base is linear
    PowLinearBase Provenance LinearityProvenance
  | -- | An power where the exponent is linear
    PowLinearExponent Provenance LinearityProvenance
  deriving (Eq, Show, Generic)

instance Pretty NonLinearitySource where
  pretty = \case
    LinearTimesLinear {} -> "X*X"
    DivideByLinear {} -> "?/X"
    PowLinearBase {} -> "X^?"
    PowLinearExponent {} -> "?^X"

instance NFData NonLinearitySource

instance Hashable NonLinearitySource

instance Serialize NonLinearitySource

--------------------------------------------------------------------------------
-- Linearity

-- | Used to annotate numeric types, representing whether it represents a
-- constant, linear or non-linear expression.
data Linearity
  = Constant
  | Linear LinearityProvenance
  | NonLinear NonLinearitySource
  deriving (Eq, Show, Generic)

instance NFData Linearity

instance Hashable Linearity

instance Serialize Linearity

instance Pretty Linearity where
  pretty = \case
    Constant -> "Constant"
    Linear {} -> "Linear"
    NonLinear nl -> "NonLinear[" <+> pretty nl <+> "]"

mapLinearityProvenance ::
  (LinearityProvenance -> LinearityProvenance) ->
  Linearity ->
  Linearity
mapLinearityProvenance f = \case
  Constant -> Constant
  Linear lp -> Linear (f lp)
  -- At the moment we don't change non-linear provenance because we
  -- want the minimal example.
  NonLinear l -> NonLinear l

--------------------------------------------------------------------------------
-- Linearity constraints

data LinearityRelation
  = MaxLinearity
  | MulLinearity
  | DivLinearity
  | PowLinearity
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
    DivLinearity -> "DivLinearity"
    PowLinearity -> "PowLinearity"
    QuantifierLinearity q -> "QuantifierLinearity[" <> pretty q <> "]"
    FunctionLinearity p -> "FunctionLinearity[" <> pretty p <> "]"

-----------------------------------------------------------------------------
-- Full builtin

data LinearityBuiltin
  = LinearityConstructor BuiltinConstructor
  | LinearityFunction BuiltinFunction
  | Linearity Linearity
  | LinearityRelation LinearityRelation
  deriving (Show, Eq, Generic)

instance Hashable LinearityBuiltin

instance Pretty LinearityBuiltin where
  pretty = \case
    LinearityConstructor c -> pretty c
    LinearityFunction f -> pretty f
    Linearity l -> pretty l
    LinearityRelation tc -> pretty tc

instance BuiltinHasStandardData LinearityBuiltin where
  mkBuiltinFunction = LinearityFunction
  getBuiltinFunction = \case
    LinearityFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = LinearityConstructor
  getBuiltinConstructor = \case
    LinearityConstructor c -> Just c
    _ -> Nothing

  getBuiltinTypeClassOp = const Nothing

instance BuiltinHasBoolLiterals LinearityBuiltin where
  mkBoolBuiltinLit b = LinearityConstructor (LBool b)
  getBoolBuiltinLit = \case
    LinearityConstructor (LBool b) -> Just b
    _ -> Nothing

instance BuiltinHasIndexLiterals LinearityBuiltin where
  getIndexBuiltinLit e = case e of
    LinearityConstructor (LIndex n) -> Just n
    _ -> Nothing
  mkIndexBuiltinLit x = LinearityConstructor (LIndex x)

instance BuiltinHasNatLiterals LinearityBuiltin where
  getNatBuiltinLit e = case e of
    LinearityConstructor (LNat b) -> Just b
    _ -> Nothing
  mkNatBuiltinLit x = LinearityConstructor (LNat x)

instance BuiltinHasRatLiterals LinearityBuiltin where
  getRatBuiltinLit e = case e of
    LinearityConstructor (LRat b) -> Just b
    _ -> Nothing
  mkRatBuiltinLit x = LinearityConstructor (LRat x)

-----------------------------------------------------------------------------
-- Patterns

pattern LinearityExpr :: Provenance -> Linearity -> Expr LinearityBuiltin
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

divLinearity :: LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
divLinearity l1 l2 l3 = linearityRelation DivLinearity [l1, l2, l3]

powLinearity :: LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
powLinearity l1 l2 l3 = linearityRelation PowLinearity [l1, l2, l3]

quantLinearity :: Quantifier -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
quantLinearity q l1 l2 = linearityRelation (QuantifierLinearity q) [l1, l2]

linear :: LinearityDSLExpr
linear = DSL $ \p _ -> Builtin p (Linearity (Linear $ prov p ""))
  where
    prov = QuantifiedVariableProvenance

tLin :: LinearityDSLExpr
tLin = type0
