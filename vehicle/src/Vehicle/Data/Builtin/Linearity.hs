-- | Builtins for deciding whether or not a property is `Constant`, `Linear` or
-- `NonLinear` during compilation to verifier queries.
module Vehicle.Data.Builtin.Linearity where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Expr
import Vehicle.Data.DSL
import Vehicle.Prelude

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

functionAccessor :: BuiltinFunction -> Accessor LinearityBuiltin ()
functionAccessor b =
  Access
    { getExpr = \case
        LinearityFunction b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> LinearityFunction b
    }

instance BuiltinHasStandardData LinearityBuiltin where
  accessBuiltinFunction =
    Access
      { mkExpr = LinearityFunction,
        getExpr = \case
          LinearityFunction c -> Just c
          _ -> Nothing
      }

  accessBuiltinConstructor =
    Access
      { mkExpr = LinearityConstructor,
        getExpr = \case
          LinearityConstructor c -> Just c
          _ -> Nothing
      }

instance BuiltinHasNatLiterals LinearityBuiltin where
  accessNatLitBuiltin =
    Access
      { getExpr = \case
          LinearityConstructor (NatLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = LinearityConstructor . NatLiteral
      }

  accessNatTensorLitBuiltin =
    Access
      { getExpr = \case
          LinearityConstructor (NatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = LinearityConstructor . NatTensorLiteral
      }

  accessAddNatBuiltin = functionAccessor (Add AddNat)
  accessMulNatBuiltin = functionAccessor (Mul MulNat)

instance BuiltinHasListLiterals LinearityBuiltin where
  accessNilBuiltin =
    Access
      { getExpr = \case
          LinearityConstructor Nil -> Just ()
          _ -> Nothing,
        mkExpr = \() -> LinearityConstructor Nil
      }

  accessConsBuiltin =
    Access
      { getExpr = \case
          LinearityConstructor Cons -> Just ()
          _ -> Nothing,
        mkExpr = \() -> LinearityConstructor Cons
      }

  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList

instance BuiltinHasIterate LinearityBuiltin where
  accessIterateBuiltin = functionAccessor Iterate

--------------------------------------------------------------------------------
-- Printing

instance ConvertableBuiltin LinearityBuiltin Builtin where
  convertBuiltin p = \case
    LinearityConstructor c -> convertBuiltin p c
    LinearityFunction f -> convertBuiltin p f
    b -> cheatConvertBuiltin p $ pretty b

instance PrintableBuiltin LinearityBuiltin where
  coercionArgs _ = Nothing

--------------------------------------------------------------------------------
-- Normalisation

instance NormalisableBuiltin LinearityBuiltin where
  evalScheme b = case b of
    LinearityFunction Iterate -> NonSimple evalIterate
    LinearityFunction _ -> None
    _ -> None

  blockingArgs = \case
    LinearityFunction f -> functionBlockingArgs f
    _ -> noBlockingArgs

  isTypeClassOp _ = False
  isCast _ _ = Nothing

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
