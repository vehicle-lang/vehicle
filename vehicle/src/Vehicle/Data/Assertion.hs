module Vehicle.Data.Assertion where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Rational equalities

newtype Equality variable constant = Equality
  { equalityExpr :: LinearExpr variable constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (ToJSONKey variable, ToJSON constant) => ToJSON (Equality variable constant)

instance (Ord variable, FromJSONKey variable, FromJSON constant) => FromJSON (Equality variable constant)

instance (Pretty variable, Pretty constant, IsConstant constant) => Pretty (Equality variable constant) where
  pretty e = pretty (equalityExpr e) <+> "== 0"

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkRationalEqualityTriviality :: RationalEquality -> Maybe Bool
checkRationalEqualityTriviality (Equality e) = case isConstant e of
  Nothing -> Nothing
  Just c -> Just $ c == 0.0

type RationalEquality = Equality ElementVariable Rational

type TensorEquality = Equality TensorVariable RationalTensor

--------------------------------------------------------------------------------
-- Rational inequalities

data Inequality variable constant = Inequality
  { strictness :: Strictness,
    inequalityExpr :: LinearExpr variable constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData variable, NFData constant) => NFData (Inequality variable constant)

instance (ToJSONKey variable, ToJSON constant) => ToJSON (Inequality variable constant)

instance (Ord variable, FromJSONKey variable, FromJSON constant) => FromJSON (Inequality variable constant)

instance (Pretty variable, Pretty constant, IsConstant constant) => Pretty (Inequality variable constant) where
  pretty ineq =
    pretty (inequalityExpr ineq)
      <+> (if strictness ineq == Strict then "<" else "<=")
      <+> "0.0"

mkInequality ::
  (Ord variable, IsConstant constant) =>
  OrderOp ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  Inequality variable constant
mkInequality op e1 e2 =
  case op of
    Lt -> Inequality Strict (addExprs 1 (-1) e1 e2)
    Le -> Inequality NonStrict (addExprs 1 (-1) e1 e2)
    Gt -> Inequality Strict (addExprs (-1) 1 e1 e2)
    Ge -> Inequality NonStrict (addExprs (-1) 1 e1 e2)

type RationalInequality = Inequality ElementVariable Rational

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkRationalInequalityTriviality :: RationalInequality -> Maybe Bool
checkRationalInequalityTriviality (Inequality s e) = case isConstant e of
  Nothing -> Nothing
  Just c -> Just $ case s of
    Strict -> c < 0.0
    NonStrict -> c <= 0.0

--------------------------------------------------------------------------------
-- Assertions

data Assertion
  = RationalEq RationalEquality
  | RationalIneq RationalInequality
  | TensorEq TensorEquality
  deriving (Show, Eq, Generic)

instance ToJSON Assertion

instance FromJSON Assertion

instance Pretty Assertion where
  pretty = \case
    RationalEq eq -> pretty eq
    RationalIneq ineq -> pretty ineq
    TensorEq eq -> pretty eq

checkTriviality :: Assertion -> MaybeTrivial Assertion
checkTriviality ass = case ass of
  RationalEq Equality {..} -> case isConstant equalityExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial (d == 0)
  RationalIneq Inequality {..} -> case isConstant inequalityExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial ((if strictness == Strict then (<) else (<=)) d 0)
  TensorEq Equality {..} -> case isConstant equalityExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial (isZero d)

prettyAssertions :: [Assertion] -> Doc a
prettyAssertions assertions =
  vsep (fmap pretty assertions)

prettyInequalities :: [RationalInequality] -> Doc a
prettyInequalities assertions =
  vsep (fmap pretty assertions)

data Relation
  = Equal
  | LessThan
  | LessThanOrEqual
  deriving (Eq, Ord)

assertionRel :: Assertion -> Relation
assertionRel = \case
  RationalEq {} -> Equal
  TensorEq {} -> Equal
  RationalIneq ineq
    | strictness ineq == Strict -> LessThan
    | otherwise -> LessThanOrEqual

eqToAssertion ::
  LinearExpr ElementVariable Rational ->
  LinearExpr ElementVariable Rational ->
  Assertion
eqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  RationalEq $ Equality e

tensorEqToAssertion ::
  LinearExpr TensorVariable RationalTensor ->
  LinearExpr TensorVariable RationalTensor ->
  Assertion
tensorEqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  TensorEq $ Equality e

mapAssertionExprs ::
  (LinearExpr TensorVariable RationalTensor -> LinearExpr TensorVariable RationalTensor) ->
  (LinearExpr ElementVariable Rational -> LinearExpr ElementVariable Rational) ->
  Assertion ->
  MaybeTrivial Assertion
mapAssertionExprs ft fr ass = checkTriviality $ case ass of
  TensorEq Equality {..} -> TensorEq $ Equality $ ft equalityExpr
  RationalEq Equality {..} -> RationalEq $ Equality $ fr equalityExpr
  RationalIneq Inequality {..} -> RationalIneq $ Inequality strictness (fr inequalityExpr)

substituteTensorEq ::
  (TensorVariable, LinearExpr TensorVariable RationalTensor) ->
  Map ElementVariable (LinearExpr ElementVariable Rational) ->
  Assertion ->
  MaybeTrivial Assertion
substituteTensorEq (var, solution) ratSolutions =
  mapAssertionExprs
    (eliminateVar var solution)
    eliminateRatVars
  where
    -- Usually the expression being substituted into is much smaller than the number of tensor
    -- variables so we traverse the expression instead of folding over the subsitutions
    eliminateRatVars :: LinearExpr ElementVariable Rational -> LinearExpr ElementVariable Rational
    eliminateRatVars expr = do
      let varExprs = lookupVar <$> Map.toList (coefficients expr)
      let constantExp = Sparse (mempty @(Map ElementVariable Coefficient)) (constantValue expr)
      foldr (addExprs 1 1) constantExp varExprs

    lookupVar :: (ElementVariable, Coefficient) -> LinearExpr ElementVariable Rational
    lookupVar (v, c) = do
      let vc = Sparse (Map.singleton v c) 0
      case Map.lookup v ratSolutions of
        Nothing -> vc
        Just sol -> eliminateVar v sol vc

substituteRationalEq :: UserElementVariable -> LinearExpr ElementVariable Rational -> Assertion -> MaybeTrivial Assertion
substituteRationalEq var solution = mapAssertionExprs id (eliminateVar var solution)

--------------------------------------------------------------------------------
-- Bounds

type Bound variable constant = Inequality variable constant

pattern Bound :: Strictness -> LinearExpr variable constant -> Bound variable constant
pattern Bound s e = Inequality s e

{-# COMPLETE Bound #-}

type LowerBound variable constant = Bound variable constant

type UpperBound variable constant = Bound variable constant

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data Bounds variable constant = Bounds
  { lowerBounds :: [LowerBound variable constant],
    upperBounds :: [UpperBound variable constant]
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData variable, NFData constant) => NFData (Bounds variable constant)

instance (ToJSONKey variable, ToJSON constant) => ToJSON (Bounds variable constant)

instance (Ord variable, FromJSONKey variable, FromJSON constant) => FromJSON (Bounds variable constant)

instance (Pretty variable, Pretty constant, IsConstant constant) => Pretty (Bounds variable constant) where
  pretty Bounds {..} =
    "below by max"
      <+> pretty lowerBounds
      <+> "and"
      <+> "above by min"
      <+> pretty upperBounds

--------------------------------------------------------------------------------
-- Variable status

data UnderConstrainedVariableStatus
  = Unconstrained
  | BoundedAbove
  | BoundedBelow
  deriving (Show, Eq, Ord)

instance Pretty UnderConstrainedVariableStatus where
  pretty = \case
    Unconstrained -> "no lower or upper bound"
    BoundedAbove -> "no lower bound"
    BoundedBelow -> "no upper bound"

instance Semigroup UnderConstrainedVariableStatus where
  Unconstrained <> r = r
  r <> Unconstrained = r
  BoundedAbove <> r = r
  r <> BoundedAbove = r
  BoundedBelow <> BoundedBelow = BoundedBelow

prettyUnderConstrainedVariable :: (Pretty var) => (var, UnderConstrainedVariableStatus) -> Doc a
prettyUnderConstrainedVariable (var, constraint) =
  pretty var <+> "-" <+> pretty constraint

checkBoundsExist ::
  (variable, Bounds variable constant) ->
  Either (variable, UnderConstrainedVariableStatus) (NonEmpty (LowerBound variable constant), NonEmpty (UpperBound variable constant))
checkBoundsExist (var, Bounds {..}) = case (lowerBounds, upperBounds) of
  ([], []) -> Left (var, Unconstrained)
  ([], _) -> Left (var, BoundedAbove)
  (_, []) -> Left (var, BoundedBelow)
  (l : ls, u : us) -> Right (l :| ls, u :| us)
