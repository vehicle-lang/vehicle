module Vehicle.Data.Assertion where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (tensorAll, zeroTensor)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Rational equalities

newtype Equality = Equality
  { equalityExpr :: LinearExpr
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Equality

instance FromJSON Equality

instance Pretty Equality where
  pretty e = pretty (equalityExpr e) <+> "== 0"

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkRationalEqualityTriviality :: Equality -> Maybe Bool
checkRationalEqualityTriviality (Equality e) = case isConstant e of
  Nothing -> Nothing
  Just c -> Just $ isZero c

--------------------------------------------------------------------------------
-- Rational inequalities

data Inequality = Inequality
  { strictness :: Strictness,
    inequalityExpr :: LinearExpr
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData Inequality

instance ToJSON Inequality

instance FromJSON Inequality

instance Pretty Inequality where
  pretty ineq =
    pretty (inequalityExpr ineq)
      <+> (if strictness ineq == Strict then "<" else "<=")
      <+> "0.0"

mkInequality :: OrderOp -> LinearExpr -> LinearExpr -> Inequality
mkInequality op e1 e2 =
  case op of
    Lt -> Inequality Strict (addExprs 1 (-1) e1 e2)
    Le -> Inequality NonStrict (addExprs 1 (-1) e1 e2)
    Gt -> Inequality Strict (addExprs (-1) 1 e1 e2)
    Ge -> Inequality NonStrict (addExprs (-1) 1 e1 e2)

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkInequalityTriviality :: Inequality -> Maybe Bool
checkInequalityTriviality (Inequality s e) = case isConstant e of
  Nothing -> Nothing
  Just c -> Just $ case s of
    Strict -> tensorAll (< 0.0) c
    NonStrict -> tensorAll (<= 0.0) c

--------------------------------------------------------------------------------
-- Assertions

data Assertion
  = RationalEq Equality
  | RationalIneq Inequality
  | TensorEq Equality
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
    Just c -> Trivial (isZero c)
  RationalIneq Inequality {..} -> case isConstant inequalityExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial (tensorAll (if strictness == Strict then (< 0.0) else (<= 0.0)) d)
  TensorEq Equality {..} -> case isConstant equalityExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial (isZero d)

prettyAssertions :: [Assertion] -> Doc a
prettyAssertions assertions =
  vsep (fmap pretty assertions)

prettyInequalities :: [Inequality] -> Doc a
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

eqToAssertion :: LinearExpr -> LinearExpr -> Assertion
eqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  RationalEq $ Equality e

tensorEqToAssertion :: LinearExpr -> LinearExpr -> Assertion
tensorEqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  TensorEq $ Equality e

mapAssertionExprs ::
  (LinearExpr -> LinearExpr) ->
  (LinearExpr -> LinearExpr) ->
  Assertion ->
  MaybeTrivial Assertion
mapAssertionExprs ft fr ass = checkTriviality $ case ass of
  TensorEq Equality {..} -> TensorEq $ Equality $ ft equalityExpr
  RationalEq Equality {..} -> RationalEq $ Equality $ fr equalityExpr
  RationalIneq Inequality {..} -> RationalIneq $ Inequality strictness (fr inequalityExpr)

substituteTensorEq ::
  (TensorVariable, LinearExpr) ->
  Map ElementVariable LinearExpr ->
  Assertion ->
  MaybeTrivial Assertion
substituteTensorEq (var, solution) ratSolutions =
  mapAssertionExprs
    (eliminateVar var solution)
    eliminateRatVars
  where
    -- Usually the expression being substituted into is much smaller than the number of tensor
    -- variables so we traverse the expression instead of folding over the subsitutions
    eliminateRatVars :: LinearExpr -> LinearExpr
    eliminateRatVars expr = do
      let varExprs = lookupVar <$> Map.toList (coefficients expr)
      let constantExp = Sparse (mempty @(Map ElementVariable Coefficient)) (constantValue expr)
      foldr (addExprs 1 1) constantExp varExprs

    lookupVar :: (ElementVariable, Coefficient) -> LinearExpr
    lookupVar (v, c) = do
      let vc = Sparse (Map.singleton v c) (zeroTensor [])
      case Map.lookup v ratSolutions of
        Nothing -> vc
        Just sol -> eliminateVar v sol vc

substituteRationalEq :: UserElementVariable -> LinearExpr -> Assertion -> MaybeTrivial Assertion
substituteRationalEq var solution = mapAssertionExprs id (eliminateVar var solution)

--------------------------------------------------------------------------------
-- Bounds

type Bound = Inequality

pattern Bound :: Strictness -> LinearExpr -> Bound
pattern Bound s e = Inequality s e

{-# COMPLETE Bound #-}

type LowerBound = Bound

type UpperBound = Bound

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data Bounds = Bounds
  { lowerBounds :: [LowerBound],
    upperBounds :: [UpperBound]
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData Bounds

instance ToJSON Bounds

instance FromJSON Bounds

instance Pretty Bounds where
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
  (Variable, Bounds) ->
  Either (Variable, UnderConstrainedVariableStatus) (NonEmpty LowerBound, NonEmpty UpperBound)
checkBoundsExist (var, Bounds {..}) = case (lowerBounds, upperBounds) of
  ([], []) -> Left (var, Unconstrained)
  ([], _) -> Left (var, BoundedAbove)
  (_, []) -> Left (var, BoundedBelow)
  (l : ls, u : us) -> Right (l :| ls, u :| us)
