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
import Vehicle.Data.Tensor (RationalTensor, allTensor, zeroTensor)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Rational equalities

newtype Equality constant = Equality
  { equalityExpr :: LinearExpr constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (ToJSON constant) => ToJSON (Equality constant)

instance (FromJSON constant) => FromJSON (Equality constant)

-- | Checks whether an equality is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkEqualityTriviality :: Equality RationalTensor -> Maybe Bool
checkEqualityTriviality (Equality e) = fmap isZero (isConstant e)

--------------------------------------------------------------------------------
-- Rational inequalities

data Inequality constant = Inequality
  { strictness :: Strictness,
    inequalityExpr :: LinearExpr constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData constant) => NFData (Inequality constant)

instance (ToJSON constant) => ToJSON (Inequality constant)

instance (FromJSON constant) => FromJSON (Inequality constant)

mkInequality :: (Constant constant) => OrderOp -> LinearExpr constant -> LinearExpr constant -> Inequality constant
mkInequality op e1 e2 =
  case op of
    Lt -> Inequality Strict (addExprs 1 (-1) e1 e2)
    Le -> Inequality NonStrict (addExprs 1 (-1) e1 e2)
    Gt -> Inequality Strict (addExprs (-1) 1 e1 e2)
    Ge -> Inequality NonStrict (addExprs (-1) 1 e1 e2)

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkInequalityTriviality :: Inequality RationalTensor -> Maybe Bool
checkInequalityTriviality (Inequality s e) = case isConstant e of
  Nothing -> Nothing
  Just tensor -> Just $ case s of
    Strict -> allTensor (< 0.0) tensor
    NonStrict -> allTensor (<= 0.0) tensor

--------------------------------------------------------------------------------
-- Assertions

data Assertion
  = RationalEq (Equality RationalTensor)
  | RationalIneq (Inequality RationalTensor)
  | TensorEq (Equality RationalTensor)
  deriving (Show, Eq, Generic)

instance ToJSON Assertion

instance FromJSON Assertion

checkTriviality :: Assertion -> MaybeTrivial Assertion
checkTriviality ass = case ass of
  RationalEq eq -> maybe (NonTrivial ass) Trivial (checkEqualityTriviality eq)
  RationalIneq ineq -> maybe (NonTrivial ass) Trivial (checkInequalityTriviality ineq)
  TensorEq eq -> maybe (NonTrivial ass) Trivial (checkEqualityTriviality eq)

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

eqToAssertion :: LinearExpr RationalTensor -> LinearExpr RationalTensor -> Assertion
eqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  RationalEq $ Equality e

tensorEqToAssertion :: LinearExpr RationalTensor -> LinearExpr RationalTensor -> Assertion
tensorEqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  TensorEq $ Equality e

mapAssertionExprs ::
  (LinearExpr RationalTensor -> LinearExpr RationalTensor) ->
  (LinearExpr RationalTensor -> LinearExpr RationalTensor) ->
  Assertion ->
  MaybeTrivial Assertion
mapAssertionExprs ft fr ass = checkTriviality $ case ass of
  TensorEq Equality {..} -> TensorEq $ Equality $ ft equalityExpr
  RationalEq Equality {..} -> RationalEq $ Equality $ fr equalityExpr
  RationalIneq Inequality {..} -> RationalIneq $ Inequality strictness (fr inequalityExpr)

substituteTensorEq ::
  (TensorVariable, LinearExpr RationalTensor) ->
  Map ElementVariable (LinearExpr RationalTensor) ->
  Assertion ->
  MaybeTrivial Assertion
substituteTensorEq (var, solution) ratSolutions =
  mapAssertionExprs
    (eliminateVar var solution)
    eliminateRatVars
  where
    -- Usually the expression being substituted into is much smaller than the number of tensor
    -- variables so we traverse the expression instead of folding over the subsitutions
    eliminateRatVars :: LinearExpr RationalTensor -> LinearExpr RationalTensor
    eliminateRatVars expr = do
      let varExprs = lookupVar <$> Map.toList (coefficients expr)
      let constantExp = Sparse (mempty @(Map ElementVariable Coefficient)) (constantValue expr)
      foldr (addExprs 1 1) constantExp varExprs

    lookupVar :: (ElementVariable, Coefficient) -> LinearExpr RationalTensor
    lookupVar (v, c) = do
      let vc = Sparse (Map.singleton v c) (zeroTensor [])
      case Map.lookup v ratSolutions of
        Nothing -> vc
        Just sol -> eliminateVar v sol vc

substituteRationalEq :: UserElementVariable -> LinearExpr RationalTensor -> Assertion -> MaybeTrivial Assertion
substituteRationalEq var solution = mapAssertionExprs id (eliminateVar var solution)

--------------------------------------------------------------------------------
-- Bounds

type Bound constant = Inequality constant

pattern Bound :: Strictness -> LinearExpr constant -> Bound constant
pattern Bound s e = Inequality s e

{-# COMPLETE Bound #-}

type LowerBound constant = Bound constant

type UpperBound constant = Bound constant

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data Bounds constant = Bounds
  { lowerBounds :: [LowerBound constant],
    upperBounds :: [UpperBound constant]
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData constant) => NFData (Bounds constant)

instance (ToJSON constant) => ToJSON (Bounds constant)

instance (FromJSON constant) => FromJSON (Bounds constant)

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
  (Variable, Bounds constant) ->
  Either (Variable, UnderConstrainedVariableStatus) (NonEmpty (LowerBound constant), NonEmpty (UpperBound constant))
checkBoundsExist (var, Bounds {..}) = case (lowerBounds, upperBounds) of
  ([], []) -> Left (var, Unconstrained)
  ([], _) -> Left (var, BoundedAbove)
  (_, []) -> Left (var, BoundedBelow)
  (l : ls, u : us) -> Right (l :| ls, u :| us)
