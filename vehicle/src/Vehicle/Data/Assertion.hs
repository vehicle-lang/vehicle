module Vehicle.Data.Assertion where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map (mapKeys)
import GHC.Generics
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr (ConjunctAll (..), MaybeTrivial (..))
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (HasShape, RatTensor, Tensor, at)
import Vehicle.Prelude
import Vehicle.Syntax.Tensor
  ( HasShape (..),
    Tensor (..),
    compareTensor,
  )

class IsRelation relation where
  isRelated :: relation -> Tensor Rational -> Tensor Rational -> Bool

checkTriviality :: (IsRelation relation) => relation -> RatTensor -> Bool
checkTriviality rel tensor = isRelated rel tensor (ConstantTensor (shapeOf tensor) 0)

--------------------------------------------------------------------------------
-- Relations

data Relation
  = OEq
  | OLe
  | OLt
  deriving (Eq, Ord)

relationToComparisonOp :: Relation -> ComparisonOp
relationToComparisonOp = \case
  OEq -> Eq
  OLe -> Le
  OLt -> Lt

instance Pretty Relation where
  pretty = pretty . relationToComparisonOp

instance IsRelation Relation where
  isRelated = \case
    OLe -> compareTensor (<=)
    OLt -> compareTensor (<)
    OEq -> compareTensor (==)

--------------------------------------------------------------------------------
-- Strictness

data InequalityRelation
  = Strict
  | NonStrict
  deriving (Show, Eq, Ord, Generic)

instance NFData InequalityRelation

instance ToJSON InequalityRelation

instance FromJSON InequalityRelation

inequalityToRelation :: InequalityRelation -> Relation
inequalityToRelation = \case
  Strict -> OLe
  NonStrict -> OLt

instance Pretty InequalityRelation where
  pretty = pretty . inequalityToRelation

--------------------------------------------------------------------------------
-- Equality relation

data EqualityRelation = EqualityRelation

instance IsRelation EqualityRelation where
  isRelated _ = compareTensor (==)

--------------------------------------------------------------------------------
-- Normalisation relations

-- TODO rename to `Comparison`?
data NormalisedRelation rel expr = NormalisedRelation
  { relation :: rel,
    expression :: expr
  }
  deriving (Show, Eq, Ord, Generic)

instance
  (NFData rel, NFData expr) =>
  NFData (NormalisedRelation rel expr)

instance
  (ToJSON rel, ToJSON expr) =>
  ToJSON (NormalisedRelation rel expr)

instance
  (FromJSON rel, FromJSON expr) =>
  FromJSON (NormalisedRelation rel expr)

instance (HasVariables expr variable) => HasVariables (NormalisedRelation rel expr) variable where
  variablesOf = variablesOf . expression
  containsVariable r v = expression r `containsVariable` v

eliminateVarsInComparison ::
  (VariableLike variable, IsRelation relation) =>
  LinearSubstitution variable ->
  NormalisedRelation relation (LinearExpr variable RatTensor) ->
  MaybeTrivial (NormalisedRelation relation (LinearExpr variable RatTensor))
eliminateVarsInComparison f NormalisedRelation {..} =
  case eliminateVars f expression of
    Right newExpr -> NonTrivial $ NormalisedRelation {expression = newExpr, ..}
    Left tensor -> Trivial (checkTriviality relation tensor)

reduceComparison ::
  (Ord variable) =>
  Int ->
  (variable -> [variable]) ->
  NormalisedRelation rel (LinearExpr variable RatTensor) ->
  Maybe (ConjunctAll (NormalisedRelation rel (LinearExpr variable RatTensor)))
reduceComparison lookupElementVariables dim (NormalisedRelation relation linearExpr) = do
  let rationalEqualities = reduceTensorExpr lookupElementVariables dim linearExpr
  let reducedComparison = fmap (NormalisedRelation relation) rationalEqualities
  case reducedComparison of
    [] -> Nothing
    (v : vs) -> Just $ ConjunctAll (v :| vs)

reduceTensorExpr ::
  forall variable.
  (Ord variable) =>
  Int ->
  (variable -> [variable]) ->
  LinearExpr variable RatTensor ->
  [LinearExpr variable RatTensor]
reduceTensorExpr dim lookupElementVariables expr = do
  fmap (reduceLinearExprAt lookupElementVariables expr) [0 .. dim - 1]

reduceLinearExprAt ::
  (Ord variable) =>
  (variable -> [variable]) ->
  LinearExpr variable RatTensor ->
  Int ->
  LinearExpr variable RatTensor
reduceLinearExprAt lookupElementVariables (Sparse coeff constant) i =
  Sparse
    { coefficients = Map.mapKeys (\v -> lookupElementVariables v !! i) coeff,
      constantValue = constant `at` i
    }

--------------------------------------------------------------------------------
-- Assertions

type Inequality expr = NormalisedRelation InequalityRelation expr

type Equality expr = NormalisedRelation () expr

splitRelation ::
  NormalisedRelation Relation expr ->
  Either (Inequality expr) (Equality expr)
splitRelation r = case relation r of
  OEq -> Right $ r {relation = ()}
  OLe -> Left $ r {relation = NonStrict}
  OLt -> Left $ r {relation = Strict}

inequalityToNormRelation :: Inequality expr -> NormalisedRelation Relation expr
inequalityToNormRelation r = case relation r of
  Strict -> r {relation = OLt}
  NonStrict -> r {relation = OLe}

type Assertion expr = NormalisedRelation Relation expr

instance (HasShape expr) => HasShape (Assertion expr) where
  shapeOf assertion = shapeOf (expression assertion)

comparisonToAssertion ::
  (VariableLike variable) =>
  ComparisonOp ->
  LinearExpr variable RatTensor ->
  LinearExpr variable RatTensor ->
  Assertion (LinearExpr variable RatTensor)
comparisonToAssertion op e1 e2 = case op of
  Ne -> developerError "Cannot convert `Ne` to assertion"
  Eq -> NormalisedRelation OEq $ addExprs 1 (-1) e1 e2
  Lt -> NormalisedRelation OLt $ addExprs 1 (-1) e1 e2
  Le -> NormalisedRelation OLe $ addExprs 1 (-1) e1 e2
  Gt -> NormalisedRelation OLt $ addExprs (-1) 1 e1 e2
  Ge -> NormalisedRelation OLe $ addExprs (-1) 1 e1 e2

type LinearSubstitution variable = Map variable (LinearExpr variable RatTensor)

equalityToAssertion :: Equality expr -> Assertion expr
equalityToAssertion (NormalisedRelation () e) = NormalisedRelation OEq e

getEquality :: Assertion expr -> Maybe (Equality expr)
getEquality (NormalisedRelation rel expr) = case rel of
  OEq -> Just (NormalisedRelation () expr)
  _ -> Nothing

getInequality :: Assertion expr -> Maybe (Inequality expr)
getInequality (NormalisedRelation rel expr) = case rel of
  OLe -> Just (NormalisedRelation NonStrict expr)
  OLt -> Just (NormalisedRelation Strict expr)
  _ -> Nothing

--------------------------------------------------------------------------------
-- Bounds

type Bound expr = Inequality expr

pattern Bound :: InequalityRelation -> expr -> Bound expr
pattern Bound s e = NormalisedRelation s e

{-# COMPLETE Bound #-}

type LowerBound expr = Bound expr

type UpperBound expr = Bound expr

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data Bounds expr = Bounds
  { lowerBounds :: [LowerBound expr],
    upperBounds :: [UpperBound expr]
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData expr) => NFData (Bounds expr)

instance (ToJSON expr) => ToJSON (Bounds expr)

instance (FromJSON expr) => FromJSON (Bounds expr)

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
  (variable, Bounds expr) ->
  Either (variable, UnderConstrainedVariableStatus) (NonEmpty (LowerBound expr), NonEmpty (UpperBound expr))
checkBoundsExist (var, Bounds {..}) = case (lowerBounds, upperBounds) of
  ([], []) -> Left (var, Unconstrained)
  ([], _) -> Left (var, BoundedAbove)
  (_, []) -> Left (var, BoundedBelow)
  (l : ls, u : us) -> Right (l :| ls, u :| us)

--------------------------------------------------------------------------------
-- Specialisation to real tensors

type LinearInequality = NormalisedRelation InequalityRelation LinearExpression

type LinearEquality = NormalisedRelation () LinearExpression

type LinearBounds = Bounds LinearExpression

type LinearAssertion = Assertion LinearExpression
