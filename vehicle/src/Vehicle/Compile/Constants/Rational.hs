{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Constants.Rational where

import Vehicle.Data.Assertion
import Vehicle.Data.Bound (SliceBounds)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensors of rationals

type LinearExpression = LinearExpr SliceVariable RatTensor

type LinearInequality = NormalisedRelation InequalityRelation LinearExpression

type LinearEquality = NormalisedRelation () LinearExpression

type LinearAssertion = Assertion LinearExpression

-- | An `AssertionTree` represents a boolean expression with assertions at
-- each terminal leaf.
type LinearAssertionTree = BooleanExpr LinearAssertion

type LinearBounds = SliceBounds LinearExpression

instance ConstantLike RatTensor where
  addConstants :: Coefficient -> Coefficient -> RatTensor -> RatTensor -> RatTensor
  addConstants a b = zipWithTensor (\x y -> a * x + b * y)

  scaleConstant :: Coefficient -> RatTensor -> RatTensor
  scaleConstant a = mapTensor (\x -> a * x)

  toRatTensor :: RatTensor -> Maybe RatTensor
  toRatTensor = Just

  minConstants :: RatTensor -> RatTensor -> RatTensor
  minConstants = zipWithTensor min

  maxConstants :: RatTensor -> RatTensor -> RatTensor
  maxConstants = zipWithTensor max

  stackConstants :: [RatTensor] -> RatTensor
  stackConstants = \case
    [] -> developerError "Cannot stack zero tensors"
    ts@(t : _) -> stack (shapeOf t) ts

  unstackConstants :: RatTensor -> [RatTensor]
  unstackConstants = unstack

eliminateVarsInComparison ::
  LinearSubstitution SliceVariable ->
  LinearAssertion ->
  MaybeTrivial LinearAssertion
eliminateVarsInComparison f NormalisedRelation {..} =
  case eliminateVars f expression of
    Right newExpr -> NonTrivial $ NormalisedRelation {expression = newExpr, ..}
    Left tensor -> Trivial (isRelated relation tensor (ConstantTensor (shapeOf tensor) 0))
