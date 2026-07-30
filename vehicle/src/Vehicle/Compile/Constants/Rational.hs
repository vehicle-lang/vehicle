{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Constants.Rational where

import Control.Monad.Identity (Identity (..))
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

instance (Monad m) => ConstantLike RatTensor m where
  addConstants a b xs ys = return $ zipWithTensor (\x y -> a * x + b * y) xs ys

  scaleConstant a xs = return $ mapTensor (\x -> a * x) xs

  toRatTensor = return . Just

  minConstants xs ys = return $ zipWithTensor min xs ys

  maxConstants xs ys = return $ zipWithTensor max xs ys

  stackConstants = \case
    [] -> developerError "Cannot stack zero tensors"
    ts@(t : _) -> return $ stack (shapeOf t) ts

  unstackConstants xs = return $ unstack xs

eliminateVarsInComparison ::
  LinearSubstitution SliceVariable ->
  LinearAssertion ->
  MaybeTrivial LinearAssertion
eliminateVarsInComparison f NormalisedRelation {..} = do
  let constantOrExpr = runIdentity $ eliminateVars f expression
  case constantOrExpr of
    Right newExpr -> NonTrivial $ NormalisedRelation {expression = newExpr, ..}
    Left tensor -> Trivial (isRelated relation tensor (ConstantTensor (shapeOf tensor) 0))
