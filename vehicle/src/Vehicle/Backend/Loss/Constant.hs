{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.Constant where

import Data.Map qualified as Map
import Vehicle.Compile.Constants.Atom.Core
import Vehicle.Compile.Constants.TensorValue
import Vehicle.Compile.Constants.TensorValue.Core
import Vehicle.Compile.Normalise.Core (MonadNorm)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Real (ExtendedRational (..))

--------------------------------------------------------------------------------
-- Purified constants

-- | A linear expression over the terms a constant could not evaluate, each held by an atom.
-- The `tensorValue` of the `TensorConstantValue` therefore never mentions a bound variable.
type PureConstant = LinearExpr Atom TensorConstantValue

uniformConstant :: UnforcedDims Builtin -> ExtendedRational -> PureConstant
uniformConstant dims value = constantExpr $ TensorConstantValue dims value Nothing

atomiseConstant :: (MonadNorm Builtin m) => TensorConstantValue -> m PureConstant
atomiseConstant constant@(TensorConstantValue dims coefficient maybeValue) = case (maybeValue, coefficient) of
  (Nothing, _) -> return $ constantExpr constant
  (Just body, Finite c) -> return $ singletonAtom dims c body
  -- The coefficient map only holds finite coefficients, so fold an infinite one into the body.
  (Just _, _) -> singletonAtom dims 1 <$> foldConstant constant

singletonAtom :: UnforcedDims Builtin -> Coefficient -> Thunk Builtin -> PureConstant
singletonAtom dims coefficient body =
  Sparse (Map.singleton (Atom dims body) coefficient) (TensorConstantValue dims 0 Nothing)

substituteAtoms :: (MonadNorm Builtin m) => PureConstant -> m TensorConstantValue
substituteAtoms expr = do
  let dims = tensorValueDims $ constantValue expr
  let mkTerm (atom, coefficient) = mkTensorConstantValue dims (Finite coefficient) (atomBody atom)
  linearExprToExpr id mkTerm (addConstants 1 1) expr

reconstruct :: (MonadNorm Builtin m) => PureConstant -> m (Thunk Builtin)
reconstruct expr = foldConstant =<< substituteAtoms expr

isFinitePureConstant :: PureConstant -> Maybe Rational
isFinitePureConstant expr = isFiniteConstant =<< isConstant expr

combineWith ::
  (MonadNorm Builtin m) =>
  (TensorConstantValue -> TensorConstantValue -> m TensorConstantValue) ->
  PureConstant ->
  PureConstant ->
  m PureConstant
combineWith combine expr1 expr2 = case (isConstant expr1, isConstant expr2) of
  (Just c1, Just c2) -> constantExpr <$> combine c1 c2
  _ -> do
    c1 <- substituteAtoms expr1
    c2 <- substituteAtoms expr2
    atomiseConstant =<< combine c1 c2

instance (MonadNorm Builtin m) => ConstantLike PureConstant m where
  addConstants = addExprsUnsafe
  scaleConstant = scaleExpr
  minConstants = combineWith minTensorValues
  maxConstants = combineWith maxTensorValues
  toRatTensor expr = toRatTensor =<< substituteAtoms expr

  stackConstants elements
    | all (Map.null . coefficients) elements =
        constantExpr <$> stackTensorValues (fmap constantValue elements)
    | otherwise = do
        values <- traverse substituteAtoms elements
        atomiseConstant =<< stackTensorValues values

  unstackConstants expr = do
    constant <- substituteAtoms expr
    fmap constantExpr <$> unstackTensorValues constant
