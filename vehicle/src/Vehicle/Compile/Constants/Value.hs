{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Constants.Value where

import Vehicle.Data.Assertion
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView (etaReduceTensor)
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude
import Vehicle.Prelude.Logging

--------------------------------------------------------------------------------
-- Tensors of values

type TensorValue = DimensionedTensorValue LossBuiltin

type TensorValueLinearExpr = LinearExpr SliceVariable TensorValue

tensorValueLinarExprToValue :: LinearExpr SliceVariable TensorValue -> TensorValue
tensorValueLinarExprToValue linearExpr = do
  let dims = tensorValueDims $ constantValue linearExpr
  let mkVarTerm v = TensorValue dims (VBoundVar (toLv v) [])
  let mkTerm (v, coeff) = scaleConstant coeff (mkVarTerm v)
  linearExprToExpr id mkTerm (addConstants 1 1) linearExpr

type UserVariableConstraint = Assertion TensorValueLinearExpr

-- | An `AssertionTree` represents a boolean expression with assertions at
-- each terminal leaf.
type UserVariableConstraintTree = BooleanExpr UserVariableConstraint

constantDimensionedValue :: VDims LossBuiltin -> Rational -> TensorValue
constantDimensionedValue dims constant =
  TensorValue dims $
    runSilentLogger $
      evalConstTensor $
        ConstTensorArgs
          { constType = IRatType,
            constValue = IRatLiteral constant,
            constDims = dims
          }

addDimensionedValue :: TensorValue -> TensorValue -> TensorValue
addDimensionedValue (TensorValue dims1 e1) (TensorValue _dims2 e2) = do
  TensorValue dims1 $
    runSilentLogger $
      evalAddRatTensor $
        TensorOp2Args dims1 e1 e2

scaleDimensionedValue :: Coefficient -> TensorValue -> TensorValue
scaleDimensionedValue c (TensorValue dims e) = do
  let constant = tensorValue $ constantDimensionedValue dims c
  let e' = runSilentLogger $ evalMulRatTensor $ TensorOp2Args dims constant e
  TensorValue dims e'

addDimensionedConstants :: AddConstants TensorValue
addDimensionedConstants c1 c2 v1 v2 = do
  let cv1 = scaleConstant c1 v1
  let cv2 = scaleConstant c2 v2
  addDimensionedValue cv1 cv2

dimensionedValueToRatTensor :: TensorValue -> Maybe RatTensor
dimensionedValueToRatTensor (TensorValue _ e1) = case e1 of
  IRatTensor t -> Just t
  _ -> Nothing

minTensorValues :: TensorValue -> TensorValue -> TensorValue
minTensorValues (TensorValue dims v1) (TensorValue _ v2) =
  TensorValue dims $
    runSilentLogger $
      evalMinRatTensor $
        TensorOp2Args
          { tensorOp2Dims = dims,
            tensorOp2Arg1 = v1,
            tensorOp2Arg2 = v2
          }

maxTensorValues :: TensorValue -> TensorValue -> TensorValue
maxTensorValues (TensorValue dims v1) (TensorValue _ v2) =
  TensorValue dims $
    runSilentLogger $
      evalMaxRatTensor $
        TensorOp2Args
          { tensorOp2Dims = dims,
            tensorOp2Arg1 = v1,
            tensorOp2Arg2 = v2
          }

stackTensorValues :: [TensorValue] -> TensorValue
stackTensorValues = \case
  [] -> developerError "Cannot stack zero tensors"
  elements@(TensorValue dims _ : _) -> do
    let newDim = INatLiteral (length elements)
    let newDims = IDimCons newDim dims
    TensorValue newDims $
      runSilentLogger $
        evalStackTensor $
          StackTensorArgs
            { stackType = IRatType,
              stackFirstDim = newDim,
              stackRemainingDims = dims,
              stackElements = fmap tensorValue elements
            }

unstackTensorValues :: TensorValue -> [TensorValue]
unstackTensorValues (TensorValue dims value) = case dims of
  IDimCons (INatLiteral d) ds -> do
    let values = runSilentLogger $ etaReduceTensor IRatType d ds value
    fmap (TensorValue ds) values
  _ -> developerError "Cannot unstack tensor with unknown dimensions"

instance ConstantLike TensorValue where
  addConstants = addDimensionedConstants
  scaleConstant = scaleDimensionedValue
  toRatTensor = dimensionedValueToRatTensor
  minConstants = minTensorValues
  maxConstants = maxTensorValues
  stackConstants = stackTensorValues
  unstackConstants = unstackTensorValues
