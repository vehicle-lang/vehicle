{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Constants.TensorValue where

import Vehicle.Compile.Constants.TensorValue.Core
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Force (forceThunk)
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
  ( RatTensorValue (..),
    etaReduceTensor,
    toRatTensorValue,
  )
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Real
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensors of values

isFiniteConstant :: TensorConstantValue -> Maybe Rational
isFiniteConstant (TensorConstantValue _ coefficient maybeValue) = case (coefficient, maybeValue) of
  (Finite c, Nothing) -> Just c
  _ -> Nothing

mkTensorConstantValue ::
  (MonadNorm Builtin m) =>
  UnforcedDims Builtin ->
  ExtendedRational ->
  Thunk Builtin ->
  m TensorConstantValue
mkTensorConstantValue dims coeff thunk = do
  if coeff == 0
    then return $ TensorConstantValue dims 0 Nothing
    else do
      forced <- forceAndRewriteTensor thunk
      case toRatTensorValue forced of
        VRatTensorLiteral (ConstantTensor _ c) -> return $ TensorConstantValue dims (coeff * c) Nothing
        VRatConstTensor (ConstTensorArgs _ constVal _) -> do
          c <- forceAndRewriteTensor constVal
          case c of
            IRatLiteral v -> return $ TensorConstantValue dims v Nothing
            _ -> return $ TensorConstantValue dims coeff (Just thunk)
        VNegRatTensor args -> mkTensorConstantValue dims (-coeff) (tensorOp1Arg args)
        _ -> return $ TensorConstantValue dims coeff (Just thunk)

addDimensionedValue ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  TensorConstantValue ->
  m TensorConstantValue
addDimensionedValue x@(TensorConstantValue dims c1 v1) y@(TensorConstantValue _dims2 c2 v2)
  | c1 == 0 = return y
  | c2 == 0 = return x
  | otherwise = do
      (c, v1', v2') <-
        if c1 == c2
          then do
            v1' <- maybeValueToValue dims v1
            v2' <- maybeValueToValue dims v2
            return (c1, v1', v2')
          else do
            v1' <- foldConstant x
            v2' <- foldConstant y
            return (1, v1', v2')

      mkTensorConstantValue dims c =<< addThunks dims v1' v2'

mulDimensionedValue ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  TensorConstantValue ->
  m TensorConstantValue
mulDimensionedValue (TensorConstantValue dims c1 v1) (TensorConstantValue _dims2 c2 v2) = do
  value1 <- maybeValueToValue dims v1
  value2 <- maybeValueToValue dims v2
  mkTensorConstantValue dims (c1 * c2) =<< mulThunks dims value1 value2

{-
\| c1 == 0 = return y
\| c2 == 0 = return x
\| c1 == PosInfinity = _
\| c2 == PosInfinity = _
  -}

divDimensionedValue ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  TensorConstantValue ->
  m TensorConstantValue
divDimensionedValue (TensorConstantValue dims c1 v1) (TensorConstantValue _dims2 c2 v2) = do
  numerator <- maybeValueToValue dims v1
  denominator <- maybeValueToValue dims v2
  mkTensorConstantValue dims (c1 / c2) =<< divThunks dims numerator denominator

scaleDimensionedValue ::
  (MonadNorm Builtin m) =>
  Coefficient ->
  TensorConstantValue ->
  m TensorConstantValue
scaleDimensionedValue coeff (TensorConstantValue dims c maybeValue)
  | coeff == 0 = return $ TensorConstantValue dims 0 Nothing
  | otherwise = do
      value <- maybeValueToValue dims maybeValue
      mkTensorConstantValue dims (Finite coeff * c) value

addDimensionedConstants ::
  (MonadNorm Builtin m) =>
  AddConstants TensorConstantValue m
addDimensionedConstants c1 c2 v1 v2 = do
  cv1 <- scaleConstant c1 v1
  cv2 <- scaleConstant c2 v2
  addDimensionedValue cv1 cv2

dimensionedValueToRatTensor ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  m (Maybe RatTensor)
dimensionedValueToRatTensor x = do
  value <- forceThunk =<< foldConstant x
  case value of
    IRatTensor (toFiniteRatTensor -> Just t) -> return $ Just t
    _ -> return Nothing

minTensorValues ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  TensorConstantValue ->
  m TensorConstantValue
minTensorValues x@(TensorConstantValue dims _ _) y = do
  v1 <- foldConstant x
  v2 <- foldConstant y
  mkTensorConstantValue dims 1 =<< minThunks dims v1 v2

maxTensorValues ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  TensorConstantValue ->
  m TensorConstantValue
maxTensorValues x@(TensorConstantValue dims _ _) y = do
  v1 <- foldConstant x
  v2 <- foldConstant y
  mkTensorConstantValue dims 1 =<< maxThunks dims v1 v2

stackTensorValues ::
  (MonadNorm Builtin m) =>
  [TensorConstantValue] ->
  m TensorConstantValue
stackTensorValues = \case
  [] -> developerError "Cannot stack zero tensors"
  elements@(TensorConstantValue dims c _ : es) -> do
    let newDims = Forced $ IDimCons (Forced $ INatLiteral (length elements)) dims
    if all ((== c) . tensorCoefficient) es
      then do
        values <- traverse (maybeValueToValue dims . tensorValue) elements
        mkTensorConstantValue newDims c =<< stackThunks dims values
      else do
        values <- traverse foldConstant elements
        mkTensorConstantValue newDims 1 =<< stackThunks dims values

unstackTensorValues ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  m [TensorConstantValue]
unstackTensorValues (TensorConstantValue dims c maybeValue) = case dims of
  Forced (IDimCons (Forced (INatLiteral d)) ds) -> do
    elems <- case maybeValue of
      Nothing -> return $ replicate d Nothing
      Just value -> do
        let values = etaReduceTensor (Forced IRatType) d ds value
        return $ fmap Just values
    return $ fmap (TensorConstantValue ds c) elems
  _ -> developerError "Cannot unstack tensor with unknown dimensions"

instance (MonadNorm Builtin m) => ConstantLike TensorConstantValue m where
  addConstants = addDimensionedConstants
  scaleConstant = scaleDimensionedValue
  toRatTensor = dimensionedValueToRatTensor
  minConstants = minTensorValues
  maxConstants = maxTensorValues
  stackConstants = stackTensorValues
  unstackConstants = unstackTensorValues

--------------------------------------------------------------------------------
-- Helpers

maybeValueToValue ::
  (MonadNorm Builtin m) =>
  UnforcedDims Builtin ->
  Maybe (Thunk Builtin) ->
  m (Thunk Builtin)
maybeValueToValue dims = maybe (constThunk dims (Finite 1)) return

foldConstant ::
  (MonadNorm Builtin m) =>
  TensorConstantValue ->
  m (Thunk Builtin)
foldConstant (TensorConstantValue dims c maybeValue) = case maybeValue of
  Nothing -> constThunk dims c
  Just value
    | c == 1 -> return value
    | c == -1 -> negThunks dims value
    | otherwise -> do
        constTensor <- constThunk dims c
        mulThunks dims constTensor value

constThunk :: (MonadNorm Builtin m) => UnforcedDims Builtin -> ExtendedRational -> m (Thunk Builtin)
constThunk dims value =
  forceEvaluation accessConstTensor evalConstTensor $
    ConstTensorArgs
      { constType = Forced IRatType,
        constValue = Forced $ IRatLiteral value,
        constDims = dims
      }

negThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> Thunk Builtin -> m (Thunk Builtin)
negThunks dims x =
  forceEvaluation accessNegRatTensor evalNegRatTensor $
    TensorOp1Args
      { tensorOp1Dims = dims,
        tensorOp1Arg = x
      }

addThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> Thunk Builtin -> Thunk Builtin -> m (Thunk Builtin)
addThunks dims x y =
  forceEvaluation accessAddRatTensor evalAddRatTensor $
    TensorOp2Args
      { tensorOp2Dims = dims,
        tensorOp2Arg1 = x,
        tensorOp2Arg2 = y
      }

mulThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> Thunk Builtin -> Thunk Builtin -> m (Thunk Builtin)
mulThunks dims x y =
  forceEvaluation accessMulRatTensor evalMulRatTensor $
    TensorOp2Args
      { tensorOp2Dims = dims,
        tensorOp2Arg1 = x,
        tensorOp2Arg2 = y
      }

divThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> Thunk Builtin -> Thunk Builtin -> m (Thunk Builtin)
divThunks dims x y =
  forceEvaluation accessDivRatTensor evalDivRatTensor $
    TensorOp2Args
      { tensorOp2Dims = dims,
        tensorOp2Arg1 = x,
        tensorOp2Arg2 = y
      }

minThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> Thunk Builtin -> Thunk Builtin -> m (Thunk Builtin)
minThunks dims x y =
  forceEvaluation accessMinRatTensor evalMinRatTensor $
    TensorOp2Args
      { tensorOp2Dims = dims,
        tensorOp2Arg1 = x,
        tensorOp2Arg2 = y
      }

maxThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> Thunk Builtin -> Thunk Builtin -> m (Thunk Builtin)
maxThunks dims x y =
  forceEvaluation accessMaxRatTensor evalMaxRatTensor $
    TensorOp2Args
      { tensorOp2Dims = dims,
        tensorOp2Arg1 = x,
        tensorOp2Arg2 = y
      }

stackThunks :: (MonadNorm Builtin m) => UnforcedDims Builtin -> [Thunk Builtin] -> m (Thunk Builtin)
stackThunks dims elements =
  forceEvaluation accessStackTensor evalStackTensor $
    StackTensorArgs
      { stackType = Forced IRatType,
        stackFirstDim = Forced $ INatLiteral (length elements),
        stackRemainingDims = dims,
        stackElements = elements
      }
