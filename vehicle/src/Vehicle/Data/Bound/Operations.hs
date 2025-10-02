module Vehicle.Data.Bound.Operations where

import Control.Monad (foldM, zipWithM)
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (appendList, toList)
import Data.Map qualified as Map
import Data.These (These (..), fromThese)
import Vehicle.Compile.Error (UnboundedIndices)
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion (Assertion, InequalityRelation (..), NormalisedRelation (..), Relation (..), combineInequalityRelations)
import Vehicle.Data.Bound
import Vehicle.Data.Builtin.Interface.Normalise (evalMaxRatTensor, evalMinRatTensor, evalStackTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView (etaReduceTensor, scaleValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (RatTensor, TensorIndices, TensorShape, mapTensor, stack, unstack, zipWithTensor)
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Level (SliceVariable, TensorVariable, TensorVariableLike (toTensorVar))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

--------------------------------------------------------------------------------
-- IsBound

class IsBoundValue value where
  scaleVal :: Value Builtin -> Coefficient -> value -> value
  minVal :: (MonadLogger m) => Value Builtin -> value -> value -> m value
  maxVal :: (MonadLogger m) => Value Builtin -> value -> value -> m value
  stackVals :: (MonadLogger m) => Value Builtin -> [value] -> m value
  unstackVals :: (MonadLogger m, MonadNameContext m, MonadFreeContext Builtin m) => Int -> Value Builtin -> value -> m [value]

instance IsBoundValue (Value Builtin) where
  scaleVal = scaleValue

  minVal dims v1 v2 =
    evalMinRatTensor $
      TensorOp2Args
        { tensorOp2Dims = implicitIrrelevant dims,
          tensorOp2Arg1 = v1,
          tensorOp2Arg2 = v2
        }

  maxVal dims v1 v2 =
    evalMaxRatTensor $
      TensorOp2Args
        { tensorOp2Dims = implicitIrrelevant dims,
          tensorOp2Arg1 = v1,
          tensorOp2Arg2 = v2
        }

  stackVals remainingDims elements =
    evalStackTensor $
      StackTensorArgs
        { stackType = implicit IRatType,
          stackFirstDim = INatLiteral (length elements),
          stackRemainingDims = implicitIrrelevant remainingDims,
          stackElements = elements
        }

  unstackVals dim dims bound = do
    nameCtx <- getNameContext
    etaReduceTensor nameCtx IRatType dim dims bound

instance IsBoundValue RatTensor where
  scaleVal _dims coef = mapTensor (* coef)

  minVal _dims v1 v2 = return $ zipWithTensor min v1 v2

  maxVal _dims v1 v2 = return $ zipWithTensor max v1 v2

  stackVals dims rows = case getDims dims of
    Just ds -> return $ stack ds rows
    Nothing -> developerError "When bounds are concrete tensors, all dimensions should be known."

  unstackVals _dim _dims bound = return $ unstack bound

--------------------------------------------------------------------------------
-- IsBoundValue

class (IsBoundValue value) => IsBound bound value where
  andBound :: (MonadLogger m) => Value Builtin -> bound value -> bound value -> m (bound value)
  boundToValue :: bound value -> (InequalityRelation, value)
  valueToBound :: (InequalityRelation, value) -> bound value

instance (IsBoundValue value) => IsBound LowerBound value where
  andBound dims (LowerBound r1 v1) (LowerBound r2 v2) = do
    newValue <- maxVal dims v1 v2
    let newRelation = combineInequalityRelations r1 r2
    return $ LowerBound newRelation newValue

  boundToValue (LowerBound rel value) = (rel, value)

  valueToBound = uncurry LowerBound

instance (IsBoundValue value) => IsBound UpperBound value where
  andBound dims (UpperBound r1 v1) (UpperBound r2 v2) = do
    newValue <- minVal dims v1 v2
    let newRelation = combineInequalityRelations r1 r2
    return $ UpperBound newRelation newValue

  boundToValue (UpperBound rel value) = (rel, value)

  valueToBound = uncurry UpperBound

stackBounds ::
  (IsBound bound value, MonadLogger m) =>
  VDims Builtin ->
  [bound value] ->
  m (bound value)
stackBounds dims bounds = do
  let (rels, values) = unzipWith boundToValue bounds
  stackedValue <- stackVals dims values
  let stackedRel = foldr combineInequalityRelations NonStrict rels
  return $ valueToBound (stackedRel, stackedValue)

unstackBounds ::
  (IsBound bound value, MonadLogger m, MonadNameContext m, MonadFreeContext Builtin m) =>
  Int ->
  VDims Builtin ->
  bound value ->
  m [bound value]
unstackBounds dim dims bound = do
  let (rel, value) = boundToValue bound
  stackedValues <- unstackVals dim dims value
  return $ fmap (\val -> valueToBound (rel, val)) stackedValues

andBoundList ::
  (MonadLogger m, IsBound bound value) =>
  VDims Builtin ->
  [bound value] ->
  m (Maybe (bound value))
andBoundList dims = \case
  [] -> return Nothing
  x : xs -> Just <$> foldM (andBound dims) x xs

--------------------------------------------------------------------------------
-- Instantiating bounds

singleSliceBound ::
  bound ->
  TensorShape ->
  TensorIndices ->
  SliceBounds bound
singleSliceBound bound shape indices = case (shape, indices) of
  (_, []) -> SliceBounds (Just bound) Nothing
  (d : ds, idx : idxs) -> do
    let childBounds = [if i == idx then singleSliceBound bound ds idxs else emptySliceBounds | i <- [0 .. d - 1]]
    SliceBounds Nothing (Just childBounds)
  _ -> developerError "Malformed shape and indices"

tryToConvertToTensorBounds ::
  (IsBoundValue value, TensorVariableLike variable, Show value) =>
  (SliceVariable -> Maybe (variable, TensorShape, Maybe (Value Builtin), TensorIndices)) ->
  Assertion (LinearExpr SliceVariable value) ->
  Maybe (variable, TensorBounds value)
tryToConvertToTensorBounds lookupTensorVar (NormalisedRelation rel expr) = do
  case Map.toList (coefficients expr) of
    [(var, coef)] -> case lookupTensorVar var of
      Just (parentVar, knownShape, unknownShape, indices) -> do
        let partialShape = toPartialShape knownShape unknownShape
        let bounds = convertToTensorBounds partialShape (toTensorVar parentVar) indices rel coef (constantValue expr)
        Just (parentVar, bounds)
      _ -> Nothing
    _ -> Nothing

convertToTensorBounds ::
  (IsBoundValue value, Show value) =>
  PartiallyKnownTensorShape ->
  TensorVariable ->
  TensorIndices ->
  Relation ->
  Coefficient ->
  value ->
  TensorBounds value
convertToTensorBounds shape var indices rel coeff bound = do
  let dims = calculateCurrentDimensions shape indices
  -- let (prevCoefficient, rearrangedExpr) = rearrangeExprToSolveForBase (scaleBound dims) (toSliceVar var) linearExpr

  -- c*x + b REL 0   -> x REL (-1/c)*b
  let strictness = case rel of
        OEq -> These NonStrict NonStrict
        OLt
          | coeff < 0 -> This Strict
          | otherwise -> That Strict
        OLe
          | coeff < 0 -> This NonStrict
          | otherwise -> That NonStrict

  let scaledBound = scaleVal dims (-(1 / coeff)) bound
  let mkLowerBound strict = singleSliceBound (LowerBound strict scaledBound) (knownPrefix shape) indices
  let mkUpperBound strict = singleSliceBound (UpperBound strict scaledBound) (knownPrefix shape) indices

  let theseBounds = bimap mkLowerBound mkUpperBound strictness
  let tupleBounds = fromThese emptySliceBounds emptySliceBounds theseBounds
  uncurry (TensorBounds var shape) tupleBounds

--------------------------------------------------------------------------------
-- Conjunction of bounds

andBounds :: Bounds expr -> Bounds expr -> Bounds expr
andBounds (Bounds lower1 upper1) (Bounds lower2 upper2) =
  Bounds (lower1 <> lower2) (upper1 <> upper2)

andSliceBounds ::
  forall m bound value.
  (MonadTraverseTensor m, MonadLogger m, IsBound bound value) =>
  SliceBounds (bound value) ->
  SliceBounds (bound value) ->
  m (SliceBounds (bound value))
andSliceBounds (SliceBounds bound1 childBounds1) (SliceBounds bound2 childBounds2) = do
  dims <- currentDimensions
  newBounds <- unionMaybeWithM (andBound dims) bound1 bound2
  newChildBounds <- unionMaybeWithM andChildSliceBounds childBounds1 childBounds2
  return $ SliceBounds newBounds newChildBounds
  where
    andChildSliceBounds :: [SliceBounds (bound value)] -> [SliceBounds (bound value)] -> m [SliceBounds (bound value)]
    andChildSliceBounds u v = traverseTensorRows (uncurry andSliceBounds) (zip u v)

andTensorBounds ::
  (MonadLogger m, IsBoundValue value) =>
  TensorBounds value ->
  TensorBounds value ->
  m (TensorBounds value)
andTensorBounds (TensorBounds var shape l1 u1) (TensorBounds _ _ l2 u2) =
  runTraverseTensorT shape $ do
    newLowerBounds <- andSliceBounds l1 l2
    newUpperBounds <- andSliceBounds u1 u2
    return $
      TensorBounds
        { boundedVar = var,
          partialShape = shape,
          tensorLowerBounds = newLowerBounds,
          tensorUpperBounds = newUpperBounds
        }

--------------------------------------------------------------------------------
-- Flattening bounds

flattenSliceBounds ::
  forall m bound value.
  (MonadLogger m, MonadNameContext m, MonadFreeContext Builtin m, MonadTraverseTensor m, IsBound bound value) =>
  SliceBounds (bound value) ->
  m (Either (NonEmpty TensorIndices) (bound value))
flattenSliceBounds = go
  where
    go ::
      SliceBounds (bound value) ->
      m (Either (NonEmpty TensorIndices) (bound value))
    go (SliceBounds maybeBound maybeChildBounds) = case maybeChildBounds of
      Nothing -> case maybeBound of
        Just bound -> return $ Right bound
        Nothing -> do
          indices <- currentIndices
          return $ Left [indices]
      Just childTensorBounds -> do
        childDims <- childDimensions
        childErrorOrBounds <- traverseTensorRows go childTensorBounds
        let (missingChildIndices, childBounds) = partitionEithers childErrorOrBounds
        case maybeBound of
          Nothing -> case missingChildIndices of
            i : is -> return $ Left (NonEmpty.appendList i (concatMap NonEmpty.toList is))
            [] -> Right <$> stackBounds childDims childBounds
          Just bound -> do
            dims <- currentDimensions
            case missingChildIndices of
              _ : _ -> do
                boundElements <- unstackBounds (length childTensorBounds) childDims bound
                let combineBound boundElement = either (return . const boundElement) (andBound dims boundElement)
                combinedElements <- zipWithM combineBound boundElements childErrorOrBounds
                Right <$> stackBounds childDims combinedElements
              [] -> do
                childBound <- stackBounds childDims childBounds
                Right <$> andBound dims bound childBound

-- | Takes in bounds over a tensor and tries to compute a single lower and upper bound for the
-- whole tensor. If it fails then it returns a list of the indices of the tensor which
-- are unbounded.
--
-- NOTE: this function is currently unsound as at the moment it discards the strictness information.
-- See https://github.com/vehicle-lang/vehicle/issues/74
flattenTensorBounds ::
  (MonadLogger m, MonadNameContext m, MonadFreeContext Builtin m, IsBoundValue value) =>
  TensorBounds value ->
  m (Either UnboundedIndices (value, value))
flattenTensorBounds TensorBounds {..} = do
  errorOrLowerBound <- runTraverseTensorT partialShape (flattenSliceBounds tensorLowerBounds)
  errorOrUpperBounds <- runTraverseTensorT partialShape (flattenSliceBounds tensorUpperBounds)
  let mkBound (LowerBound _ lower) (UpperBound _ upper) = (lower, upper)
  return $ theseErrors mkBound errorOrLowerBound errorOrUpperBounds
