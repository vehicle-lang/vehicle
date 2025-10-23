module Vehicle.Data.Bound where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.These (These (..), fromThese)
import GHC.Generics
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.Tensor (TensorIndices, TensorShape)
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Level

--------------------------------------------------------------------------------
-- BoundedValue

-- | A value paired with some bounds.
-- Frequently used with `printFriendly` and `printVerbose`
data BoundedValue var bounds = BoundedValue var bounds

type UnboundedIndices = These (NonEmpty TensorIndices) (NonEmpty TensorIndices)

--------------------------------------------------------------------------------
-- IsBound

class (ConstantLike constant) => IsBound bound constant where
  andBounds :: bound constant -> bound constant -> bound constant
  boundToValue :: bound constant -> (InequalityRelation, constant)
  valueToBound :: (InequalityRelation, constant) -> bound constant

stackBounds ::
  (IsBound bound constant, MonadLogger m) =>
  [bound constant] ->
  m (bound constant)
stackBounds bounds = do
  let (rels, values) = unzipWith boundToValue bounds
  let stackedValue = stackConstants values
  let stackedRel = foldr combineInequalityRelations NonStrict rels
  return $ valueToBound (stackedRel, stackedValue)

unstackBounds ::
  (IsBound bound constant, MonadLogger m) =>
  bound constant ->
  m [bound constant]
unstackBounds bound = do
  let (rel, constant) = boundToValue bound
  let stackedValues = unstackConstants constant
  return $ fmap (\val -> valueToBound (rel, val)) stackedValues

andBoundList ::
  (MonadLogger m, IsBound bound constant) =>
  [bound constant] ->
  m (Maybe (bound constant))
andBoundList = \case
  [] -> return Nothing
  x : xs -> return $ Just $ foldr andBounds x xs

--------------------------------------------------------------------------------
-- Lower bounds

-- | A single lower bound on some value
data LowerBound expr = LowerBound InequalityRelation expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (LowerBound expr)

instance (ToJSON expr) => ToJSON (LowerBound expr)

instance (FromJSON expr) => FromJSON (LowerBound expr)

instance (ConstantLike constant) => IsBound LowerBound constant where
  andBounds (LowerBound r1 v1) (LowerBound r2 v2) = do
    let newValue = maxConstants v1 v2
    let newRelation = combineInequalityRelations r1 r2
    LowerBound newRelation newValue

  boundToValue (LowerBound rel value) = (rel, value)

  valueToBound = uncurry LowerBound

--------------------------------------------------------------------------------
-- UpperBound

-- | A single upper bound on some value
data UpperBound expr = UpperBound InequalityRelation expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (UpperBound expr)

instance (ToJSON expr) => ToJSON (UpperBound expr)

instance (FromJSON expr) => FromJSON (UpperBound expr)

instance (ConstantLike constant) => IsBound UpperBound constant where
  andBounds (UpperBound r1 v1) (UpperBound r2 v2) = do
    let newValue = minConstants v1 v2
    let newRelation = combineInequalityRelations r1 r2
    UpperBound newRelation newValue

  boundToValue (UpperBound rel value) = (rel, value)

  valueToBound = uncurry UpperBound

--------------------------------------------------------------------------------
-- SliceBounds

-- | A collection of lower and upper bounds on some particular slice of a tensor
data SliceBounds expr = SliceBounds
  { lowerBounds :: [LowerBound expr],
    upperBounds :: [UpperBound expr]
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData expr) => NFData (SliceBounds expr)

instance (ToJSON expr) => ToJSON (SliceBounds expr)

instance (FromJSON expr) => FromJSON (SliceBounds expr)

andSliceBounds :: SliceBounds expr -> SliceBounds expr -> SliceBounds expr
andSliceBounds (SliceBounds lower1 upper1) (SliceBounds lower2 upper2) =
  SliceBounds (lower1 <> lower2) (upper1 <> upper2)

--------------------------------------------------------------------------------
-- A collection of one-sided bounds over

-- | A collection of one-sided bounds on slices of a tensor
data NestedSliceBounds bound = NestedSliceBounds
  { _sliceBound :: Maybe bound,
    _childConstraints :: Maybe [NestedSliceBounds bound]
  }
  deriving (Show)

emptyNestedSliceBounds :: NestedSliceBounds bound
emptyNestedSliceBounds = NestedSliceBounds Nothing Nothing

isEmptyNestedSliceBounds :: NestedSliceBounds bound -> Bool
isEmptyNestedSliceBounds = \case
  NestedSliceBounds Nothing Nothing -> True
  _ -> False

singleNestedSliceBound ::
  bound ->
  TensorShape ->
  TensorIndices ->
  NestedSliceBounds bound
singleNestedSliceBound bound shape indices = case (shape, indices) of
  (_, []) -> NestedSliceBounds (Just bound) Nothing
  (d : ds, idx : idxs) -> do
    let childBounds = [if i == idx then singleNestedSliceBound bound ds idxs else emptyNestedSliceBounds | i <- [0 .. d - 1]]
    NestedSliceBounds Nothing (Just childBounds)
  _ -> developerError "Malformed shape and indices"

prettyNestedSliceBounds ::
  forall m bound a.
  (MonadTraverseTensor m) =>
  (bound -> Doc a) ->
  NestedSliceBounds bound ->
  m [Doc a]
prettyNestedSliceBounds prettyBound = go
  where
    go :: NestedSliceBounds bound -> m [Doc a]
    go (NestedSliceBounds maybeBound maybeChildBounds) = do
      let prettySingleBound bound = [prettyBound bound]
      let prettyChildBounds = traverseTensorRows go

      let boundDoc = maybe [] prettySingleBound maybeBound
      childBoundDocs <- maybe (return []) prettyChildBounds maybeChildBounds
      return $ boundDoc <> concat childBoundDocs

andNestedSliceBounds ::
  forall m bound constant.
  (MonadTraverseTensor m, MonadLogger m, IsBound bound constant) =>
  NestedSliceBounds (bound constant) ->
  NestedSliceBounds (bound constant) ->
  m (NestedSliceBounds (bound constant))
andNestedSliceBounds (NestedSliceBounds bound1 childBounds1) (NestedSliceBounds bound2 childBounds2) = do
  let newBounds = unionMaybeWith andBounds bound1 bound2
  newChildBounds <- unionMaybeWithM andChildNestedSliceBounds childBounds1 childBounds2
  return $ NestedSliceBounds newBounds newChildBounds
  where
    andChildNestedSliceBounds :: [NestedSliceBounds (bound constant)] -> [NestedSliceBounds (bound constant)] -> m [NestedSliceBounds (bound constant)]
    andChildNestedSliceBounds u v = traverseTensorRows (uncurry andNestedSliceBounds) (zip u v)

flattenNestedSliceBounds ::
  forall m bound constant.
  (MonadLogger m, MonadNameContext m, MonadTraverseTensor m, IsBound bound constant) =>
  NestedSliceBounds (bound constant) ->
  m (Either (NonEmpty TensorIndices) (bound constant))
flattenNestedSliceBounds = go
  where
    go ::
      NestedSliceBounds (bound constant) ->
      m (Either (NonEmpty TensorIndices) (bound constant))
    go (NestedSliceBounds maybeBound maybeChildBounds) = case maybeChildBounds of
      Nothing -> case maybeBound of
        Just bound -> return $ Right bound
        Nothing -> do
          indices <- currentIndices
          return $ Left [indices]
      Just childTensorBounds -> do
        childErrorOrBounds <- traverseTensorRows go childTensorBounds
        let (missingChildIndices, childBounds) = partitionEithers childErrorOrBounds
        case maybeBound of
          Nothing -> case missingChildIndices of
            i : is -> return $ Left $ concatNonEmpty $ i :| is
            [] -> Right <$> stackBounds childBounds
          Just bound -> do
            case missingChildIndices of
              _ : _ -> do
                boundElements <- unstackBounds bound
                let combineBound boundElement = either (const boundElement) (andBounds boundElement)
                let combinedElements = zipWith combineBound boundElements childErrorOrBounds
                Right <$> stackBounds combinedElements
              [] -> do
                childBound <- stackBounds childBounds
                return $ Right $ andBounds bound childBound

-------------------------------------------------------------------------------
-- Tensor bounds

-- | This is a data structure for representing partial bounds on a tensor
-- variable and is capable of storing bounds over each nested row in the
-- tensor individually.
data TensorBounds bound = TensorBounds
  { boundedVar :: TensorVariable,
    partialShape :: PartiallyKnownTensorShape,
    tensorLowerBounds :: NestedSliceBounds (LowerBound bound),
    tensorUpperBounds :: NestedSliceBounds (UpperBound bound)
  }

isEmptyTensorBounds :: TensorBounds bound -> Bool
isEmptyTensorBounds TensorBounds {..} =
  isEmptyNestedSliceBounds tensorLowerBounds && isEmptyNestedSliceBounds tensorUpperBounds

emptyTensorBounds :: (TensorVariableLike variable) => variable -> PartiallyKnownTensorShape -> TensorBounds bound
emptyTensorBounds var shape =
  TensorBounds
    { boundedVar = toTensorVar var,
      partialShape = shape,
      tensorLowerBounds = emptyNestedSliceBounds,
      tensorUpperBounds = emptyNestedSliceBounds
    }

andTensorBounds ::
  (MonadLogger m, ConstantLike constant) =>
  TensorBounds constant ->
  TensorBounds constant ->
  m (TensorBounds constant)
andTensorBounds (TensorBounds var shape l1 u1) (TensorBounds _ _ l2 u2) =
  runTraverseTensorT shape $ do
    newLowerBounds <- andNestedSliceBounds l1 l2
    newUpperBounds <- andNestedSliceBounds u1 u2
    return $
      TensorBounds
        { boundedVar = var,
          partialShape = shape,
          tensorLowerBounds = newLowerBounds,
          tensorUpperBounds = newUpperBounds
        }

--------------------------------------------------------------------------------
-- Instantiating bounds

data VariableInfo = VariableInfo
  { parentVariable :: TensorVariable,
    parentShape :: PartiallyKnownTensorShape,
    indices :: TensorIndices
  }

tryToConvertToTensorBounds ::
  (ConstantLike constant, Show constant) =>
  (SliceVariable -> Maybe VariableInfo) ->
  Assertion (LinearExpr SliceVariable constant) ->
  Maybe (TensorVariable, TensorBounds constant)
tryToConvertToTensorBounds lookupTensorVar (NormalisedRelation rel expr) = do
  case Map.toList (coefficients expr) of
    [(var, coef)] -> case lookupTensorVar var of
      Just VariableInfo {..} -> do
        let bounds = convertToTensorBounds parentShape parentVariable indices rel coef (constantValue expr)
        Just (parentVariable, bounds)
      _ -> Nothing
    _ -> Nothing

convertToTensorBounds ::
  (ConstantLike constant, Show constant) =>
  PartiallyKnownTensorShape ->
  TensorVariable ->
  TensorIndices ->
  Relation ->
  Coefficient ->
  constant ->
  TensorBounds constant
convertToTensorBounds shape var indices rel coeff bound = do
  -- c*x + b REL 0   -> x REL (-1/c)*b
  let strictness = case rel of
        OEq -> These NonStrict NonStrict
        OLt
          | coeff < 0 -> This Strict
          | otherwise -> That Strict
        OLe
          | coeff < 0 -> This NonStrict
          | otherwise -> That NonStrict

  let scaledBound = scaleConstant (-(1 / coeff)) bound
  let mkLowerBound strict = singleNestedSliceBound (LowerBound strict scaledBound) (knownPrefix shape) indices
  let mkUpperBound strict = singleNestedSliceBound (UpperBound strict scaledBound) (knownPrefix shape) indices

  let theseBounds = bimap mkLowerBound mkUpperBound strictness
  let tupleBounds = fromThese emptyNestedSliceBounds emptyNestedSliceBounds theseBounds
  uncurry (TensorBounds var shape) tupleBounds

-- | Takes in bounds over a tensor and tries to compute a single lower and upper bound for the
-- whole tensor. If it fails then it returns a list of the indices of the tensor which
-- are unbounded.
--
-- NOTE: this function is currently unsound as at the moment it discards the strictness information.
-- See https://github.com/vehicle-lang/vehicle/issues/74
flattenTensorBounds ::
  (MonadLogger m, MonadNameContext m, ConstantLike constant) =>
  TensorBounds constant ->
  m (Either UnboundedIndices (constant, constant))
flattenTensorBounds TensorBounds {..} = do
  errorOrLowerBound <- runTraverseTensorT partialShape (flattenNestedSliceBounds tensorLowerBounds)
  errorOrUpperBounds <- runTraverseTensorT partialShape (flattenNestedSliceBounds tensorUpperBounds)
  let mkBound (LowerBound _ lower) (UpperBound _ upper) = (lower, upper)
  return $ theseErrors mkBound errorOrLowerBound errorOrUpperBounds
