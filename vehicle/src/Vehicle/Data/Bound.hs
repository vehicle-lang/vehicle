module Vehicle.Data.Bound where

import Control.DeepSeq (NFData)
import Control.Monad (mapAndUnzipM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.These (These (..))
import GHC.Generics
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.Tensor (HasShape (..), RatTensor, TensorIndices, TensorShape, allTensor, anyTensor, zipWithTensor)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Tensor.Core (KnownPrefixOfTensorShape)
import Vehicle.Data.Variable.Bound.Level

--------------------------------------------------------------------------------
-- BoundedValue

-- | A value paired with some bounds.
-- Frequently used with `printFriendly` and `printVerbose`
data BoundedValue value bounds = BoundedValue
  { boundedValue :: value,
    valueBounds :: bounds
  }
  deriving stock (Show)

instance Functor (BoundedValue value) where
  fmap f (BoundedValue var bounds) = BoundedValue var (f bounds)

instance Bifunctor BoundedValue where
  bimap f g (BoundedValue var bounds) = BoundedValue (f var) (g bounds)

type UnboundedIndices = These (NonEmpty TensorIndices) (NonEmpty TensorIndices)

wholeTensorUnbounded :: UnboundedIndices
wholeTensorUnbounded = These [[]] [[]]

--------------------------------------------------------------------------------
-- IsBound

class (Monad m, ConstantLike constant m) => IsBound bound constant m where
  andBound :: bound constant -> bound constant -> m (bound constant)
  boundToValue :: bound constant -> m (InequalityRelation, constant)
  valueToBound :: (InequalityRelation, constant) -> m (bound constant)

stackBounds ::
  (IsBound bound constant m) =>
  [bound constant] ->
  m (bound constant)
stackBounds bounds = do
  (rels, values) <- mapAndUnzipM boundToValue bounds
  stackedValue <- stackConstants values
  let stackedRel = foldr combineInequalityRelations NonStrict rels
  valueToBound (stackedRel, stackedValue)

unstackBounds ::
  (IsBound bound constant m) =>
  bound constant ->
  m [bound constant]
unstackBounds bound = do
  (rel, constant) <- boundToValue bound
  stackedValues <- unstackConstants constant
  traverse (\val -> valueToBound (rel, val)) stackedValues

andBoundList ::
  (IsBound bound constant m) =>
  [bound constant] ->
  m (Maybe (bound constant))
andBoundList = \case
  [] -> return Nothing
  x : xs -> Just <$> foldrM andBound x xs

--------------------------------------------------------------------------------
-- Lower bounds

-- | A single lower bound on some value
data LowerBound expr = LowerBound
  { lowerBoundRel :: InequalityRelation,
    lowerBoundValue :: expr
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance (HasShape expr) => HasShape (LowerBound expr) where
  shapeOf = shapeOf . lowerBoundValue

instance (ConstantLike expr m) => IsBound LowerBound expr m where
  andBound (LowerBound r1 v1) (LowerBound r2 v2) = do
    newValue <- maxConstants v1 v2
    let newRelation = combineInequalityRelations r1 r2
    return $ LowerBound newRelation newValue

  boundToValue (LowerBound rel value) = return (rel, value)

  valueToBound = return . uncurry LowerBound

--------------------------------------------------------------------------------
-- UpperBound

-- | A single upper bound on some value
data UpperBound expr = UpperBound
  { upperBoundRel :: InequalityRelation,
    upperBoundValue :: expr
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance (ConstantLike expr m) => IsBound UpperBound expr m where
  andBound (UpperBound r1 v1) (UpperBound r2 v2) = do
    newValue <- minConstants v1 v2
    let newRelation = combineInequalityRelations r1 r2
    return $ UpperBound newRelation newValue

  boundToValue (UpperBound rel value) = return (rel, value)

  valueToBound = return . uncurry UpperBound

--------------------------------------------------------------------------------
-- Bounds

class IsBounds bounds constant where
  emptyBounds :: bounds constant
  andBounds :: bounds constant -> bounds constant -> bounds constant

isEmptyBounds :: (Eq (bounds constant)) => (IsBounds bounds constant) => bounds constant -> Bool
isEmptyBounds bounds = bounds == emptyBounds

--------------------------------------------------------------------------------
-- SliceBounds

-- | A collection of lower and upper bounds on some particular slice of a tensor
data SliceBounds expr = SliceBounds
  { lowerBounds :: [LowerBound expr],
    upperBounds :: [UpperBound expr]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance IsBounds SliceBounds constant where
  emptyBounds = SliceBounds mempty mempty

  andBounds (SliceBounds lower1 upper1) (SliceBounds lower2 upper2) =
    SliceBounds (lower1 <> lower2) (upper1 <> upper2)

--------------------------------------------------------------------------------
-- A collection of one-sided bounds over

-- | A collection of one-sided bounds on slices of a tensor
data NestedSliceBounds expr = NestedSliceBounds
  { sliceBounds :: SliceBounds expr,
    childSliceBounds :: Maybe [NestedSliceBounds expr]
  }
  deriving stock (Show, Eq, Ord)

singleNestedSliceBound ::
  SliceBounds expr ->
  TensorShape ->
  TensorIndices ->
  NestedSliceBounds expr
singleNestedSliceBound bounds shape indices = case (shape, indices) of
  (_, []) -> NestedSliceBounds bounds Nothing
  (d : ds, idx : idxs) -> do
    let childBounds = [if i == idx then singleNestedSliceBound bounds ds idxs else emptyBounds | i <- [0 .. d - 1]]
    NestedSliceBounds emptyBounds (Just childBounds)
  _ -> developerError "Malformed shape and indices"

-- | Builds the nested bounds for a collection of slice bounds in one pass.
-- Equivalent to `foldr1 andBounds . map (\(is, b) -> singleNestedSliceBound b shape is)`
-- but linear rather than quadratic in the number of slices.
nestedSliceBoundsFromList ::
  TensorShape ->
  [(TensorIndices, SliceBounds expr)] ->
  NestedSliceBounds expr
nestedSliceBoundsFromList shape entries = do
  -- Bounds with no remaining indices apply to this slice, the rest apply
  -- to the child slice given by their first index.
  let boundsOnThisSlice = [bounds | ([], bounds) <- entries]
  let boundsOnChildren = [(i, (is, bounds)) | (i : is, bounds) <- entries]

  let childBounds = case (shape, boundsOnChildren) of
        (_, []) -> Nothing
        (d : ds, _) -> do
          -- Group the bounds by child index. `fromListWith (<>)` prepends each new
          -- bound to the group, hence the `reverse` to restore the original order.
          let boundsByChild = reverse <$> Map.fromListWith (<>) [(i, [entry]) | (i, entry) <- boundsOnChildren]
          let mkChildBounds i = maybe emptyBounds (nestedSliceBoundsFromList ds) (Map.lookup i boundsByChild)
          Just $ map mkChildBounds [0 .. d - 1]
        ([], _) -> developerError "Malformed shape and indices"

  NestedSliceBounds
    { sliceBounds = foldr andBounds emptyBounds boundsOnThisSlice,
      childSliceBounds = childBounds
    }

instance IsBounds NestedSliceBounds expr where
  emptyBounds =
    NestedSliceBounds
      { sliceBounds = emptyBounds,
        childSliceBounds = Nothing
      }

  andBounds bounds1 bounds2 =
    NestedSliceBounds
      { sliceBounds = andBounds (sliceBounds bounds1) (sliceBounds bounds2),
        childSliceBounds = unionMaybeWith (zipWith andBounds) (childSliceBounds bounds1) (childSliceBounds bounds2)
      }

-------------------------------------------------------------------------------
-- Tensor bounds

-- | This is a data structure for representing partial bounds on a tensor
-- variable and is capable of storing bounds over each nested row in the
-- tensor individually.
newtype TensorBounds expr = TensorBounds
  { tensorSliceBounds :: NestedSliceBounds expr
  }
  deriving stock (Eq, Ord)

instance IsBounds TensorBounds expr where
  emptyBounds =
    TensorBounds
      { tensorSliceBounds = emptyBounds
      }

  andBounds (TensorBounds bounds1) (TensorBounds bounds2) =
    TensorBounds
      { tensorSliceBounds = andBounds bounds1 bounds2
      }

--------------------------------------------------------------------------------
-- Instantiating bounds

data VariableInfo = VariableInfo
  { parentVariable :: TensorVariable,
    parentShape :: KnownPrefixOfTensorShape,
    indices :: TensorIndices
  }

type MaybeBounds expr =
  Maybe
    ( These
        (LowerBound expr)
        (UpperBound expr)
    )

maybeBoundsToSliceBounds :: These (LowerBound expr) (UpperBound expr) -> SliceBounds expr
maybeBoundsToSliceBounds = \case
  This lower -> SliceBounds [lower] mempty
  That upper -> SliceBounds mempty [upper]
  These lower upper -> SliceBounds [lower] [upper]

tryConvertAssertionToBound ::
  (ConstantLike constant m) =>
  SliceVariable ->
  Assertion (LinearExpr SliceVariable constant) ->
  m (MaybeBounds (LinearExpr SliceVariable constant))
tryConvertAssertionToBound targetVariable (NormalisedRelation rel expr)
  | targetVariable `Set.notMember` variablesOf expr = return Nothing
  | otherwise = do
      (coeff, valueExpr) <- rearrangeExprToSolveFor targetVariable expr
      let strictness = case rel of
            OEq -> These NonStrict NonStrict
            OLt
              | coeff < 0 -> This Strict
              | otherwise -> That Strict
            OLe
              | coeff < 0 -> This NonStrict
              | otherwise -> That NonStrict

      return $ Just $ bimap (`LowerBound` valueExpr) (`UpperBound` valueExpr) strictness

tryToConvertToTensorBounds ::
  (Monad m, MonadReadableNameContext m, ConstantLike constant m, Show constant) =>
  (SliceVariable -> m (Maybe VariableInfo)) ->
  Assertion (LinearExpr SliceVariable constant) ->
  m (Maybe (TensorVariable, TensorBounds constant))
tryToConvertToTensorBounds lookupTensorVar (NormalisedRelation rel expr) = do
  case Map.toList (coefficients expr) of
    [(var, coef)] -> do
      maybeVar <- lookupTensorVar var
      case maybeVar of
        Just VariableInfo {..} -> do
          scaledBound <- scaleConstant (-(1 / coef)) (constantValue expr)
          bounds <- convertToTensorBounds parentShape indices rel coef scaledBound
          return $ Just (parentVariable, bounds)
        _ -> return Nothing
    _ -> return Nothing

convertToTensorBounds ::
  (ConstantLike constant m, Show constant) =>
  KnownPrefixOfTensorShape ->
  TensorIndices ->
  Relation ->
  Coefficient ->
  constant ->
  m (TensorBounds constant)
convertToTensorBounds shape indices rel coeff bound = do
  let sliceBounds = convertToSliceBounds rel coeff bound
  let tensorSliceBounds = singleNestedSliceBound sliceBounds shape indices
  return $ TensorBounds tensorSliceBounds

-- | Converts `c*x + b REL 0` where `bound = (-1/c)*b` into the bounds `x REL bound`.
convertToSliceBounds ::
  Relation ->
  Coefficient ->
  constant ->
  SliceBounds constant
convertToSliceBounds rel coeff bound = do
  let strictness = case rel of
        OEq -> These NonStrict NonStrict
        OLt
          | coeff < 0 -> This Strict
          | otherwise -> That Strict
        OLe
          | coeff < 0 -> This NonStrict
          | otherwise -> That NonStrict

  let maybeBounds = bimap (`LowerBound` bound) (`UpperBound` bound) strictness
  maybeBoundsToSliceBounds maybeBounds

mapSliceBounds :: (a -> b) -> SliceBounds a -> SliceBounds b
mapSliceBounds f (SliceBounds lower upper) =
  SliceBounds (fmap (fmap f) lower) (fmap (fmap f) upper)

--------------------------------------------------------------------------------
-- Domains

data Domain expr = Domain
  { lowerBound :: LowerBound expr,
    upperBound :: UpperBound expr
  }
  deriving stock (Show, Functor)

isSatisfiable :: Domain RatTensor -> Bool
isSatisfiable (Domain LowerBound {..} UpperBound {..}) = do
  let relation = combineInequalityRelations lowerBoundRel upperBoundRel
  anyTensor id $ zipWithTensor (inequalityRelationToOp relation) lowerBoundValue upperBoundValue

isEquality :: Domain RatTensor -> Bool
isEquality (Domain LowerBound {..} UpperBound {..}) = do
  let relation = combineInequalityRelations lowerBoundRel upperBoundRel
  case relation of
    Strict -> False
    NonStrict -> allTensor id $ zipWithTensor (==) lowerBoundValue upperBoundValue
