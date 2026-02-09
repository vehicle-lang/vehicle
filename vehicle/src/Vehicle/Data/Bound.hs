module Vehicle.Data.Bound where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.These (These (..))
import GHC.Generics
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.Tensor (HasShape (..), TensorIndices, TensorShape)
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Level

--------------------------------------------------------------------------------
-- BoundedValue

-- | A value paired with some bounds.
-- Frequently used with `printFriendly` and `printVerbose`
data BoundedValue value bounds = BoundedValue
  { boundedValue :: value,
    valueBounds :: bounds
  }
  deriving (Show)

instance Functor (BoundedValue value) where
  fmap f (BoundedValue var bounds) = BoundedValue var (f bounds)

instance Bifunctor BoundedValue where
  bimap f g (BoundedValue var bounds) = BoundedValue (f var) (g bounds)

type UnboundedIndices = These (NonEmpty TensorIndices) (NonEmpty TensorIndices)

wholeTensorUnbounded :: UnboundedIndices
wholeTensorUnbounded = These [[]] [[]]

--------------------------------------------------------------------------------
-- IsBound

class (ConstantLike constant) => IsBound bound constant where
  andBound :: bound constant -> bound constant -> bound constant
  boundToValue :: bound constant -> (InequalityRelation, constant)
  valueToBound :: (InequalityRelation, constant) -> bound constant

stackBounds ::
  (IsBound bound constant) =>
  [bound constant] ->
  bound constant
stackBounds bounds = do
  let (rels, values) = unzipWith boundToValue bounds
  let stackedValue = stackConstants values
  let stackedRel = foldr combineInequalityRelations NonStrict rels
  valueToBound (stackedRel, stackedValue)

unstackBounds ::
  (IsBound bound constant) =>
  bound constant ->
  [bound constant]
unstackBounds bound = do
  let (rel, constant) = boundToValue bound
  let stackedValues = unstackConstants constant
  fmap (\val -> valueToBound (rel, val)) stackedValues

andBoundList ::
  (IsBound bound constant) =>
  [bound constant] ->
  Maybe (bound constant)
andBoundList = \case
  [] -> Nothing
  x : xs -> Just $ foldr andBound x xs

--------------------------------------------------------------------------------
-- Lower bounds

-- | A single lower bound on some value
data LowerBound expr = LowerBound
  { lowerBoundRel :: InequalityRelation,
    lowerBoundValue :: expr
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (LowerBound expr)

instance (ToJSON expr) => ToJSON (LowerBound expr)

instance (FromJSON expr) => FromJSON (LowerBound expr)

instance (HasShape expr) => HasShape (LowerBound expr) where
  shapeOf = shapeOf . lowerBoundValue

instance (ConstantLike expr) => IsBound LowerBound expr where
  andBound (LowerBound r1 v1) (LowerBound r2 v2) = do
    let newValue = maxConstants v1 v2
    let newRelation = combineInequalityRelations r1 r2
    LowerBound newRelation newValue

  boundToValue (LowerBound rel value) = (rel, value)

  valueToBound = uncurry LowerBound

--------------------------------------------------------------------------------
-- UpperBound

-- | A single upper bound on some value
data UpperBound expr = UpperBound
  { upperBoundRel :: InequalityRelation,
    upperBoundValue :: expr
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (UpperBound expr)

instance (ToJSON expr) => ToJSON (UpperBound expr)

instance (FromJSON expr) => FromJSON (UpperBound expr)

instance (ConstantLike expr) => IsBound UpperBound expr where
  andBound (UpperBound r1 v1) (UpperBound r2 v2) = do
    let newValue = minConstants v1 v2
    let newRelation = combineInequalityRelations r1 r2
    UpperBound newRelation newValue

  boundToValue (UpperBound rel value) = (rel, value)

  valueToBound = uncurry UpperBound

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
  deriving (Show, Eq, Ord, Generic)

instance (NFData expr) => NFData (SliceBounds expr)

instance (ToJSON expr) => ToJSON (SliceBounds expr)

instance (FromJSON expr) => FromJSON (SliceBounds expr)

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
  deriving (Show, Eq, Ord)

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
  deriving (Eq, Ord)

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
    parentShape :: PartiallyKnownTensorShape,
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
  (ConstantLike constant) =>
  SliceVariable ->
  Assertion (LinearExpr SliceVariable constant) ->
  MaybeBounds (LinearExpr SliceVariable constant)
tryConvertAssertionToBound targetVariable (NormalisedRelation rel expr)
  | targetVariable `Set.notMember` variablesOf expr = Nothing
  | otherwise = do
      let (coeff, valueExpr) = rearrangeExprToSolveFor targetVariable expr
      let strictness = case rel of
            OEq -> These NonStrict NonStrict
            OLt
              | coeff < 0 -> This Strict
              | otherwise -> That Strict
            OLe
              | coeff < 0 -> This NonStrict
              | otherwise -> That NonStrict

      Just $ bimap (`LowerBound` valueExpr) (`UpperBound` valueExpr) strictness

tryToConvertToTensorBounds ::
  (Monad m, MonadReadableNameContext m, ConstantLike constant, Show constant) =>
  (SliceVariable -> m (Maybe VariableInfo)) ->
  Assertion (LinearExpr SliceVariable constant) ->
  m (Maybe (TensorVariable, TensorBounds constant))
tryToConvertToTensorBounds lookupTensorVar (NormalisedRelation rel expr) = do
  case Map.toList (coefficients expr) of
    [(var, coef)] -> do
      maybeVar <- lookupTensorVar var
      return $ case maybeVar of
        Just VariableInfo {..} -> do
          let scaledBound = scaleConstant (-(1 / coef)) (constantValue expr)
          let bounds = convertToTensorBounds parentShape indices rel coef scaledBound
          Just (parentVariable, bounds)
        _ -> Nothing
    _ -> return Nothing

convertToTensorBounds ::
  (ConstantLike constant, Show constant) =>
  PartiallyKnownTensorShape ->
  TensorIndices ->
  Relation ->
  Coefficient ->
  constant ->
  TensorBounds constant
convertToTensorBounds shape indices rel coeff bound = do
  -- c*x + b REL 0   -> x REL (-1/c)*b
  let strictness = case rel of
        OEq -> These NonStrict NonStrict
        OLt
          | coeff < 0 -> This Strict
          | otherwise -> That Strict
        OLe
          | coeff < 0 -> This NonStrict
          | otherwise -> That NonStrict

  let maybeBounds = bimap (`LowerBound` bound) (`UpperBound` bound) strictness
  let sliceBounds = maybeBoundsToSliceBounds maybeBounds
  let tensorSliceBounds = singleNestedSliceBound sliceBounds (knownPrefix shape) indices
  TensorBounds tensorSliceBounds

--------------------------------------------------------------------------------
-- Domains

data Domain expr = Domain
  { lowerBound :: LowerBound expr,
    upperBound :: UpperBound expr
  }
  deriving (Show, Functor)

isSatisfiable :: Domain Rational -> Bool
isSatisfiable (Domain LowerBound {..} UpperBound {..}) = do
  let relation = combineInequalityRelations lowerBoundRel upperBoundRel
  inequalityRelationToOp relation lowerBoundValue upperBoundValue

isEquality :: Domain Rational -> Bool
isEquality (Domain LowerBound {..} UpperBound {..}) = do
  let relation = combineInequalityRelations lowerBoundRel upperBoundRel
  case relation of
    Strict -> False
    NonStrict -> (==) lowerBoundValue upperBoundValue
