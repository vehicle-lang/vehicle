module Vehicle.Data.Bound where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Vehicle.Compile.Prelude (Doc, MonadLogger)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Level

--------------------------------------------------------------------------------
-- IsBound

type BoundCombinator bound = forall m. (MonadLogger m) => bound -> bound -> m bound

newtype MinOperation bound = MinOperation (BoundCombinator bound)

newtype MaxOperation bound = MaxOperation (BoundCombinator bound)

--------------------------------------------------------------------------------
-- BoundedValue

-- | A value paired with some bounds.
-- Frequently used with `printFriendly` and `printVerbose`
data BoundedValue var bounds = BoundedValue var bounds

--------------------------------------------------------------------------------
-- Lower bounds

-- | A single lower bound on some value
data LowerBound expr = LowerBound InequalityRelation expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (LowerBound expr)

instance (ToJSON expr) => ToJSON (LowerBound expr)

instance (FromJSON expr) => FromJSON (LowerBound expr)

--------------------------------------------------------------------------------
-- UpperBound

-- | A single upper bound on some value
data UpperBound expr = UpperBound InequalityRelation expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (UpperBound expr)

instance (ToJSON expr) => ToJSON (UpperBound expr)

instance (FromJSON expr) => FromJSON (UpperBound expr)

--------------------------------------------------------------------------------
-- Bounds

-- | A collection of lower and upper bounds on some value.
data Bounds expr = Bounds
  { lowerBounds :: [LowerBound expr],
    upperBounds :: [UpperBound expr]
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData expr) => NFData (Bounds expr)

instance (ToJSON expr) => ToJSON (Bounds expr)

instance (FromJSON expr) => FromJSON (Bounds expr)

--------------------------------------------------------------------------------
-- A collection of one-sided bounds over

-- | A collection of one-sided bounds on slices of a tensor
data SliceBounds bound = SliceBounds
  { _sliceBound :: Maybe bound,
    _childConstraints :: Maybe [SliceBounds bound]
  }
  deriving (Show)

emptySliceBounds :: SliceBounds bound
emptySliceBounds = SliceBounds Nothing Nothing

isEmptySliceBounds :: SliceBounds bound -> Bool
isEmptySliceBounds = \case
  SliceBounds Nothing Nothing -> True
  _ -> False

prettySliceBounds ::
  forall m bound a.
  (MonadTraverseTensor m) =>
  (bound -> Doc a) ->
  SliceBounds bound ->
  m [Doc a]
prettySliceBounds prettyBound = go
  where
    go :: SliceBounds bound -> m [Doc a]
    go (SliceBounds maybeBound maybeChildBounds) = do
      let prettySingleBound bound = [prettyBound bound]
      let prettyChildBounds = traverseTensorRows go

      let boundDoc = maybe [] prettySingleBound maybeBound
      childBoundDocs <- maybe (return []) prettyChildBounds maybeChildBounds
      return $ boundDoc <> concat childBoundDocs

-------------------------------------------------------------------------------
-- Tensor bounds

-- | This is a data structure for representing partial bounds on a tensor
-- variable and is capable of storing bounds over each nested row in the
-- tensor individually.
data TensorBounds bound = TensorBounds
  { boundedVar :: TensorVariable,
    partialShape :: PartiallyKnownTensorShape,
    tensorLowerBounds :: SliceBounds (LowerBound bound),
    tensorUpperBounds :: SliceBounds (UpperBound bound)
  }

isEmptyTensorBounds :: TensorBounds bound -> Bool
isEmptyTensorBounds TensorBounds {..} =
  isEmptySliceBounds tensorLowerBounds && isEmptySliceBounds tensorUpperBounds

-------------------------------------------------------------------------------
-- Concrete instantiations

type LinearBounds = Bounds LinearExpression
