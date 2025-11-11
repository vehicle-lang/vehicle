module Vehicle.Data.Tensor.Traversal where

import Control.Monad.Reader (MonadReader (..), Reader, ReaderT (..), asks, runReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (fromMaybe)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, TensorShape)

--------------------------------------------------------------------------------
-- PartiallyKnownTensorShape

-- | Represents the dimensions of a tensor where we know the leading dimensions
-- but the trailing dimensions are still unknown (i.e. depends on external
-- resources, see MNIST robustness specification for an example)
data PartiallyKnownTensorShape = PartiallyKnownTensorShape
  { knownPrefix :: TensorShape,
    unknownSuffix :: Value Builtin
  }

toPartialShape :: TensorShape -> Maybe (Value Builtin) -> PartiallyKnownTensorShape
toPartialShape knownDims maybeUnknownDims =
  PartiallyKnownTensorShape
    { knownPrefix = knownDims,
      unknownSuffix = fromMaybe IDimNil maybeUnknownDims
    }

emptyPartialShape :: PartiallyKnownTensorShape
emptyPartialShape = toPartialShape [] Nothing

extractPartialShape :: Value Builtin -> PartiallyKnownTensorShape
extractPartialShape v = uncurry PartiallyKnownTensorShape $ go v
  where
    go :: Value Builtin -> (TensorShape, Value Builtin)
    go = \case
      IDimCons (INatLiteral d) ds -> first (d :) $ go ds
      value -> ([], value)

calculateCurrentDimensions :: PartiallyKnownTensorShape -> TensorIndices -> Value Builtin
calculateCurrentDimensions PartiallyKnownTensorShape {..} reverseIndices = do
  let remainingShapePrefix = drop (length reverseIndices) knownPrefix
  foldr (\i -> IDimCons (INatLiteral i)) unknownSuffix remainingShapePrefix

--------------------------------------------------------------------------------
-- Tensor traversal

type MonadTraverseTensor m =
  ( MonadReader (PartiallyKnownTensorShape, TensorIndices) m
  )

traverseTensorRows :: (MonadTraverseTensor m) => (a -> m b) -> [a] -> m [b]
traverseTensorRows f rows = do
  let fLocal (i, v) = local (second (i :)) (f v)
  traverse fLocal (zip [0 ..] rows)

currentDimensions :: (MonadTraverseTensor m) => m (Value Builtin)
currentDimensions = asks (uncurry calculateCurrentDimensions)

childDimensions :: (MonadTraverseTensor m) => m (Value Builtin)
childDimensions = local (second (0 :)) currentDimensions

currentIndices :: (MonadTraverseTensor m) => m TensorIndices
currentIndices = asks (reverse . snd)

runTraverseTensorT ::
  (Monad m) =>
  PartiallyKnownTensorShape ->
  ReaderT (PartiallyKnownTensorShape, TensorIndices) m a ->
  m a
runTraverseTensorT shape action = runReaderT action (shape, mempty)

runTraverseTensor :: PartiallyKnownTensorShape -> Reader (PartiallyKnownTensorShape, TensorIndices) a -> a
runTraverseTensor shape action = runReader action (shape, mempty)
