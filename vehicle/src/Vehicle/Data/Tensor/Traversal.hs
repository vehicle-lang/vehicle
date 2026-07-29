module Vehicle.Data.Tensor.Traversal where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Vehicle.Data.Tensor (TensorIndices, TensorShape)

-- | We may not be able to calculate the exact dimensions a tensor, but this
-- value represents the prefix that of the shape that is known, e.g.
-- [1,2,n] would have a prefix of [1,2]
type KnownPrefixOfTensorShape = TensorShape

--------------------------------------------------------------------------------
-- Tensor traversal

type MonadTraverseTensor m = MonadReader TensorIndices m

traverseTensorRows :: (MonadTraverseTensor m) => (a -> m b) -> [a] -> m [b]
traverseTensorRows f rows = do
  let fLocal (i, v) = local (i :) (f v)
  traverse fLocal (zip [0 ..] rows)

currentIndices :: (MonadTraverseTensor m) => m TensorIndices
currentIndices = asks reverse

runTraverseTensorT ::
  (Monad m) =>
  ReaderT TensorIndices m a ->
  m a
runTraverseTensorT action = runReaderT action mempty
