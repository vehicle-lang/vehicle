module Vehicle.Data.Tensor where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Vehicle.Data.LinearExpr
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor constants

data Tensor a = Tensor
  { tensorDims :: TensorDimensions,
    tensorValues :: Vector a
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)

instance (ToJSON a) => ToJSON (Tensor a)

instance (FromJSON a) => FromJSON (Tensor a)

instance (IsConstant a) => IsConstant (Tensor a) where
  isZero = Vector.all isZero . tensorValues
  scaleConstant v = fmap (scaleConstant v)
  addConstants a b = zipWithTensor (addConstants a b)

zipWithTensor :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
zipWithTensor f t1 t2 =
  Tensor
    { tensorDims = tensorDims t1,
      tensorValues = Vector.zipWith f (tensorValues t1) (tensorValues t2)
    }

stack :: NonEmpty (Tensor a) -> Tensor a
stack tensors@(t :| ts) =
  Tensor (length tensors : tensorDims t) (Vector.concat (fmap tensorValues (t : ts)))

foldMapTensor :: forall a b. (a -> b) -> ([b] -> b) -> Tensor a -> b
foldMapTensor mkValue mkVec (Tensor dims value) = go dims (Vector.toList value)
  where
    go :: TensorDimensions -> [a] -> b
    go [] [x] = mkValue x
    go [] _xs = developerError "Mis-sized tensor. Expected a single element."
    go (_d : ds) xs = do
      let inputVarIndicesChunks = chunksOf (product ds) xs
      let elems = fmap (go ds) inputVarIndicesChunks
      mkVec elems

instance (Pretty a) => Pretty (Tensor a) where
  pretty = foldMapTensor pretty prettyFlatList

type RationalTensor = Tensor Rational

zeroTensor :: TensorDimensions -> RationalTensor
zeroTensor dims = Tensor dims (Vector.replicate (product dims) 0)
