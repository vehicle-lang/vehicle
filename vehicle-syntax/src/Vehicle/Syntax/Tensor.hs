module Vehicle.Syntax.Tensor
  ( TensorShape,
    TensorIndices,
    showTensorIndices,
    flattenIndices,
    HasShape (..),
    isZeroDimensional,
    Tensor (ConstantTensor),
    BoolTensor,
    RatTensor,
    IndexTensor,
    NatTensor,
    pattern ZeroDimTensor,
    allTensor,
    anyTensor,
    zipWithTensor,
    prettyTensor,
    stack,
    unstack,
    at,
    foldTensor,
    foldMapTensor,
    mapTensor,
    traverseTensor,
    toList,
    toVector,
    fromVector,
    isTensorOfAll,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split (chunksOf)
import Data.Serialize (Serialize)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Instances ()
import Data.Vector.Internal.Check (HasCallStack)
import Data.Vector.Serialize ()
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..), concatWith, surround, (<+>))
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Indices

type TensorShape = [Int]

type TensorIndices = [Int]

showTensorIndices :: TensorIndices -> String
showTensorIndices xs = concatMap (\v -> "!" <> show v) (reverse xs)

flattenIndices :: TensorShape -> TensorIndices -> Int
flattenIndices shape indices =
  sum $ zipWith (*) indices (NonEmpty.tail (NonEmpty.scanr (*) 1 shape))

class HasShape a where
  shapeOf :: a -> TensorShape

isZeroDimensional :: (HasShape a) => a -> Bool
isZeroDimensional v = null (shapeOf v)

--------------------------------------------------------------------------------
-- Tensor constants

data Tensor a
  = DenseTensor TensorShape (Vector a)
  | ConstantTensor TensorShape a
  deriving (Show, Eq, Ord, Generic)

instance (NFData a) => NFData (Tensor a)

instance (Serialize a) => Serialize (Tensor a)

instance (Hashable a) => Hashable (Tensor a)

instance (ToJSON a) => ToJSON (Tensor a)

instance (FromJSON a) => FromJSON (Tensor a)

toVector :: Tensor a -> Vector a
toVector = \case
  ConstantTensor shape value -> Vector.replicate (product shape) value
  DenseTensor _ values -> values

fromVector :: (Eq a) => TensorShape -> Vector a -> Tensor a
fromVector shape values
  | Vector.null values = DenseTensor shape values
  | Vector.all (== Vector.head values) values = ConstantTensor shape (Vector.head values)
  | otherwise = DenseTensor shape values

fromVectorSlice :: (Eq a) => Int -> TensorShape -> Vector a -> Int -> Tensor a
fromVectorSlice stride sliceShape values strideIndex =
  fromVector sliceShape $ Vector.slice (strideIndex * stride) stride values

mapTensor :: (Eq b) => (a -> b) -> Tensor a -> Tensor b
mapTensor f = \case
  DenseTensor shape values -> fromVector shape $ fmap f values
  ConstantTensor shape value -> ConstantTensor shape (f value)

instance Foldable Tensor where
  foldr f e t = foldr f e (toVector t)

traverseTensor :: (Applicative m, Eq b) => (a -> m b) -> Tensor a -> m (Tensor b)
traverseTensor f = \case
  DenseTensor shape values -> fromVector shape <$> traverse f values
  ConstantTensor shape value -> ConstantTensor shape <$> f value

instance HasShape (Tensor a) where
  shapeOf = \case
    ConstantTensor shape _ -> shape
    DenseTensor shape _ -> shape

toList :: Tensor a -> [a]
toList = Vector.toList . toVector

innerMap :: (a -> b) -> (Vector a -> b) -> Tensor a -> b
innerMap f fs = \case
  ConstantTensor _ value -> f value
  DenseTensor _ values -> fs values

allTensor :: (a -> Bool) -> Tensor a -> Bool
allTensor f = innerMap f (Vector.all f)

anyTensor :: (a -> Bool) -> Tensor a -> Bool
anyTensor f = innerMap f (Vector.any f)

zipWithTensor :: (Eq c) => (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
zipWithTensor f xs ys = case (xs, ys) of
  (ConstantTensor shape u, ConstantTensor _ v) -> ConstantTensor shape (f u v)
  (DenseTensor shape us, ConstantTensor _ v) -> fromVector shape $ fmap (`f` v) us
  (ConstantTensor shape u, DenseTensor _ vs) -> fromVector shape $ fmap (f u) vs
  (DenseTensor shape us, DenseTensor _ vs) -> fromVector shape $ Vector.zipWith f us vs

foldTensor :: (a -> a -> a) -> Tensor a -> Tensor a -> Tensor a
foldTensor f e t = case toList t of
  [] -> e
  (x : xs) -> ZeroDimTensor $ foldr f x xs

at :: (HasCallStack, Eq a) => Tensor a -> Int -> Tensor a
at xs i = case shapeOf xs of
  [] -> developerError "Indexing into a zero-dimensional tensor"
  d : ds
    | i >= d ->
        developerError $
          "Index" <+> pretty i <+> "out of bounds in tensor of shape" <+> pretty (d : ds)
    | otherwise -> case xs of
        ConstantTensor _ value -> ConstantTensor ds value
        DenseTensor _ values -> do
          let stride = product ds
          fromVectorSlice stride ds values i

stack :: (Eq a) => [Int] -> [Tensor a] -> Tensor a
stack ds ts = do
  let dims = length ts : ds
  case allConstant ts of
    Just v -> ConstantTensor dims v
    Nothing -> fromVector dims $ Vector.concat $ fmap toVector ts
  where
    allConstant :: (Eq a) => [Tensor a] -> Maybe a
    allConstant [] = Nothing
    allConstant (x : xs) = case x of
      ConstantTensor _ v
        | all (== x) xs -> Just v
        | otherwise -> Nothing
      _ -> Nothing

unstack :: (HasCallStack, Eq a) => Tensor a -> [Tensor a]
unstack xs = case shapeOf xs of
  [] -> []
  d : ds -> case xs of
    ConstantTensor _ value -> replicate d (ConstantTensor ds value)
    DenseTensor _ values -> do
      let stride = product ds
      fmap (fromVectorSlice stride ds values) [0 .. d - 1]

foldMapTensor :: forall a b. (a -> b) -> (TensorShape -> [b] -> b) -> Tensor a -> b
foldMapTensor mkValue mkVec t =
  foldMapTensorLike mkValue mkVec (shapeOf t) (toList t)

foldMapTensorLike :: (a -> b) -> (TensorShape -> [b] -> b) -> TensorShape -> [a] -> b
foldMapTensorLike mkValue _mkVec [] [x] = mkValue x
foldMapTensorLike _mkValue _mkVec [] _xs = developerError "Mis-sized tensor. Expected a single element."
foldMapTensorLike mkValue mkVec (_ : ds) xs = do
  let inputVarIndicesChunks = chunksOf (product ds) xs
  let elems = fmap (foldMapTensorLike mkValue mkVec ds) inputVarIndicesChunks
  mkVec ds elems

prettyTensor :: (a -> Doc b) -> Tensor a -> Doc b
prettyTensor prettyElement = do
  let prettyRow _dims bs = "[" <+> concatWith (surround ", ") bs <+> "]"
  foldMapTensor prettyElement prettyRow

isTensorOfAll :: (Eq a) => Tensor a -> a -> Bool
isTensorOfAll t x = case t of
  DenseTensor {} -> False
  ConstantTensor _ v -> v == x

instance (Pretty a) => Pretty (Tensor a) where
  pretty = prettyTensor pretty

type BoolTensor = Tensor Bool

type NatTensor = Tensor Int

type IndexTensor = Tensor Int

type RatTensor = Tensor Rational

-- | Represents a plain value, with zero dimensions
pattern ZeroDimTensor :: a -> Tensor a
pattern ZeroDimTensor v <- ConstantTensor [] v
  where
    ZeroDimTensor v = ConstantTensor [] v
