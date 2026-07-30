module Vehicle.Data.Tensor
  ( TensorShape,
    TensorDimension,
    TensorIndex,
    TensorIndices,
    allIndicesForShape,
    allMultiIndices,
    showTensorIndices,
    flattenIndices,
    HasShape (..),
    isZeroDimensional,
    Tensor (ConstantTensor),
    BoolTensor,
    IndexTensor,
    NatTensor,
    RatTensor,
    ExtendedRatTensor,
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
    foldMapTensorLike,
    mapTensor,
    traverseTensor,
    toList,
    toVector,
    fromVector,
    isTensorOfAll,
    compareTensor,
    extendTensor,
    transposeTensor,
    toFiniteRatTensor,
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
import Vehicle.Data.Real
import Vehicle.Prelude.Error (developerError)

--------------------------------------------------------------------------------
-- Indices

type TensorDimension = Int

type TensorShape = [TensorDimension]

type TensorIndex = Int

type TensorIndices = [TensorIndex]

showTensorIndices :: TensorIndices -> String
showTensorIndices xs = concatMap (\v -> "!" <> show v) (reverse xs)

flattenIndices :: TensorShape -> TensorIndices -> Int
flattenIndices shape indices =
  sum $ zipWith (*) indices (NonEmpty.tail (NonEmpty.scanr (*) 1 shape))

-- | This includes indices for the slices
allIndicesForShape :: TensorShape -> [TensorIndices]
allIndicesForShape shape = go shape mempty
  where
    go :: TensorShape -> TensorIndices -> [TensorIndices]
    go dims is =
      is : case dims of
        [] -> []
        d : ds -> concatMap (\i -> go ds (i : is)) ([0 .. d - 1] :: [Int])

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

foldTensor :: (a -> a -> a) -> a -> Tensor a -> Tensor a
foldTensor f e t = ZeroDimTensor $ case toList t of
  [] -> e
  (x : xs) -> foldr f x xs

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

stack :: (Eq a) => TensorShape -> [Tensor a] -> Tensor a
stack elementDims ts = do
  let dims = length ts : elementDims
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

extendTensor :: Int -> Tensor a -> Tensor a
extendTensor dim = \case
  ConstantTensor shape value -> ConstantTensor (dim : shape) value
  DenseTensor shape values -> DenseTensor (dim : shape) (Vector.concat (replicate dim values))

allMultiIndices :: TensorShape -> [TensorIndices]
allMultiIndices = \case
  [] -> [[]]
  d : ds -> [i : rest | i <- [0 .. d - 1], rest <- allMultiIndices ds]

transposeTensor :: (Eq a) => Tensor a -> Tensor a
transposeTensor = \case
  ConstantTensor shape value -> ConstantTensor (reverse shape) value
  DenseTensor shape values -> do
    let revShape = reverse shape
    let pickAt revIs = values Vector.! flattenIndices shape (reverse revIs)
    fromVector revShape $ Vector.fromList [pickAt revIs | revIs <- allMultiIndices revShape]

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

compareTensor :: (a -> b -> Bool) -> Tensor a -> Tensor b -> Bool
compareTensor f t1 t2 = allTensor id $ zipWithTensor f t1 t2

prettyTensor :: (a -> Doc b) -> Tensor a -> Doc b
prettyTensor prettyElement = \case
  ConstantTensor [] value -> prettyElement value
  ConstantTensor shape value -> "const" <+> prettyElement value <+> pretty shape
  denseTensor -> do
    let prettyRow _dims bs = "[" <+> concatWith (surround ", ") bs <+> "]"
    foldMapTensor prettyElement prettyRow denseTensor

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

type ExtendedRatTensor = Tensor ExtendedRational

toFiniteRatTensor :: ExtendedRatTensor -> Maybe RatTensor
toFiniteRatTensor = traverseTensor $ \case
  Finite v -> Just v
  _ -> Nothing

-- | Represents a plain value, with zero dimensions
pattern ZeroDimTensor :: a -> Tensor a
pattern ZeroDimTensor v <- ConstantTensor [] v
  where
    ZeroDimTensor v = ConstantTensor [] v
