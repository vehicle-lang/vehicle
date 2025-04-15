{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
module Vehicle.Syntax.Tensor where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.List.Split (chunksOf)
import Data.Serialize (Serialize)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Instances ()
import Data.Vector.Serialize ()
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), concatWith, surround, (<+>))
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Indices

type TensorShape = [Int]

type TensorIndices = [Int]

showTensorIndices :: TensorIndices -> String
showTensorIndices xs = concatMap (\v -> "!" <> show v) (reverse xs)

class HasShape a where
  shapeOf :: a -> TensorShape

isZeroDimensional :: (HasShape a) => a -> Bool
isZeroDimensional v = null (shapeOf v)

--------------------------------------------------------------------------------
-- Tensor value

data TensorValue a = Constant a | Values (Vector a)
  deriving (Show, Eq, Ord, Generic)

instance Functor TensorValue where
  fmap f = \case
    Constant v -> Constant $ f v
    Values vs -> Values $ fmap f vs

tensorValuesToVector :: TensorShape -> TensorValue a -> Vector a
tensorValuesToVector shape = \case
  Constant v -> Vector.replicate (product shape) v
  Values vs -> vs

instance (NFData a) => NFData (TensorValue a)

instance (Serialize a) => Serialize (TensorValue a)

instance (Hashable a) => Hashable (TensorValue a)

instance (ToJSON a) => ToJSON (TensorValue a)

instance (FromJSON a) => FromJSON (TensorValue a)

allValues :: (a -> Bool) -> TensorValue a -> Bool
allValues f = \case
  Constant v -> f v
  Values vs -> Vector.all f vs

anyValues :: (a -> Bool) -> TensorValue a -> Bool
anyValues f = \case
  Constant v -> f v
  Values vs -> Vector.any f vs

atValues :: [Int] -> Int -> TensorValue a -> TensorValue a
atValues elemDims d = \case
  Constant v -> Constant v
  Values vs -> do
    let stride = product elemDims
    Values $ Vector.slice (d * stride) stride vs

zipWithValues :: (a -> b -> c) -> TensorValue a -> TensorValue b -> TensorValue c
zipWithValues f c d = case (c, d) of
  (Constant v, _) -> fmap (f v) d
  (_, Constant u) -> fmap (flip f u) c
  (Values vs, Values us) -> Values $ Vector.zipWith f vs us

--------------------------------------------------------------------------------
-- Tensor constants

data Tensor a = Tensor
  { tensorShape :: TensorShape,
    tensorValue :: TensorValue a
  }
  deriving (Show, Eq, Ord, Generic, Functor)

instance (NFData a) => NFData (Tensor a)

instance (Serialize a) => Serialize (Tensor a)

instance (Hashable a) => Hashable (Tensor a)

instance (ToJSON a) => ToJSON (Tensor a)

instance (FromJSON a) => FromJSON (Tensor a)

instance Foldable Tensor where
  foldr f e t = foldr f e (tensorToVector t)

instance Traversable Tensor where
  traverse f (Tensor shape values) =
    Tensor shape <$> case values of
      Constant v -> Constant <$> f v
      Values vs -> Values <$> traverse f vs

tensorToVector :: Tensor a -> Vector a
tensorToVector (Tensor shape values) =
  tensorValuesToVector shape values

tensorToList :: Tensor a -> [a]
tensorToList = Vector.toList . tensorToVector

allTensor :: (a -> Bool) -> Tensor a -> Bool
allTensor f = allValues f . tensorValue

anyTensor :: (a -> Bool) -> Tensor a -> Bool
anyTensor f = anyValues f . tensorValue

zipWithTensor :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
zipWithTensor f (Tensor shape c) (Tensor _shape d) = Tensor shape (zipWithValues f c d)

foldTensor :: (a -> a -> a) -> Tensor a -> Tensor a -> Tensor a
foldTensor f e t = case tensorToList t of
  [] -> e
  (x : xs) -> ZeroDimTensor $ foldr f x xs

at :: Tensor a -> Int -> Tensor a
at (Tensor shape values) i = case shape of
  [] -> developerError "Indexing into a zero-dimensional tensor"
  d : ds
    | i < d -> Tensor ds (atValues ds i values)
    | otherwise -> developerError $ "Index" <+> pretty i <+> "out of bounds in tensor of shape" <+> pretty shape

stack :: (Eq a) => [Int] -> [Tensor a] -> Tensor a
stack ds ts = do
  let dims = length ts : ds
  let values = fmap tensorValue ts
  let elems = case allConstant values of
        Just v -> Constant v
        Nothing -> Values $ Vector.concat $ fmap tensorToVector ts
  Tensor dims elems
  where
    allConstant :: (Eq a) => [TensorValue a] -> Maybe a
    allConstant [] = Nothing
    allConstant (x : xs) = case x of
      Constant v
        | all (== x) xs -> Just v
        | otherwise -> Nothing
      _ -> Nothing

unstack :: Tensor a -> [Tensor a]
unstack (Tensor shape values) = case shape of
  [] -> []
  d : ds -> case values of
    Constant v -> replicate d (Tensor ds $ Constant v)
    Values vs -> do
      let s = product ds
      fmap (\i -> Tensor ds $ Values $ Vector.slice (i * s) ((i + 1) * s) vs) [0 .. d - 1]

foldMapTensor :: forall a b. (a -> b) -> (TensorShape -> [b] -> b) -> Tensor a -> b
foldMapTensor mkValue mkVec t =
  foldMapTensorLike mkValue mkVec (tensorShape t) (tensorToList t)

foldMapTensorLike :: (a -> b) -> (TensorShape -> [b] -> b) -> TensorShape -> [a] -> b
foldMapTensorLike mkValue _mkVec [] [x] = mkValue x
foldMapTensorLike _mkValue _mkVec [] _xs = developerError "Mis-sized tensor. Expected a single element."
foldMapTensorLike mkValue mkVec (_ : ds) xs = do
  let inputVarIndicesChunks = chunksOf (product ds) xs
  let elems = fmap (foldMapTensorLike mkValue mkVec ds) inputVarIndicesChunks
  mkVec ds elems

instance (Pretty a) => Pretty (Tensor a) where
  pretty = foldMapTensor pretty (\_dims bs -> "[" <+> concatWith (surround ", ") bs <+> "]")

type BoolTensor = Tensor Bool

type NatTensor = Tensor Int

type IndexTensor = Tensor Int

type RatTensor = Tensor Rational

zeroTensor :: TensorShape -> RatTensor
zeroTensor dims = Tensor dims (Constant 0)

singletonTensor :: a -> Tensor a
singletonTensor a = Tensor [1] (Constant a)

pattern ConstantTensor :: TensorShape -> a -> Tensor a
pattern ConstantTensor dims v = (Tensor dims (Constant v))

-- | Represents a plain value, with zero dimensions
pattern ZeroDimTensor :: a -> Tensor a
pattern ZeroDimTensor v <- ConstantTensor [] v
  where
    ZeroDimTensor v = ConstantTensor [] v
