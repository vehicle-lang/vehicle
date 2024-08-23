module Vehicle.Data.Tensor where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Char qualified as Char
import Data.List qualified as List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Vehicle.Prelude.Misc (IsConstant (..), jsonOptions)
import Vehicle.Prelude.Prettyprinter (Pretty (..), prettyFlatList, (<+>))
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Indices

type TensorShape = [Int]

type TensorIndices = [Int]

computeFlatIndex :: TensorShape -> TensorIndices -> Int
computeFlatIndex = go
  where
    go :: TensorShape -> TensorIndices -> Int
    go [] [] = 0
    go (d : ds) (i : is) | i < d = i * product ds + go ds is
    go ds is = developerError $ "Invalid flat tensor arguments" <+> pretty ds <+> pretty is

showTensorIndices :: TensorIndices -> String
showTensorIndices xs = concatMap (\v -> "!" <> show v) (reverse xs)

--------------------------------------------------------------------------------
-- Tensor constants

data Tensor a = Tensor
  { tensorShape :: TensorShape,
    tensorValue :: Vector a
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)

instance (NFData a) => NFData (Tensor a)

-- | Ensure names are normalised in JSON.
stripFieldNamePrefix :: String -> String
stripFieldNamePrefix name = maybe name (map Char.toLower) $ List.stripPrefix "tensor" name

instance (ToJSON a) => ToJSON (Tensor a) where
  toJSON =
    genericToJSON
      jsonOptions
        { fieldLabelModifier = stripFieldNamePrefix
        }

instance (FromJSON a) => FromJSON (Tensor a) where
  parseJSON =
    genericParseJSON
      jsonOptions
        { fieldLabelModifier = stripFieldNamePrefix
        }

instance (IsConstant a) => IsConstant (Tensor a) where
  isZero = Vector.all isZero . tensorValue
  scaleConstant v = fmap (scaleConstant v)
  addConstants a b = zipWithTensor (addConstants a b)

mapTensor :: (a -> b) -> Tensor a -> Tensor b
mapTensor = fmap

zipWithTensor :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
zipWithTensor f t1 t2 =
  Tensor
    { tensorShape = tensorShape t1,
      tensorValue = Vector.zipWith f (tensorValue t1) (tensorValue t2)
    }

foldTensor :: (Tensor a -> b -> b) -> b -> Tensor a -> b
foldTensor f e = \case
  Tensor [] _ -> e
  Tensor (_d : ds) xs -> do
    let inputChunks = chunksOf (product ds) (Vector.toList xs)
    let inputTensorChunks = fmap (Tensor ds . Vector.fromList) inputChunks
    foldr f e inputTensorChunks

stack :: NonEmpty (Tensor a) -> Tensor a
stack (t :| ts) = do
  let dims = 1 + length ts : tensorShape t
  let elems = Vector.concat (fmap tensorValue (t : ts))
  Tensor dims elems

foldMapTensor :: forall a b. (a -> b) -> (TensorShape -> [b] -> b) -> Tensor a -> b
foldMapTensor mkValue mkVec (Tensor dims value) =
  foldMapTensorLike mkValue mkVec dims (Vector.toList value)

foldMapTensorLike :: (a -> b) -> (TensorShape -> [b] -> b) -> TensorShape -> [a] -> b
foldMapTensorLike mkValue _mkVec [] [x] = mkValue x
foldMapTensorLike _mkValue _mkVec [] _xs = developerError "Mis-sized tensor. Expected a single element."
foldMapTensorLike mkValue mkVec (_ : ds) xs = do
  let inputVarIndicesChunks = chunksOf (product ds) xs
  let elems = fmap (foldMapTensorLike mkValue mkVec ds) inputVarIndicesChunks
  mkVec ds elems

instance (Pretty a) => Pretty (Tensor a) where
  pretty = foldMapTensor pretty (const prettyFlatList)

type RationalTensor = Tensor Rational

zeroTensor :: TensorShape -> RationalTensor
zeroTensor dims = Tensor dims (Vector.replicate (product dims) 0)

singletonTensor :: a -> Tensor a
singletonTensor a = Tensor [1] (Vector.singleton a)
