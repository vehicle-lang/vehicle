module Vehicle.Data.Tensor where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Char qualified as Char
import Data.List qualified as List (stripPrefix)
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
  { tensorShape :: TensorShape,
    tensorValue :: Vector a
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)

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

zipWithTensor :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
zipWithTensor f t1 t2 =
  Tensor
    { tensorShape = tensorShape t1,
      tensorValue = Vector.zipWith f (tensorValue t1) (tensorValue t2)
    }

stack :: NonEmpty (Tensor a) -> Tensor a
stack tensors@(t :| ts) =
  Tensor (length tensors : tensorShape t) (Vector.concat (fmap tensorValue (t : ts)))

foldMapTensor :: forall a b. (a -> b) -> ([b] -> b) -> Tensor a -> b
foldMapTensor mkValue mkVec (Tensor dims value) = go dims (Vector.toList value)
  where
    go :: TensorShape -> [a] -> b
    go [] [x] = mkValue x
    go [] _xs = developerError "Mis-sized tensor. Expected a single element."
    go (_d : ds) xs = do
      let inputVarIndicesChunks = chunksOf (product ds) xs
      let elems = fmap (go ds) inputVarIndicesChunks
      mkVec elems

instance (Pretty a) => Pretty (Tensor a) where
  pretty = foldMapTensor pretty prettyFlatList

type RationalTensor = Tensor Rational

zeroTensor :: TensorShape -> RationalTensor
zeroTensor dims = Tensor dims (Vector.replicate (product dims) 0)
