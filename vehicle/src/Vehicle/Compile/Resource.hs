module Vehicle.Compile.Resource where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics
import Prettyprinter
import Vehicle.Data.Builtin.Core (BuiltinType (..))
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Parameters

data ParameterType sizeType
  = ParameterBoolType
  | ParameterRealType
  | ParameterNatType
  | ParameterIndexType sizeType

--------------------------------------------------------------------------------
-- Dataset

data DatasetType sizeType
  = DatasetVectorType (DatasetType sizeType) sizeType
  | DatasetListType (DatasetType sizeType)
  | DatasetTensorType (DatasetElementType sizeType) sizeType
  | DatasetRecordType Identifier (GenericRecordFields (DatasetType sizeType))
  | DatasetElementType (DatasetElementType sizeType)

data DatasetElementType sizeType
  = DatasetRealType
  | DatasetNatType
  | DatasetIndexType sizeType
  deriving (Show)

--------------------------------------------------------------------------------
-- Networks

type NetworkName = Name

data NetworkType = NetworkType
  { networkInputType :: NetworkIOType,
    networkOutputType :: NetworkIOType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkType

instance ToJSON NetworkType

instance FromJSON NetworkType

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    pretty input <+> "->" <+> pretty output

type NetworkIOType = NetworkModality NetworkIOBase

data NetworkIOBase = NetworkIOBase
  { baseType :: NetworkBaseType,
    dimensions :: TensorShape
  }
  deriving (Eq, Ord, Show, Generic)

data AbstractNetworkModality a
  = UniModal a
  | MultiModal Identifier [(Name, a)]
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data NetworkTensorModality a
  = TensorModality a
  | RecordModality Identifier (NonEmpty (Name, NetworkTensorModality a))
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

tensorModalityDepth :: NetworkTensorModality TensorShape -> Int
tensorModalityDepth = \case
  TensorModality dims -> length dims
  RecordModality _ ((_, shape) :| _) -> 1 + tensorModalityDepth shape

type NetworkModality a = AbstractNetworkModality (NetworkTensorModality a)

networkModalityDepth :: NetworkModality TensorShape -> Int
networkModalityDepth = \case
  UniModal a -> tensorModalityDepth a
  MultiModal _ as -> maximum (fmap (tensorModalityDepth . snd) as)

mapNetworkModality :: (a -> b) -> NetworkModality a -> NetworkModality b
mapNetworkModality f = fmap (fmap f)

traverseNetworkModality :: (Monad t) => (a -> t b) -> NetworkModality a -> t (NetworkModality b)
traverseNetworkModality f = traverse (traverse f)

concatNetworkModality :: AbstractNetworkModality a -> [a]
concatNetworkModality = \case
  UniModal x -> [x]
  MultiModal _ fields -> fmap snd fields

instance (NFData a) => NFData (AbstractNetworkModality a)

instance (ToJSON a) => ToJSON (AbstractNetworkModality a)

instance (FromJSON a) => FromJSON (AbstractNetworkModality a)

instance (Pretty a) => Pretty (AbstractNetworkModality a) where
  pretty (UniModal e) = "UniModal" <+> pretty e
  pretty (MultiModal _ es) = "MultiModal" <+> pretty es

instance (Pretty a) => Pretty (NetworkTensorModality a) where
  pretty (TensorModality e) = pretty e
  pretty (RecordModality _name fields) = prettyMapEntries (fmap (bimap pretty pretty) (NonEmpty.toList fields))

instance (NFData a) => NFData (NetworkTensorModality a)

instance (ToJSON a) => ToJSON (NetworkTensorModality a)

instance (FromJSON a) => FromJSON (NetworkTensorModality a)

instance NFData NetworkIOBase

instance ToJSON NetworkIOBase

instance FromJSON NetworkIOBase

instance Pretty NetworkIOBase where
  pretty = \case
    (NetworkIOBase t dims) -> "Tensor" <+> pretty t <+> pretty dims

data NetworkBaseType
  = NetworkRatType
  deriving (Eq, Ord, Enum, Show, Generic)

instance NFData NetworkBaseType

instance ToJSON NetworkBaseType

instance FromJSON NetworkBaseType

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty RatType
