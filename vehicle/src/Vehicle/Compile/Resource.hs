module Vehicle.Compile.Resource where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Bifunctor (second)
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

data NetworkTensorModality a
  = TensorModality a
  | RecordModality Identifier [Name] (NetworkTensorModality a)
  deriving (Eq, Ord, Show, Generic)

data NetworkModality a
  = UniModal (NetworkTensorModality a)
  | MultiModal Identifier [(Name, NetworkTensorModality a)]
  deriving (Eq, Ord, Show, Generic)

instance (NFData a) => NFData (NetworkModality a)

instance (ToJSON a) => ToJSON (NetworkModality a)

instance (FromJSON a) => FromJSON (NetworkModality a)

instance (Pretty a) => Pretty (NetworkModality a) where
  pretty (UniModal e) = "UniModal" <+> pretty e
  pretty (MultiModal _ es) = "MultiModal" <+> pretty es

instance (Pretty a) => Pretty (NetworkTensorModality a) where
  pretty (TensorModality e) = pretty e
  pretty (RecordModality _name _fields e) = pretty e

instance (NFData a) => NFData (NetworkTensorModality a)

instance (ToJSON a) => ToJSON (NetworkTensorModality a)

instance (FromJSON a) => FromJSON (NetworkTensorModality a)

instance Functor NetworkTensorModality where
  fmap f (TensorModality a) = TensorModality (f a)
  fmap f (RecordModality i n a) = RecordModality i n (f <$> a)

instance Functor NetworkModality where
  fmap f (UniModal a) = UniModal (f <$> a)
  fmap f (MultiModal ty as) = MultiModal ty (second (fmap f) <$> as)

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
