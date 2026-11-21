module Vehicle.Compile.Resource where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
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
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Networks

type NetworkName = Name

data NetworkType = NetworkType
  { networkInputType :: NetworkIOType,
    networkOutputType :: NetworkIOType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    pretty input <+> "->" <+> pretty output

type NetworkIOType = NetworkModality NetworkIOBase

data NetworkModality a
  = UniModal a
  | MultiModal [(Name, a)]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance (Pretty a) => Pretty (NetworkModality a) where
  pretty (UniModal e) = pretty e
  pretty (MultiModal es) = pretty es

instance Functor NetworkModality where
  fmap f (UniModal a) = UniModal (f a)
  fmap f (MultiModal as) = MultiModal (fmap applyRight as)
    where
      applyRight (a, b) = (a, f b)

data NetworkTensorType = NetworkTensorType
  { baseType :: NetworkBaseType,
    dimensions :: TensorShape
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data NetworkRecordType = NetworkRecordType
  { baseRecordType :: NetworkBaseType,
    recordTypeIdent :: Identifier,
    recordDims :: TensorShape, -- The dimensions of the tensor equivalent of the record
    recordFields :: [Name]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data NetworkIOBase
  = TensorIOType NetworkTensorType
  | RecordIOType NetworkRecordType
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Pretty NetworkIOBase where
  pretty = \case
    (TensorIOType (NetworkTensorType t dims)) -> "Tensor" <+> pretty t <+> pretty dims
    (RecordIOType (NetworkRecordType t ident dims fields)) ->
      "Record"
        <+> pretty ident
        <+> ":"
        <> line
        <> prettyMapEntries ((,typ) <$> map pretty fields)
      where
        typ = case dims of
          [] -> pretty t
          [_x] -> pretty t
          (_x : xs) -> "Tensor" <+> pretty t <+> pretty xs

getIODims :: NetworkIOBase -> TensorShape
getIODims (TensorIOType (NetworkTensorType _ dims)) = dims
getIODims (RecordIOType (NetworkRecordType _ _ dims _)) = dims

data NetworkBaseType
  = NetworkRatType
  deriving stock (Eq, Ord, Enum, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty RatType
