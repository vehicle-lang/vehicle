module Vehicle.Compile.Resource where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Networks

type NetworkName = Name

data NetworkType = NetworkType
  { inputTensor :: NetworkIOType,
    outputTensor :: NetworkIOType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkType

instance ToJSON NetworkType

instance FromJSON NetworkType

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    pretty input <+> "->" <+> pretty output

networkSize :: NetworkType -> Int
networkSize network = tensorSize (inputTensor network) + tensorSize (outputTensor network)

data NetworkTensorType = NetworkTensorType
  { baseType :: NetworkBaseType,
    dimensions :: TensorShape
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkTensorType

instance ToJSON NetworkTensorType

instance FromJSON NetworkTensorType

-- TODO: move
-- Hopefully we would only have to store the fieldnames
-- Field types are the same and something compatible with networks
-- Tensor network seems to make baseRecordType always the same?
type GenericRecordFieldNames = [FieldName]

data NetworkRecordType = NetworkRecordType
  { baseRecordType :: NetworkBaseType, -- TODO: see if we can name these better? not sure why we cant have duplicate names here
    recordTypeIdent :: Identifier, -- trying to avoid making things complicated lol
    recordDimensions :: TensorShape,
    recordFields :: GenericRecordFieldNames
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkRecordType

instance ToJSON NetworkRecordType

instance FromJSON NetworkRecordType

-- TODO: rename
data NetworkIOType
  = NetworkTensorTypeConstructor NetworkTensorType
  | NetworkRecordTypeConstructor NetworkRecordType
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkIOType

instance ToJSON NetworkIOType

instance FromJSON NetworkIOType

-- TODO: rename
tensorSize :: NetworkIOType -> Int
tensorSize typ = case typ of
  NetworkTensorTypeConstructor tensor -> product (dimensions tensor)
  NetworkRecordTypeConstructor record -> product (recordDimensions record)

instance Pretty NetworkTensorType where
  pretty tensor =
    "Tensor"
      <+> pretty (baseType tensor)
      <+> pretty (dimensions tensor)

instance Pretty NetworkRecordType where
  pretty record =
    "Record"
      <+> pretty (baseRecordType record)
      <+> pretty (recordDimensions record)

instance Pretty NetworkIOType where
  pretty = \case
    NetworkTensorTypeConstructor tensor -> pretty tensor
    NetworkRecordTypeConstructor record -> pretty record

data NetworkBaseType
  = NetworkRatType
  deriving (Eq, Ord, Enum, Show, Generic)

instance NFData NetworkBaseType

instance ToJSON NetworkBaseType

instance FromJSON NetworkBaseType

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty RatType
