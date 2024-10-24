module Vehicle.Compile.Resource where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Hashable (Hashable)
import GHC.Generics
import Vehicle.Data.Builtin.Core (BuiltinType (..))
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Networks

data NetworkType = NetworkType
  { inputTensor :: NetworkTensorType,
    outputTensor :: NetworkTensorType
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

tensorSize :: NetworkTensorType -> Int
tensorSize tensor = product (dimensions tensor)

instance Pretty NetworkTensorType where
  pretty tensor =
    "Tensor"
      <+> pretty (baseType tensor)
      <+> pretty (dimensions tensor)

data NetworkBaseType
  = NetworkRatType
  deriving (Eq, Ord, Enum, Show, Generic)

instance NFData NetworkBaseType

instance ToJSON NetworkBaseType

instance FromJSON NetworkBaseType

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty Rat

--------------------------------------------------------------------------------
-- Resource constraints

data ResourceConstraint
  = IsValidParameterType
  | IsValidDatasetType
  | IsValidNetworkType
  | IsValidPropertyType
  deriving (Show, Eq, Ord, Generic)

instance Hashable ResourceConstraint

instance Pretty ResourceConstraint where
  pretty = \case
    IsValidParameterType -> "IsValidParameterType"
    IsValidDatasetType -> "IsValidDatasetType"
    IsValidNetworkType -> "IsValidNetworkType"
    IsValidPropertyType -> "IsValidPropertyType"
