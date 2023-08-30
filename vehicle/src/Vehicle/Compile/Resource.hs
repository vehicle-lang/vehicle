module Vehicle.Compile.Resource where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Hashable (Hashable)
import GHC.Generics
import Vehicle.Prelude
import Vehicle.Syntax.Builtin (BuiltinType (..))

--------------------------------------------------------------------------------
-- Networks

data NetworkType = NetworkType
  { inputTensor :: NetworkTensorType,
    outputTensor :: NetworkTensorType
  }
  deriving (Show, Generic)

instance ToJSON NetworkType

instance FromJSON NetworkType

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    pretty input <+> "->" <+> pretty output

networkSize :: NetworkType -> Int
networkSize network = tensorSize (inputTensor network) + tensorSize (outputTensor network)

data NetworkTensorType = NetworkTensorType
  { baseType :: NetworkBaseType,
    dimensions :: TensorDimensions
  }
  deriving (Show, Generic)

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
  deriving (Enum, Show, Generic)

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
