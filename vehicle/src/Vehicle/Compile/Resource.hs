module Vehicle.Compile.Resource where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Map (Map)
import GHC.Generics
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core

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
-- Context

type ParameterContext = Map Identifier StandardNormExpr

type DatasetContext = Map Identifier StandardNormExpr

type NetworkContext = Map Name (FilePath, NetworkType)

data ResourceContext = ResourceContext
  { parameterContext :: ParameterContext,
    datasetContext :: DatasetContext,
    networkContext :: NetworkContext
  }
