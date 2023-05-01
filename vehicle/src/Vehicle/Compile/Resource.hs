module Vehicle.Compile.Resource where

import Data.Map (Map)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core

--------------------------------------------------------------------------------
-- Networks

data NetworkType = NetworkType
  { inputTensor :: NetworkTensorType,
    outputTensor :: NetworkTensorType
  }

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    "[input =" <+> pretty input <+> "output =" <+> pretty output <> "]"

networkSize :: NetworkType -> Int
networkSize network = tensorSize (inputTensor network) + tensorSize (outputTensor network)

data NetworkTensorType = NetworkTensorType
  { baseType :: NetworkBaseType,
    dimensions :: TensorDimensions
  }

tensorSize :: NetworkTensorType -> Int
tensorSize tensor = product (dimensions tensor)

instance Pretty NetworkTensorType where
  pretty tensor =
    "Tensor"
      <+> pretty (baseType tensor)
      <+> pretty (dimensions tensor)

data NetworkBaseType
  = NetworkRatType
  deriving (Enum)

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
