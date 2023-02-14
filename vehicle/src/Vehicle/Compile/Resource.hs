module Vehicle.Compile.Resource where

import Data.Map (Map)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn (DBType)

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
    dimensions :: [Int]
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

reconstructNetworkBaseType :: NetworkBaseType -> Provenance -> DBType Builtin
reconstructNetworkBaseType NetworkRatType = RatType

type NetworkContext = Map Name (FilePath, NetworkType)
