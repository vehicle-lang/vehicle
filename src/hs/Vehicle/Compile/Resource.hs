
module Vehicle.Compile.Resource where

import Data.Map (Map)
import Data.Set (Set)

import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Parameters

type ParameterContext = Set Symbol

--------------------------------------------------------------------------------
-- Implicit parameters

type ImplicitParameterContext =
  Map Symbol (Maybe (DeclProvenance, ResourceType, Int))

--------------------------------------------------------------------------------
-- Datasets

type DatasetContext = Set Symbol

--------------------------------------------------------------------------------
-- Networks

data NetworkType = NetworkType
  { inputTensor  :: NetworkTensorType
  , outputTensor :: NetworkTensorType
  }

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    "[input =" <+> pretty input <+> "output =" <+> pretty output <> "]"

data NetworkTensorType = NetworkTensorType
  { baseType    :: NetworkBaseType
  , dimensions  :: [Int]
  }

instance Pretty NetworkTensorType where
  pretty tensor = "Tensor" <+>
    pretty (baseType tensor) <+> pretty (dimensions tensor)

data NetworkBaseType
  = NetworkRatType
  deriving (Enum)

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty Rat

reconstructNetworkBaseType :: Provenance -> NetworkBaseType -> CheckedExpr
reconstructNetworkBaseType ann NetworkRatType = RatType ann

allowedNetworkElementTypes :: [CheckedExpr]
allowedNetworkElementTypes = reconstructNetworkBaseType mempty <$>
  [(NetworkRatType :: NetworkBaseType) ..]

type NetworkContext = Map Symbol NetworkType

type MetaNetwork = [Symbol]
