
module Vehicle.Compile.Resource.Core where

import Data.Map (Map)

import Vehicle.Language.AST
import Vehicle.Prelude
import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Parameters

data ParameterType
  = ParamBoolType
  | ParamNatType
  | ParamIntType
  | ParamRatType
  | ParamIndexType CheckedExpr

type ParameterContext = Map Symbol CheckedExpr

--------------------------------------------------------------------------------
-- Datasets

data DatasetType
  = DatasetListType DatasetType
  | DatasetTensorType DatasetType CheckedExpr
  | DatasetBaseType DatasetBaseType
  deriving (Show)

data DatasetBaseType
  = DatasetBoolType
  | DatasetNatType
  | DatasetIntType
  | DatasetRatType
  | DatasetIndexType CheckedExpr
  deriving (Show)

datasetBaseType :: DatasetType -> DatasetBaseType
datasetBaseType = \case
  DatasetListType   tElem   -> datasetBaseType tElem
  DatasetTensorType tElem _ -> datasetBaseType tElem
  DatasetBaseType   tElem   -> tElem

reconstructDatasetBaseType :: CheckedAnn -> DatasetBaseType -> CheckedExpr
reconstructDatasetBaseType ann = \case
  DatasetBoolType    -> BoolType ann
  DatasetNatType     -> NatType ann
  DatasetIntType     -> IntType ann
  DatasetRatType     -> RatType ann
  DatasetIndexType n -> IndexType ann n

reconstructDatasetType :: CheckedAnn -> DatasetType -> CheckedExpr
reconstructDatasetType ann = \case
  DatasetListType   tElem      -> ListType ann (reconstructDatasetType ann tElem)
  DatasetTensorType tElem dims -> TensorType ann (reconstructDatasetType ann tElem) dims
  DatasetBaseType   base       -> reconstructDatasetBaseType ann base

type DatasetContext = Map Symbol CheckedExpr

--------------------------------------------------------------------------------
-- Networks

data AbstractNetworkType a = NetworkType
  { inputTensor  :: AbstractNetworkTensorType a
  , outputTensor :: AbstractNetworkTensorType a
  }

type NetworkType                = AbstractNetworkType Int
type NetworkTypeWithUnknownDims = AbstractNetworkType CheckedExpr

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    "[input =" <+> pretty input <+> "output =" <+> pretty output <> "]"

networkSize :: NetworkType -> Int
networkSize network = size (inputTensor network) + size (outputTensor network)


data AbstractNetworkTensorType a = NetworkTensorType
  { size  :: a
  , tElem :: NetworkBaseType
  }

type NetworkTensorType                = AbstractNetworkTensorType Int
type NetworkTensorTypeWithUnknownDims = AbstractNetworkTensorType CheckedExpr

instance Pretty NetworkTensorType where
  pretty (NetworkTensorType size tElem) =
    "Tensor" <+> pretty tElem <+> "[" <> pretty size <> "]"

data NetworkBaseType
  = NetworkRatType
  deriving (Enum)

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty (NumericType Rat)

reconstructNetworkBaseType :: CheckedAnn -> NetworkBaseType -> CheckedExpr
reconstructNetworkBaseType ann NetworkRatType = RatType ann

allowedNetworkElementTypes :: [CheckedExpr]
allowedNetworkElementTypes = reconstructNetworkBaseType mempty <$>
  [(NetworkRatType :: NetworkBaseType) ..]

type MetaNetwork = [Symbol]

type NetworkContext = Map Symbol NetworkType
