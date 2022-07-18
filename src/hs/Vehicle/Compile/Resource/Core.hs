
module Vehicle.Compile.Resource.Core where

import Data.Map (Map)
import Control.Monad.Except (MonadError(..))

import Vehicle.Language.AST
import Vehicle.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Dataset typing

data DatasetType
  = DatasetListType   DatasetType
  | DatasetTensorType DatasetType [Int]
  | DatasetBaseType   DatasetBaseType
  deriving (Show)

data DatasetBaseType
  = DatasetBoolType
  | DatasetNatType
  | DatasetIntType
  | DatasetRatType
  | DatasetIndexType Int
  deriving (Show)

getDatasetType :: forall m . MonadCompile m
               => Provenance
               -> Identifier
               -> CheckedExpr
               -> m DatasetType
getDatasetType ann ident datasetType = getContainerType True datasetType
  where
  getContainerType :: Bool -> CheckedExpr -> m DatasetType
  getContainerType topLevel = \case
    ListType   _ tElem       -> DatasetListType <$> getContainerType False tElem
    TensorType _ tElem tDims -> case getDimensions tDims of
      Just dims -> DatasetTensorType <$> getContainerType False tElem <*> pure dims
      Nothing   -> throwError $ DatasetVariableSizeTensor (ident, ann) tDims
    typ -> if topLevel
      then typingError
      else DatasetBaseType <$> getDatasetElemType typ

  getDatasetElemType :: CheckedExpr -> m DatasetBaseType
  getDatasetElemType elemType = case elemType of
    BoolType{}            -> return DatasetBoolType
    NatType{}             -> return DatasetNatType
    IntType{}             -> return DatasetIntType
    RatType{}             -> return DatasetRatType
    ConcreteIndexType _ n -> return $ DatasetIndexType n
    _ -> throwError $ DatasetTypeUnsupportedElement (ident, ann) elemType

  typingError :: m a
  typingError = compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple datasetType) <+>
      "should have been caught during type-checking"

datasetBaseType :: DatasetType -> DatasetBaseType
datasetBaseType = \case
  DatasetListType   tElem   -> datasetBaseType tElem
  DatasetTensorType tElem _ -> datasetBaseType tElem
  DatasetBaseType   tElem   -> tElem

reconstructDatasetBaseType :: Provenance -> DatasetBaseType -> CheckedExpr
reconstructDatasetBaseType ann = \case
  DatasetBoolType    -> BoolType ann
  DatasetNatType     -> NatType ann
  DatasetIntType     -> IntType ann
  DatasetRatType     -> RatType ann
  DatasetIndexType n -> ConcreteIndexType ann n

reconstructDatasetType :: Provenance -> DatasetType -> CheckedExpr
reconstructDatasetType ann = \case
  DatasetListType   tElem      -> ListType ann (reconstructDatasetType ann tElem)
  DatasetTensorType tElem dims -> mkTensorType ann (reconstructDatasetType ann tElem) dims
  DatasetBaseType   base       -> reconstructDatasetBaseType ann base

--------------------------------------------------------------------------------
-- Networks

data NetworkType = NetworkType
  { inputTensor  :: NetworkTensorType
  , outputTensor :: NetworkTensorType
  }

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    "[input =" <+> pretty input <+> "output =" <+> pretty output <> "]"

networkSize :: NetworkType -> Int
networkSize network = size (inputTensor network) + size (outputTensor network)


data NetworkTensorType = NetworkTensorType
  { size  :: Int
  , tElem :: NetworkBaseType
  }

instance Pretty NetworkTensorType where
  pretty (NetworkTensorType size tElem) =
    "Tensor" <+> pretty tElem <+> "[" <> pretty size <> "]"

data NetworkBaseType
  = NetworkRatType
  deriving (Enum)

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty (NumericType Rat)

reconstructNetworkBaseType :: Provenance -> NetworkBaseType -> CheckedExpr
reconstructNetworkBaseType ann NetworkRatType = RatType ann

allowedNetworkElementTypes :: [CheckedExpr]
allowedNetworkElementTypes = reconstructNetworkBaseType mempty <$>
  [(NetworkRatType :: NetworkBaseType) ..]

type MetaNetwork = [Symbol]

type NetworkContext = Map Symbol NetworkType
