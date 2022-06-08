module Vehicle.Compile.Resource.Dataset
  ( checkDatasetType
  , parseDataset
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError(throwError))
import Data.Bifunctor (second)
import Data.Map qualified as Map
import System.FilePath (takeExtension)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Dataset.IDX (readIDX)
import Vehicle.Compile.Resource.Core
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Dataset typing

getDatasetType :: Bool
               -> CheckedAnn
               -> Identifier
               -> CheckedExpr
               -> Either CompileError DatasetType
getDatasetType topLevel ann ident typ = case typ of
  ListType   _ tElem       -> DatasetListType   <$> getDatasetType False ann ident tElem
  TensorType _ tElem tDims -> DatasetTensorType <$> getDatasetType False ann ident tElem <*> pure tDims
  _                        -> do
    let elemType = getDatasetElemType ann ident typ
    if topLevel
      then Left $ DatasetTypeUnsupportedContainer ident ann typ
      else second DatasetBaseType elemType

getDatasetElemType :: CheckedAnn
                   -> Identifier
                   -> CheckedExpr
                   -> Either CompileError DatasetBaseType
getDatasetElemType ann ident elemType = case elemType of
  BoolType{}    -> Right DatasetBoolType
  NatType{}     -> Right DatasetNatType
  IntType{}     -> Right DatasetIntType
  RatType{}     -> Right DatasetRatType
  IndexType _ n -> Right $ DatasetIndexType n
  _             -> Left $ DatasetTypeUnsupportedElement ident ann elemType

checkDatasetType :: MonadCompile m
                 => CheckedAnn
                 -> Identifier
                 -> CheckedExpr
                 -> m ()
checkDatasetType ann ident datasetType =
  case getDatasetType True ann ident datasetType of
    Left err -> throwError err
    Right{}  -> return ()

--------------------------------------------------------------------------------
-- Dataset parsing

parseDataset :: (MonadIO m, MonadCompile m)
             => DatasetLocations
             -> CheckedAnn
             -> Identifier
             -> CheckedExpr
             -> m CheckedExpr
parseDataset datasetLocations ann ident datasetType = do
  let name = nameOf ident

  internalType <- case getDatasetType True ann ident datasetType of
    Left{} -> compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple datasetType) <+>
      "should have been caught during type-checking"
    Right res -> return res

  case Map.lookup name datasetLocations of
    Just file -> do
      logDebug MinDetail $ "reading" <+> squotes (pretty ident)
      value <- case takeExtension file of
        ".idx" -> readIDX file ident ann internalType
        ext    -> throwError $ UnsupportedResourceFormat ident ann Dataset ext
      return value
    _ -> throwError $ ResourceNotProvided ident ann Dataset