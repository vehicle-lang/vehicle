module Vehicle.Compile.Resource.Dataset
  ( getDatasetType
  , parseDataset
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError(throwError))
import Data.Map qualified as Map
import System.FilePath (takeExtension)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Dataset.IDX (readIDX)
import Vehicle.Compile.Resource.Core

--------------------------------------------------------------------------------
-- Dataset parsing

parseDataset :: (MonadIO m, MonadCompile m)
             => DatasetLocations
             -> Provenance
             -> Identifier
             -> CheckedExpr
             -> m CheckedExpr
parseDataset datasetLocations ann ident datasetType = do
  let name = nameOf ident

  internalType <- getDatasetType ann ident datasetType

  case Map.lookup name datasetLocations of
    Just file -> do
      logDebug MinDetail $ "reading" <+> squotes (pretty ident)
      value <- case takeExtension file of
        ".idx" -> readIDX file ident ann internalType
        ext    -> throwError $ UnsupportedResourceFormat ident ann Dataset ext
      return value
    _ -> throwError $ ResourceNotProvided ident ann Dataset
