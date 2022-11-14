module Vehicle.Compile.ExpandResources.Dataset
  ( parseDataset
  ) where

import Control.Monad.Except (MonadError (..), MonadIO)
import Data.Map qualified as Map
import System.FilePath (takeExtension)

import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset.IDX (readIDX)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Normalise.NormExpr

--------------------------------------------------------------------------------
-- Dataset parsing

parseDataset :: (MonadIO m, MonadExpandResources m)
             => DatasetLocations
             -> DeclProvenance
             -> GluedType
             -> m NormExpr
parseDataset datasetLocations decl@(ident, _) expectedType =
  case Map.lookup (nameOf ident) datasetLocations of
    Just file -> do
      logDebug MinDetail $ "Reading" <+> squotes (pretty ident)
      value <- case takeExtension file of
        ".idx" -> readIDX file decl expectedType
        ext    -> throwError $ UnsupportedResourceFormat decl Dataset ext
      return value
    _ -> throwError $ ResourceNotProvided decl Dataset
