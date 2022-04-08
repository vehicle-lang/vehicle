
module Vehicle.Resource
  ( module X
  , readDataset
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map qualified as Map
import System.FilePath (takeExtension)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error

import Vehicle.Resource.Core as X
import Vehicle.Resource.Dataset.IDX

--------------------------------------------------------------------------------
-- Monad

readDataset :: (MonadIO m, MonadCompile m)
            => ResourceLocations
            -> Identifier
            -> Provenance
            -> CheckedExpr
            -> m CheckedExpr
readDataset resources ident prov expectedType =
  case Map.lookup (nameOf ident) resources of
    Just (Dataset, file) -> case takeExtension file of
      ".idx" -> readIDX file ident prov expectedType
      ext    -> throwError $ UnsupportedResourceFormat ident prov Dataset ext
    _ -> throwError $ ResourceNotProvided ident prov Dataset