module Vehicle.Compile.Resource.Dataset
  ( expandDatasets
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT (runReaderT))
import Control.Monad.Except (MonadError(throwError))
import Data.Map qualified as Map
import System.FilePath (takeExtension)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Dataset.IDX (readIDX)

--------------------------------------------------------------------------------
-- Dataset expansion

-- | This function expands all the dataset declarations in the program with
-- the contents of the provided datasets.
expandDatasets :: (MonadIO m, MonadCompile m)
               => DatasetLocations
               -> InputProg
               -> m InputProg
expandDatasets resources prog1 = do
  logDebug MinDetail "Beginning expansion of datasets"
  incrCallDepth
  prog2 <- runReaderT (expandProg prog1) resources
  decrCallDepth
  logDebug MinDetail $ "Finished expansion of datasets" <> line
  return prog2

--------------------------------------------------------------------------------
-- Types

type MonadDataset m =
  ( MonadIO m
  , MonadCompile m
  , MonadReader DatasetLocations m
  )

expandProg :: MonadDataset m => InputProg -> m InputProg
expandProg (Main ds) = Main  <$> traverse expandDecl ds

expandDecl :: MonadDataset m => InputDecl -> m InputDecl
expandDecl (DefResource p Dataset ident t) = do
  resources <- ask
  e <- readDataset resources ident p
  return $ DefFunction p ident t e
expandDecl d = return d

readDataset :: (MonadIO m, MonadCompile m)
            => DatasetLocations
            -> Identifier
            -> Provenance
            -> m InputExpr
readDataset resources ident prov =
  case Map.lookup (nameOf ident) resources of
    Just file -> case takeExtension file of
      ".idx" -> readIDX file ident prov
      ext    -> throwError $ UnsupportedResourceFormat ident prov Dataset ext
    _ -> throwError $ ResourceNotProvided ident prov Dataset