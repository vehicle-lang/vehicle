module Vehicle.Compile.Resource.Dataset
  ( expandDatasets
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer (MonadWriter(..), WriterT (runWriterT))
import Control.Monad.Reader (MonadReader(..), ReaderT (runReaderT))
import Control.Monad.Except (MonadError(throwError))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
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
expandDatasets datasets prog1 = logCompilerPass "insertion of datasets" $ do
  (prog2, foundDatasets) <- runWriterT (runReaderT (expandProg prog1) datasets)
  warnIfUnusedResources Dataset (Map.keysSet datasets) foundDatasets
  return prog2

--------------------------------------------------------------------------------
-- Types

type MonadDataset m =
  ( MonadIO m
  , MonadCompile m
  , MonadReader DatasetLocations m
  , MonadWriter (Set Symbol) m
  )

expandProg :: MonadDataset m => InputProg -> m InputProg
expandProg (Main ds) = Main  <$> traverse expandDecl ds

expandDecl :: MonadDataset m => InputDecl -> m InputDecl
expandDecl (DefResource p Dataset ident t) = do
  resources <- ask
  let name = nameOf ident
  e <- case Map.lookup name resources of
    Just file -> do
      tell (Set.singleton name)
      case takeExtension file of
        ".idx" -> readIDX file ident p
        ext    -> throwError $ UnsupportedResourceFormat ident p Dataset ext
    _ -> throwError $ ResourceNotProvided ident p Dataset
  return $ DefFunction p ident t e
expandDecl d = return d