module Vehicle.Compile.RemoveDatasetDecls
  ( removeDatasetDecls
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT (runReaderT))

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Resource (readDataset)

--------------------------------------------------------------------------------
-- Dataset expansion

-- | This function replaces all dataset declarations from the program with the
-- contents of the provided dataset.
removeDatasetDecls :: (MonadIO m, MonadCompile m)
                   => ResourceLocations
                   -> CheckedProg
                   -> m CheckedProg
removeDatasetDecls resources prog1 = do
  logDebug "Beginning expansion of datasets"
  incrCallDepth

  prog2 <- runReaderT (expandProg prog1) resources
  logDebug $ prettySimple prog2

  decrCallDepth
  logDebug $ "Finished expansion of datasets" <> line
  return prog2

--------------------------------------------------------------------------------
-- Types

type MonadDataset m =
  ( MonadIO m
  , MonadCompile m
  , MonadReader ResourceLocations m
  )

expandProg :: MonadDataset m => CheckedProg -> m CheckedProg
expandProg (Main ds) = Main  <$> traverse expandDecl ds

expandDecl :: MonadDataset m => CheckedDecl -> m CheckedDecl
expandDecl (DefResource p Dataset ident t) = do
  resources <- ask
  e <- readDataset resources ident p t
  return $ DefFunction p ident t e
expandDecl d = return d