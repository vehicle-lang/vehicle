module Vehicle.Backend.Agda.Interact
  ( writeAgdaFile,
    writeResultToFile,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text.IO qualified as TIO
import Data.Version (makeVersion)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Vehicle.Prelude.Logging

writeAgdaFile ::
  (MonadLogger m, MonadIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeAgdaFile = writeResultToFile (Just agdaOutputFormat)

agdaOutputFormat :: ExternalOutputFormat
agdaOutputFormat =
  ExternalOutputFormat
    { formatName = "Agda",
      formatVersion = Just $ makeVersion [2, 6, 2],
      commentToken = "--",
      emptyLines = True
    }

-- TODO This really needs to move.
writeResultToFile ::
  (MonadIO m, MonadLogger m) =>
  Maybe ExternalOutputFormat ->
  Maybe FilePath ->
  Doc a ->
  m ()
writeResultToFile target filepath doc = do
  let text = layoutAsText $ prependfileHeader doc target
  case filepath of
    Nothing -> liftIO $ TIO.putStrLn text
    Just outputFilePath -> do
      logDebug MaxDetail $ "Creating file:" <+> pretty filepath
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory outputFilePath)
        TIO.writeFile outputFilePath text
