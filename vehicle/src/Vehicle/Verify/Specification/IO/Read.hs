module Vehicle.Verify.Specification.IO.Read
  ( MonadReadSpecification,
    readSpecification,
  )
where

import Colog.Core.Action
import Colog.Core.Severity
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Text.IO qualified as TIO
import Language.LSP.Logging
import Language.LSP.Protocol.Types (filePathToUri, toNormalizedUri)
import Language.LSP.Server (LspT, MonadLsp (..), getVirtualFile)
import Language.LSP.VFS (virtualFileText)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Specification

class (Monad m) => MonadReadSpecification m where
  readSpecification :: FilePath -> m ModuleText

instance (MonadStdIO IO) => MonadReadSpecification IO where
  readSpecification = readSpecificationFromDisk

instance (MonadUnliftIO m) => MonadReadSpecification (LspT cfg m) where
  readSpecification = readSpecificationFromLspVfs

instance (MonadReadSpecification m) => MonadReadSpecification (ExceptT e m) where
  readSpecification = lift . readSpecification

instance (MonadReadSpecification m) => MonadReadSpecification (ReaderT w m) where
  readSpecification = lift . readSpecification

instance (MonadReadSpecification m) => MonadReadSpecification (StateT s m) where
  readSpecification = lift . readSpecification

readFileFromDiskOrErrMsg :: (MonadUnliftIO m) => FilePath -> m (Either (Doc ann) ModuleText)
readFileFromDiskOrErrMsg inputFile = do
  errorOrContents <- liftIO $ try @IOException $ TIO.readFile inputFile

  case errorOrContents of
    Left err ->
      return $
        Left $
          "Error occured while reading specification"
            <+> quotePretty inputFile
            <> ":"
            <> line
            <> indent 2 (pretty (show err))
    Right contents -> return $ Right contents

readSpecificationFromDisk :: (MonadStdIO m) => FilePath -> m ModuleText
readSpecificationFromDisk inputFile
  | takeExtension inputFile /= specificationFileExtension = do
      fatalError $
        "Specification"
          <+> quotePretty inputFile
          <+> "has unsupported"
          <+> "extension"
          <+> quotePretty (takeExtension inputFile)
          <> "."
            <+> "Only files with a"
            <+> quotePretty specificationFileExtension
            <+> "extension are supported."
  | otherwise = do
      errorOrContents <- liftIO $ readFileFromDiskOrErrMsg inputFile

      case errorOrContents of
        Left err -> fatalError err
        Right contents -> return contents

readSpecificationFromLspVfs :: (MonadLsp config m) => FilePath -> m ModuleText
readSpecificationFromLspVfs inputFile = do
  let inputFileUri = toNormalizedUri $ filePathToUri inputFile
  maybeVf <- getVirtualFile inputFileUri
  case maybeVf of
    Just vf -> return $ virtualFileText vf
    Nothing -> do
      errorOrContents <- liftIO $ readFileFromDiskOrErrMsg inputFile

      case errorOrContents of
        Left err -> do
          logToShowMessage
            <& ( renderStrict $ layoutPretty defaultLayoutOptions $ err
               )
              `WithSeverity` Error
          liftIO exitFailure -- TODO: probably should handle this more gracefully
        Right contents -> return contents
