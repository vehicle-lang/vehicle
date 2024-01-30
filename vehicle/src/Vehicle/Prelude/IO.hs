module Vehicle.Prelude.IO
  ( specificationFileExtension,
    specificationCacheIndexFileExtension,
    propertyVerificationResultFileExtension,
    propertyVerificationPlanFileExtension,
    vehicleObjectFileExtension,
    vehicleLibraryExtension,
    removeFileIfExists,
    fatalError,
    programOutput,
    getVehiclePath,
    ExternalOutputFormat (..),
    MonadStdIO (..),
  )
where

import Control.Exception (catch, throwIO)
-- import Control.Monad (forM_)

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (WriterT)
import Data.Text (Text)
import Data.Version (Version)
import Prettyprinter (Doc)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPrint, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

--------------------------------------------------------------------------------
-- Streams

class (MonadIO m) => MonadStdIO m where
  writeStdout :: Text -> m ()
  writeStderr :: Text -> m ()

  writeStdoutLn :: Text -> m ()
  writeStdoutLn = writeStdout . (<> "\n")

  writeStderrLn :: Text -> m ()
  writeStderrLn = writeStderr . (<> "\n")

{-# SPECIALIZE writeStdout :: Text -> IO () #-}

{-# SPECIALIZE writeStdoutLn :: Text -> IO () #-}

{-# SPECIALIZE writeStderr :: Text -> IO () #-}

{-# SPECIALIZE writeStderrLn :: Text -> IO () #-}

instance (MonadStdIO m) => MonadStdIO (StateT s m) where
  writeStdout :: (MonadStdIO m) => Text -> StateT s m ()
  writeStdout = lift . writeStdout
  writeStderr :: (MonadStdIO m) => Text -> StateT s m ()
  writeStderr = lift . writeStderr

instance (MonadStdIO m) => MonadStdIO (ReaderT s m) where
  writeStdout :: (MonadStdIO m) => Text -> ReaderT s m ()
  writeStdout = lift . writeStdout
  writeStderr :: (MonadStdIO m) => Text -> ReaderT s m ()
  writeStderr = lift . writeStderr

instance (Monoid w, MonadStdIO m) => MonadStdIO (WriterT w m) where
  writeStdout :: (Monoid w, MonadStdIO m) => Text -> WriterT w m ()
  writeStdout = lift . writeStdout
  writeStderr :: (Monoid w, MonadStdIO m) => Text -> WriterT w m ()
  writeStderr = lift . writeStderr

instance (MonadStdIO m) => MonadStdIO (IdentityT m) where
  writeStdout = lift . writeStdout
  writeStderr = lift . writeStderr
  writeStdoutLn = lift . writeStdoutLn
  writeStderrLn = lift . writeStderrLn

instance (MonadStdIO m) => MonadStdIO (ExceptT e m) where
  writeStdout :: (MonadStdIO m) => Text -> ExceptT e m ()
  writeStdout = lift . writeStdout
  writeStderr :: (MonadStdIO m) => Text -> ExceptT e m ()
  writeStderr = lift . writeStderr

--------------------------------------------------------------------------------
-- Files

baseFileExtension :: String
baseFileExtension = ".vcl"

specificationFileExtension :: String
specificationFileExtension = baseFileExtension

specificationCacheIndexFileExtension :: String
specificationCacheIndexFileExtension = baseFileExtension <> "-cache-index"

propertyVerificationPlanFileExtension :: String
propertyVerificationPlanFileExtension = baseFileExtension <> "-plan"

propertyVerificationResultFileExtension :: String
propertyVerificationResultFileExtension = baseFileExtension <> "-result"

vehicleObjectFileExtension :: String
vehicleObjectFileExtension = baseFileExtension <> "o"

vehicleLibraryExtension :: String
vehicleLibraryExtension = baseFileExtension <> "lib"

--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: (MonadIO m) => Doc a -> m b
fatalError message = liftIO $ do
  hPrint stderr message
  exitFailure

programOutput :: (MonadIO m) => Doc a -> m ()
programOutput message = liftIO $ print message

--------------------------------------------------------------------------------
-- Library utilities

vehiclePathVariable :: String
vehiclePathVariable = "VEHICLE_PATH"

fallbackVehiclePathVariable :: String
fallbackVehiclePathVariable = case os of
  -- Windows
  "mingw32" -> "APPDATA"
  -- All other systems
  _ -> "HOME"

getVehiclePath :: (MonadIO m) => m FilePath
getVehiclePath = do
  vehiclePathVar <- liftIO $ lookupEnv vehiclePathVariable
  vehiclePath <- case vehiclePathVar of
    Just dir -> return dir
    Nothing -> do
      homeDir <- liftIO $ lookupEnv fallbackVehiclePathVariable
      case homeDir of
        Just dir -> return (dir </> ".vehicle")
        Nothing -> do
          env <- liftIO getEnvironment
          error $
            "Could not find home directory via path variable "
              <> fallbackVehiclePathVariable
              <> ". But could find environment "
              <> "variables: "
              <> show env
  liftIO $ createDirectoryIfMissing False vehiclePath
  return vehiclePath

--------------------------------------------------------------------------------
-- Other

data ExternalOutputFormat = ExternalOutputFormat
  { formatName :: forall a. Doc a,
    formatVersion :: Maybe Version,
    commentToken :: forall a. Doc a,
    emptyLines :: Bool
  }
