module Vehicle.Prelude.IO
  ( vehicleFileExtension,
    vehicleObjectFileExtension,
    vehicleProofCacheFileExtension,
    vehicleLibraryExtension,
    removeFileIfExists,
    writeFileAtomically,
    fatalError,
    programOutput,
    getVehiclePath,
    ExternalOutputFormat (..),
  )
where

import Control.Exception (catch, throwIO)
-- import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Version (Version)
import Prettyprinter (Doc)
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hPrint, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempFile)
import System.Info (os)

--------------------------------------------------------------------------------
-- Files

vehicleFileExtension :: String
vehicleFileExtension = ".vcl"

vehicleObjectFileExtension :: String
vehicleObjectFileExtension = vehicleFileExtension <> "o"

vehicleProofCacheFileExtension :: String
vehicleProofCacheFileExtension = vehicleFileExtension <> "p"

vehicleLibraryExtension :: String
vehicleLibraryExtension = vehicleFileExtension <> "lib"

--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: MonadIO m => Doc a -> m b
fatalError message = liftIO $ do
  hPrint stderr message
  exitFailure

programOutput :: MonadIO m => Doc a -> m ()
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

getVehiclePath :: MonadIO m => m FilePath
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

-- | Writes a file atomically by first writing it to a temporary file and
-- then copying that temporary file into place.
writeFileAtomically :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
writeFileAtomically write file contents = do
  withSystemTempFile (takeFileName file) $ \tempFile _handle -> do
    write tempFile contents
    copyFile tempFile file

--------------------------------------------------------------------------------
-- Other

data ExternalOutputFormat = ExternalOutputFormat
  { formatName :: forall a. Doc a,
    formatVersion :: Maybe Version,
    commentToken :: forall a. Doc a
  }
