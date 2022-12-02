module Vehicle.Prelude.IO
  ( vehicleFileExtension
  , vehicleObjectFileExtension
  , vehicleProofCacheFileExtension
  , removeFileIfExists
  , fatalError
  , programOutput
  , LibraryName
  , installLibrary
  , getLibraryLocation
  ) where

import Control.Exception (catch, throwIO)
-- import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Prettyprinter (Doc)
import System.Directory (removeFile, createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath ((</>), (<.>))
import System.Info (os)

--------------------------------------------------------------------------------
-- Files

vehicleFileExtension :: String
vehicleFileExtension = ".vcl"

vehicleObjectFileExtension :: String
vehicleObjectFileExtension = vehicleFileExtension <> "o"

vehicleProofCacheFileExtension :: String
vehicleProofCacheFileExtension = vehicleFileExtension <> "p"

vehiclePathVariable :: String
vehiclePathVariable = "VEHICLE_PATH"

fallbackVehiclePathVariable :: String
fallbackVehiclePathVariable = case os of
  -- Windows
  "mingw32" -> "APPDATA"
  -- All other systems
  _         -> "HOME"

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

getVehiclePath :: MonadIO m => m FilePath
getVehiclePath = do
  vehiclePathVar <- liftIO $ lookupEnv vehiclePathVariable
  vehiclePath <- case vehiclePathVar of
    Just dir -> return dir
    Nothing -> do
      homeDir <- liftIO $ lookupEnv fallbackVehiclePathVariable
      case homeDir of
        Just dir -> return (dir </> ".vehicle")
        Nothing  -> do
          env <- liftIO getEnvironment
          error $
            "Could not find home directory via path variable " <>
            fallbackVehiclePathVariable <> ". But could find environment " <>
            "variables: " <> show env
  liftIO $ createDirectoryIfMissing False vehiclePath
  return vehiclePath

type LibraryName = String

installLibrary :: MonadIO m => LibraryName -> Text -> m ()
installLibrary name contents = do
  vehiclePath <- getVehiclePath
  let libraryFile = vehiclePath </> name <.> vehicleFileExtension
  liftIO $ TIO.writeFile libraryFile contents

getLibraryLocation :: MonadIO m => LibraryName -> m FilePath
getLibraryLocation name = do
  vehiclePath <- getVehiclePath
  return $ vehiclePath </> name <.> vehicleFileExtension
