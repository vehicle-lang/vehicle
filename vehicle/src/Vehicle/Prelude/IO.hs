module Vehicle.Prelude.IO
  ( vehicleSpecificationFileExtension,
    vehicleVerificationPlanFileExtension,
    vehicleObjectFileExtension,
    vehicleProofCacheFileExtension,
    vehicleLibraryExtension,
    removeFileIfExists,
    safeWriteToFile,
    safeReadFromFile,
    fatalError,
    programOutput,
    getVehiclePath,
    ExternalOutputFormat (..),
  )
where

-- import Control.Monad (forM_)

import Control.DeepSeq (NFData, deepseq)
import Control.Exception (bracket, catch, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Version (Version)
import GHC.IO.Handle (LockMode (..), hLock)
import Prettyprinter (Doc)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hPrint, openFile, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

--------------------------------------------------------------------------------
-- Files

baseFileExtension :: String
baseFileExtension = ".vcl"

vehicleSpecificationFileExtension :: String
vehicleSpecificationFileExtension = baseFileExtension

vehicleVerificationPlanFileExtension :: String
vehicleVerificationPlanFileExtension = baseFileExtension <> "e"

vehicleObjectFileExtension :: String
vehicleObjectFileExtension = baseFileExtension <> "o"

vehicleProofCacheFileExtension :: String
vehicleProofCacheFileExtension = baseFileExtension <> "p"

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

-- | Writes a file atomically by first acquiring a lock on it.
safeWriteToFile :: (Handle -> a -> IO ()) -> FilePath -> a -> IO ()
safeWriteToFile writeOp file contents = do
  let acquireFile = do
        handle <- openFile file WriteMode
        hLock handle ExclusiveLock
        return handle
  let processFile handle = writeOp handle contents
  let releaseFile = hClose

  bracket acquireFile releaseFile processFile

-- | Writes a file atomically by first acquiring a lock on it.
safeReadFromFile :: (NFData a) => (Handle -> IO a) -> FilePath -> IO a
safeReadFromFile readOp file = do
  let acquireFile = do
        handle <- openFile file ReadMode
        hLock handle SharedLock
        return handle
  let processFile handle = do
        result <- readOp handle
        result `deepseq` return result
  let releaseFile = hClose

  bracket acquireFile releaseFile processFile

--------------------------------------------------------------------------------
-- Other

data ExternalOutputFormat = ExternalOutputFormat
  { formatName :: forall a. Doc a,
    formatVersion :: Maybe Version,
    commentToken :: forall a. Doc a,
    emptyLines :: Bool
  }
