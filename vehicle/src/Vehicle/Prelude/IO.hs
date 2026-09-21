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
    lockedReadFile,
    lockedWriteFile,
    lockedWriteTextFile,
    ExternalOutputFormat (..),
    CommentStyle (..),
    MonadStdIO (..),
  )
where

import Control.Exception (IOException, catch, throwIO)
-- import Control.Monad (forM_)

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer.Strict (WriterT)
import Data.ByteString qualified as BIO
import Data.Hashable (hash)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Version (Version)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (exitFailure)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hFileSize, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Info (os)
import Vehicle.Prelude.Prettyprinter

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

-- | Runs an action on a file while holding an OS-level lock associated with
-- it, so that concurrent instances of the compiler can't interleave reads and
-- writes, or read a half-written version, of the same file. Blocks until the
-- lock is available rather than failing immediately.
--
-- We take the lock itself out (using the `filelock` package) on a sidecar
-- lock file kept in the OS temporary directory, rather than on the target
-- file itself. On Windows, a file that has an OS-level lock held on it cannot
-- even be opened by another handle while the lock is held, so locking the
-- target file directly would make it impossible for any other process to
-- read or write it, defeating the point. Using a sidecar file in the
-- temporary directory avoids this, and keeps lock files out of the user's
-- own directories.
withLockedFile :: SharedExclusive -> FilePath -> IOMode -> (Handle -> IO a) -> IO a
withLockedFile lockMode filepath mode action = do
  lockFilePath <- getLockFilePath filepath
  withFileLock lockFilePath lockMode $ \_ ->
    withBinaryFile filepath mode action

-- | Computes the path of the sidecar lock file used to guard the given file,
-- creating the directory that holds it if necessary.
getLockFilePath :: FilePath -> IO FilePath
getLockFilePath filepath = do
  tmpDir <- getTemporaryDirectory
  let locksDir = tmpDir </> "vehicle-locks"
  createDirectoryIfMissing True locksDir
  canonicalPath <- canonicalizePath filepath `catch` \(_ :: IOException) -> return filepath
  return $ locksDir </> show (abs (hash canonicalPath)) <> ".lock"

-- | Reads a file while holding a shared OS-level file lock, so that it can't
-- be read while another instance of the compiler is mid-write to the same
-- file. Blocks until the lock is available rather than failing immediately.
--
-- Note we use `hGet` rather than `hGetContents` here, as the latter closes
-- the handle once it has read the contents, which would cause the subsequent
-- `hUnlock` in `withLockedFile` to fail with a bad file descriptor error.
lockedReadFile :: FilePath -> IO BIO.ByteString
lockedReadFile filepath =
  withLockedFile Shared filepath ReadMode $ \h -> do
    size <- hFileSize h
    BIO.hGet h (fromIntegral size)

lockedWriteTextFile :: FilePath -> Text -> IO ()
lockedWriteTextFile filepath contents =
  withLockedFile Exclusive filepath WriteMode $ \h -> TIO.hPutStr h contents

-- | Writes a file while holding an exclusive OS-level file lock, so that
-- concurrent instances of the compiler can't interleave writes to, or read a
-- half-written version of, the same file.
lockedWriteFile :: FilePath -> BIO.ByteString -> IO ()
lockedWriteFile filepath contents =
  withLockedFile Exclusive filepath WriteMode $ \h -> BIO.hPut h contents

fatalError :: (MonadStdIO m) => Doc a -> m b
fatalError message = do
  writeStderr $ layoutAsText message
  liftIO exitFailure

programOutput :: (MonadStdIO m) => Doc a -> m ()
programOutput message = writeStdoutLn $ layoutAsText message

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

data CommentStyle
  = Line (forall a. Doc a)
  | Block (forall a. Doc a) (forall a. Doc a)

data ExternalOutputFormat = ExternalOutputFormat
  { formatName :: forall a. Doc a,
    formatVersion :: Maybe Version,
    commentStyle :: CommentStyle,
    emptyLines :: Bool
  }
