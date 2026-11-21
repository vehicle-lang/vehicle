{-# LANGUAGE CPP #-}

module Vehicle.Prelude.Version
  ( VersionString,
    impreciseVehicleVersion,
    getPreciseVehicleVersion,
    DecodeResult (..),
    encodeAndWriteVersioned,
    readAndDecodeVersioned,
  )
where

import Control.Exception (IOException, catch, evaluate)
import Control.Monad.Trans (MonadIO (..))
import Data.Serialize (Serialize, decode, encode)
import Data.Version (showVersion)
#ifdef releaseBuild
#else
import Control.Exception (SomeException, try)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Hashable qualified as Hashable
import Numeric (showHex)
import System.Environment (getExecutablePath)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
#endif
import GHC.Generics (Generic)
import Paths_vehicle qualified as Cabal (version)
import Vehicle.Prelude.IO (lockedReadFile, lockedWriteFile)

--------------------------------------------------------------------------------
-- Current versions

-- | The imprecise current version of Vehicle. This can be used whenever the
-- precise version doesn't really matters. This is used when writing out the
-- Vehicle version number at the top of compiled files for example, and avoids
-- the golden file tests from complaining every time we make a fresh commit.
impreciseVehicleVersion :: String
impreciseVehicleVersion = showVersion Cabal.version
#ifdef releaseBuild
#else
  <> "+dev"
#endif

-- | The current precise version of Vehicle, that takes into account the current
-- dev build so that it can be used to identify which builds of Vehicle can safely
-- share serialised data (interface files, installed libraries etc.).
--
-- Note that we can't rely on decoding errors to detect incompatible data, as
-- `cereal` is untagged and will happily decode old bytes into well-typed but
-- semantically wrong values.
--
--   * Release builds: the Cabal version.
--
--   * Development builds: additionally includes a hash of the running
--     executable, so any rebuild (including from a dirty working tree)
--     invalidates all caches.
--
-- This is also what `vehicle --version` prints, as it precisely identifies
-- the build. Note it must remain compliant with PEP 440 as the Python bindings
-- parse it: https://peps.python.org/pep-0440/#local-version-identifiers
getPreciseVehicleVersion :: (MonadIO m) => m VersionString
getPreciseVehicleVersion = liftIO $ evaluate preciseVehicleVersion

preciseVehicleVersion :: VersionString
#ifdef releaseBuild
preciseVehicleVersion = showVersion Cabal.version
#else
preciseVehicleVersion = unsafePerformIO $ do
  -- The executable doesn't change during the lifetime of the process, so it is
  -- safe to compute this once and treat it as a constant.
  result <- try $ do
    exePath <- getExecutablePath
    contents <- LazyByteString.readFile exePath
    evaluate $ Hashable.hash contents
  suffix <- case result of
    Right exeHash -> return $ "exe." <> showHex (fromIntegral exeHash :: Word) ""
    Left (_ :: SomeException) -> do
      -- If we can't identify the executable, then we can't know which caches
      -- are safe to reuse. Use a random key so that nothing is ever reused.
      nonce <- randomIO :: IO Word
      return $ "uncacheable." <> showHex nonce ""
  return $ showVersion Cabal.version <> "+dev." <> suffix
{-# NOINLINE preciseVehicleVersion #-}
#endif

--------------------------------------------------------------------------------
-- Versioned objects

-- | Encodes a serialisable object along with the cache compatibility key of
-- the current build.
encodeAndWriteVersioned :: (MonadIO m, Serialize a) => FilePath -> a -> m ()
encodeAndWriteVersioned filepath payload = do
  key <- getPreciseVehicleVersion
  let encodedPayload = encode payload
  let versionedPayload = Versioned key encodedPayload
  let encodedVersionedPayload = encode versionedPayload
  liftIO $ lockedWriteFile filepath encodedVersionedPayload

-- | Attempts to deserialise a file that was encoded with `encodeAndWriteVersioned`.
readAndDecodeVersioned :: (MonadIO m, Serialize a) => FilePath -> m (DecodeResult a)
readAndDecodeVersioned filepath = do
  currentKey <- getPreciseVehicleVersion
  errorOrContents <- liftIO $ do
    (Right <$> lockedReadFile filepath) `catch` \(e :: IOException) -> return (Left e)

  case errorOrContents of
    Left err -> return $ IOError err
    Right bytestring -> case decode bytestring of
      Left err -> return $ InexplicableDecodingError err
      Right Versioned {..}
        | version /= currentKey ->
            return $ VersionMismatchError currentKey version
        | otherwise -> case decode payload of
            Left err -> return $ InexplicableDecodingError err
            Right v -> return $ SuccessfulDecoding v

data DecodeResult a
  = IOError IOException
  | InexplicableDecodingError String
  | VersionMismatchError VersionString VersionString
  | SuccessfulDecoding a

data Versioned a = Versioned
  { version :: VersionString,
    payload :: a
  }
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- `Data.Version` tags feature is deprecated so we can't use it.
type VersionString = String
