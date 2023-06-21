{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Vehicle.Prelude.Version
  ( VersionString,
    preciseVehicleVersion,
    impreciseVehicleVersion,
    DecodeResult (..),
    encodeAndWriteVersioned,
    readAndDecodeVersioned,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad.Trans (MonadIO (..))
import Data.ByteString qualified as BIO
import Data.Serialize (Serialize, decode, encode)
import Data.Version (showVersion)
import Development.GitRev
import GHC.Generics (Generic)
import Paths_vehicle qualified as Cabal (version)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Current versions

-- | The imprecise current version of Vehicle. This can be used whenever the
-- precise version doesn't really matters. This is used when writing out the
-- Vehicle version number at the top of compiled files for example, and avoids
-- the golden file tests from complaining every time we make a fresh commit.
--
-- This uses Template Haskell to append `dev` to the version in the Cabal file
-- for all builds, unless the flag `release-build` is set in the Cabal file.
impreciseVehicleVersion :: String
impreciseVehicleVersion = showVersion Cabal.version
#ifdef releaseBuild
#else
  <> "+dev"
#endif

-- | The precise current version of Vehicle. This should be used whenever the
-- exact version really matters, e.g. serialisation compatability.
--
-- This uses Template Haskell to append the github commit to the version in
-- the Cabal file for all builds, unless the flag `release-build` is set in the
-- Cabal file.
preciseVehicleVersion :: String
preciseVehicleVersion = showVersion Cabal.version
#ifdef releaseBuild
#else
  -- Note this needs to be compliant with
  -- https://peps.python.org/pep-0440/#local-version-identifiers
  <> maybe "" ("+" ++) commitInfo
  where
  -- Taken from src/full/Agda/VersionCommit.hs
  commitInfo :: Maybe String
  commitInfo
    | hash == "UNKNOWN" = Nothing
    | otherwise         = Just $ abbrev hash ++ dirty
    where
      hash = $(gitHash)

      -- Check if any tracked files have uncommitted changes
      dirty | $(gitDirtyTracked) = ".dirty"
            | otherwise          = ""

      -- Abbreviate a commit hash while keeping it unambiguous
      abbrev = take 7
#endif

--------------------------------------------------------------------------------
-- Versioned objects

-- | Encodes a serialisable object along with the precise version of Vehicle
-- that was used. This version includes the commit hash.
encodeAndWriteVersioned :: (MonadIO m, Serialize a) => FilePath -> a -> m ()
encodeAndWriteVersioned filepath payload = do
  let encodedPayload = encode payload
  let versionedPayload = Versioned preciseVehicleVersion encodedPayload
  let encodedVersionedPayload = encode versionedPayload
  liftIO $ BIO.writeFile filepath encodedVersionedPayload

-- | Attempts to deserialise a file that was encoded with `encodeAndWriteVersioned`.
readAndDecodeVersioned :: (MonadIO m, Serialize a) => FilePath -> m (DecodeResult a)
readAndDecodeVersioned filepath = do
  errorOrContents <- liftIO $ do
    (Right <$> BIO.readFile filepath) `catch` \(e :: IOException) -> return (Left e)

  case errorOrContents of
    Left err -> return $ IOError err
    Right bytestring -> case decode bytestring of
      Left err -> return $ InexplicableDecodingError err
      Right Versioned {..} -> do
        -- The version is always potentially out of sync if the Github repo is
        -- in a dirty state
        let isOutOfSync = $(gitDirtyTracked) || version /= preciseVehicleVersion
        if isOutOfSync
          then do
            -- If we are out of sync then we still try to restore the payload but
            -- we need to attach a timeout to it as unfortunately the deserializer
            -- can potentially loop reading malformed data.
            maybeResult <- liftIO $ timeout 1000000 $ return (decode payload)
            let mkMismatchError = VersionMismatchError preciseVehicleVersion version
            case maybeResult of
              Nothing -> return $ mkMismatchError "Decoding timed out"
              Just (Left err) -> return $ mkMismatchError err
              Just (Right v) -> return $ SuccessfulDecoding v
          else case decode payload of
            Left err -> return $ InexplicableDecodingError err
            Right v -> return $ SuccessfulDecoding v

data DecodeResult a
  = IOError IOException
  | InexplicableDecodingError String
  | VersionMismatchError VersionString VersionString String
  | SuccessfulDecoding a

data Versioned a = Versioned
  { version :: VersionString,
    payload :: a
  }
  deriving (Generic)

instance (Serialize a) => Serialize (Versioned a)

-- `Data.Version` tags feature is deprecated so we can't use it.
type VersionString = String
