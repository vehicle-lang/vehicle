module Vehicle.Verify
  ( VerifyOptions (..),
    VerifierID (..),
    verify,
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.FilePath (takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Compile (CompileOptions (..), compile)
import Vehicle.Compile.Prelude (DatasetLocations, NetworkLocations, ParameterValues)
import Vehicle.Prelude
import Vehicle.Prelude.Logging
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier

data VerifyOptions = VerifyOptions
  { specification :: FilePath,
    -- Compilation options
    properties :: PropertyNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    verificationCache :: Maybe FilePath,
    -- Shared options
    verifierID :: VerifierID,
    verifierLocation :: Maybe VerifierExecutable,
    verifierExtraArgs :: Maybe String
  }
  deriving (Eq, Show)

verify :: (MonadStdIO IO) => LoggingSettings -> VerifyOptions -> IO ()
verify loggingSettings options@VerifyOptions {..} = do
  validQueryFolder <- isValidQueryFolder specification
  if validQueryFolder
    then verifyQueries loggingSettings specification verifierID verifierLocation verifierExtraArgs
    else
      if takeExtension specification /= specificationFileExtension
        then fatalError (invalidTargetError specification)
        else compileAndVerifyQueries loggingSettings options $ \folder ->
          verifyQueries loggingSettings folder verifierID verifierLocation verifierExtraArgs

-- | Compiles the specification to a temporary directory and then tries to verify it.
compileAndVerifyQueries :: (MonadStdIO IO) => LoggingSettings -> VerifyOptions -> (FilePath -> IO ()) -> IO ()
compileAndVerifyQueries loggingSettings VerifyOptions {..} verifyCommand = do
  let queryFormat = VerifierQueries $ verifierQueryFormatID $ verifiers verifierID

  let inFolder = case verificationCache of
        Nothing -> withSystemTempDirectory "specification"
        Just folder -> \f -> f folder

  inFolder $ \tempDir -> do
    compile loggingSettings $
      CompileOptions
        { target = queryFormat,
          specification = specification,
          declarationsToCompile = properties,
          output = Just tempDir,
          moduleName = Nothing,
          verificationCache = verificationCache,
          outputAsJSON = False,
          ..
        }

    verifyCommand tempDir

verifyQueries :: (MonadStdIO IO) => LoggingSettings -> FilePath -> VerifierID -> Maybe VerifierExecutable -> Maybe String -> IO ()
verifyQueries loggingSettings queryFolder verifierID verifierLocation maybeVerifierExtraArgs = do
  -- Create the verification settings
  let verifier = verifiers verifierID
  verifierExecutable <- locateVerifierExecutable verifier verifierLocation
  let verifierExtraArgs = maybe [] words maybeVerifierExtraArgs
  let verifierSettings = VerifierSettings verifier verifierExecutable verifierExtraArgs
  -- Run verification
  runLoggerT loggingSettings $ verifySpecification verifierSettings queryFolder

-- | Tries to locate the executable for the verifier at the provided
-- location and falls back to the PATH variable if none provided. If not
-- found then the program will error.
locateVerifierExecutable ::
  (MonadIO m) =>
  Verifier ->
  Maybe VerifierExecutable ->
  m VerifierExecutable
locateVerifierExecutable Verifier {..} = \case
  Just providedLocation -> liftIO $ do
    absolutePath <- makeAbsolute providedLocation
    exists <- doesFileExist providedLocation
    if exists
      then return absolutePath
      else fatalError (missingVerifierExecutableError verifierID providedLocation)
  Nothing -> do
    maybeLocationOnPath <- liftIO $ findExecutable verifierExecutableName
    case maybeLocationOnPath of
      Just locationOnPath -> return locationOnPath
      Nothing -> fatalError (unlocatableVerifierExecutableError verifierExecutableName)

missingVerifierExecutableError :: VerifierID -> FilePath -> Doc a
missingVerifierExecutableError verifierID location =
  "No"
    <+> pretty verifierID
    <+> "executable found"
    <+> "at the provided location"
    <+> quotePretty location
    <> "."

unlocatableVerifierExecutableError :: String -> Doc a
unlocatableVerifierExecutableError verifierName =
  "Could not locate the executable"
    <+> quotePretty verifierName
    <+> "via the PATH environment variable."
    <> line
    <> "Please either provide it using the `--verifier-location` command line option"
      <+> "or add it to the PATH environment variable."

invalidTargetError :: FilePath -> Doc a
invalidTargetError target =
  "The target"
    <+> quotePretty target
    <+> "is not a valid value. It must either be:"
    <> line
    <> indent
      2
      ( "i) a"
          <+> pretty specificationFileExtension
          <> line
          <> "ii) a folder containing a"
            <+> pretty specificationCacheIndexFileExtension
            <+> "file generated via a `vehicle compile` command."
      )
