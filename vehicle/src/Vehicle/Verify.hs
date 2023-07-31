module Vehicle.Verify
  ( VerifyOptions (..),
    VerifierID,
    verify,
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath (takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Compile (CompileOptions (..), compile)
import Vehicle.Compile.Prelude (DatasetLocations, NetworkLocations, ParameterValues)
import Vehicle.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)

data VerifyOptions = VerifyOptions
  { specification :: FilePath,
    -- Compilation options
    properties :: PropertyNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    cacheLocation :: Maybe FilePath,
    -- Shared options
    verifierID :: VerifierID,
    verifierLocation :: Maybe VerifierExecutable
  }
  deriving (Eq, Show)

verify :: LoggingSettings -> VerifyOptions -> IO ()
verify loggingSettings options@VerifyOptions {..} = do
  validQueryFolder <- isValidQueryFolder specification
  if validQueryFolder
    then do
      let queryFolder = specification
      verifyQueries loggingSettings queryFolder verifierID verifierLocation
    else
      if takeExtension specification == specificationFileExtension
        then compileAndVerifyQueries loggingSettings options
        else do
          fatalError (invalidTargetError specification)

-- | Compiles the specification to a temporary directory and then tries to verify it.
compileAndVerifyQueries :: LoggingSettings -> VerifyOptions -> IO ()
compileAndVerifyQueries loggingSettings VerifyOptions {..} = do
  let queryFormat = VerifierQueries $ verifierQueryFormat $ verifiers verifierID

  let inFolder = case cacheLocation of
        Nothing -> withSystemTempDirectory "specification"
        Just workingFolder -> \f -> f workingFolder

  inFolder $ \tempDir -> do
    compile loggingSettings $
      CompileOptions
        { target = queryFormat,
          specification = specification,
          declarationsToCompile = properties,
          outputFile = Just tempDir,
          moduleName = Nothing,
          cacheLocation = cacheLocation,
          outputAsJSON = False,
          ..
        }

    verifyQueries loggingSettings tempDir verifierID verifierLocation

verifyQueries :: LoggingSettings -> FilePath -> VerifierID -> Maybe FilePath -> IO ()
verifyQueries loggingSettings queryFolder verifierID verifierLocation = do
  let verifierImpl = verifiers verifierID
  verifierExecutable <- locateVerifierExecutable verifierImpl verifierLocation
  runImmediateLogger loggingSettings $
    verifySpecification queryFolder verifierImpl verifierExecutable

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
    exists <- doesFileExist providedLocation
    if exists
      then return providedLocation
      else fatalError (missingVerifierExecutableError verifierIdentifier providedLocation)
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
    <> "Please either provide it using the `--verifierLocation` command line option"
      <+> "or add it to the PATH environment variable."

invalidTargetError :: FilePath -> Doc a
invalidTargetError target =
  "The target"
    <+> quotePretty target
    <+> "is not a valid value. It must either be:"
    <> line
    <> indent
      2
      ( "i) a" <+> pretty specificationFileExtension
          <> line
          <> "ii) a folder containing a"
            <+> pretty specificationCacheIndexFileExtension
            <+> "file generated via a `vehicle compile` command."
      )
