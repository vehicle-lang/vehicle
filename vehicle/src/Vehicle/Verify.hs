module Vehicle.Verify
  ( VerifyOptions (..),
    VerifierID (..),
    verify,
  )
where

import Control.Monad.IO.Class (MonadIO (..), liftIO)
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.FilePath (takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Compile (CompileOptions (..), compile)
import Vehicle.Compile.Prelude (DatasetLocations, NetworkLocations, ParameterValues)
import Vehicle.Prelude
import Vehicle.Prelude.Logging
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.Execute (verifySpecification)
import Vehicle.Verify.Specification.Execute.Reporting
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
    verifierExtraArgs :: Maybe String,
    noSatPrint :: Bool
  }
  deriving (Eq, Show)

verify :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> VerifyOptions -> IO ()
verify loggingSettings outputAsJSON options@VerifyOptions {..} = do
  validQueryFolder <- isValidQueryFolder specification
  if validQueryFolder
    then verifyQueries loggingSettings outputAsJSON specification verifierID verifierLocation verifierExtraArgs noSatPrint
    else
      if takeExtension specification /= specificationFileExtension
        then fatalError (invalidTargetError specification)
        else compileAndVerifyQueries loggingSettings outputAsJSON options $ \folder ->
          verifyQueries loggingSettings outputAsJSON folder verifierID verifierLocation verifierExtraArgs noSatPrint

-- | Compiles the specification to a temporary directory and then tries to verify it.
compileAndVerifyQueries :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> VerifyOptions -> (FilePath -> IO ()) -> IO ()
compileAndVerifyQueries loggingSettings outputAsJSON VerifyOptions {..} verifyCommand = do
  let queryFormat = VerifierQueries $ verifierQueryFormatID $ verifiers verifierID

  let inFolder = case verificationCache of
        Nothing -> withSystemTempDirectory "specification"
        Just folder -> \f -> f folder

  inFolder $ \tempDir -> do
    compile loggingSettings outputAsJSON $
      CompileOptions
        { target = queryFormat,
          specification = specification,
          declarationsToCompile = properties,
          output = Just tempDir,
          moduleName = Nothing,
          verificationCache = verificationCache,
          ..
        }

    verifyCommand tempDir

-- | Verifies queries in either human-readable or JSON streaming mode
verifyQueries ::
  (MonadStdIO IO) =>
  LoggingSettings ->
  OutputAsJSON ->
  FilePath ->
  VerifierID ->
  Maybe VerifierExecutable ->
  Maybe String ->
  Bool ->
  IO ()
verifyQueries loggingSettings outputAsJSON queryFolder verifierID verifierLocation maybeVerifierExtraArgs noSatOutputs = do
  let verifier = verifiers verifierID
  verifierExecutable <- locateVerifierExecutable verifier verifierLocation
  let verifierExtraArgs = maybe [] words maybeVerifierExtraArgs
  let verifierSettings = VerificationSettings verifier verifierExecutable verifierExtraArgs queryFolder noSatOutputs
  runLoggerT loggingSettings $ verifySpecification outputAsJSON verifierSettings

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
