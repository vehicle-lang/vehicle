module Vehicle.Verify
  ( VerifyOptions (..),
    verify,
  )
where

import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Data.List (isInfixOf)
import Data.Version (parseVersion)
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.FilePath (takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Text.ParserCombinators.ReadP (eof, readP_to_S)
import Vehicle.Compile (CompileOptions (..), QueryOptions (..), compile)
import Vehicle.Compile.Prelude (DatasetLocations, NetworkLocations, ParameterValues)
import Vehicle.Prelude
import Vehicle.Prelude.Logging
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core (QueryFormatID)
import Vehicle.Verify.Solver
import Vehicle.Verify.Solver.Marabou (marabouSolver)
import Vehicle.Verify.Solver.Test (testSolver)
import Vehicle.Verify.Solver.VNNLIB (vnnlibSolver)
import Vehicle.Verify.Specification.Execute (verifySpecification)
import Vehicle.Verify.Specification.Execute.Reporting
import Vehicle.Verify.Specification.IO

data VerifyOptions = VerifyOptions
  { specification :: FilePath,
    -- Compilation options
    properties :: [PropertyName],
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    verificationCache :: Maybe FilePath,
    -- Shared options
    solverExecutable :: SolverExecutable,
    solverExtraArgs :: Maybe String,
    noSatPrint :: Bool
  }
  deriving (Eq, Show)

verify :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> VerifyOptions -> IO ()
verify loggingSettings outputAsJSON options@VerifyOptions {..} = do
  solver <- runLoggerT loggingSettings $ locateSolver solverExecutable
  validQueryFolder <- isValidQueryFolder specification
  if validQueryFolder
    then verifyQueries loggingSettings outputAsJSON specification solver solverExtraArgs noSatPrint
    else
      if takeExtension specification /= specificationFileExtension
        then fatalError (invalidTargetError specification)
        else compileAndVerifyQueries loggingSettings outputAsJSON options (solverQueryFormatID solver) $ \folder ->
          verifyQueries loggingSettings outputAsJSON folder solver solverExtraArgs noSatPrint

-- | Compiles the specification to a temporary directory and then tries to verify it.
compileAndVerifyQueries :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> VerifyOptions -> QueryFormatID -> (FilePath -> IO ()) -> IO ()
compileAndVerifyQueries loggingSettings outputAsJSON VerifyOptions {..} queryFormatID verifyCommand = do
  let inFolder = case verificationCache of
        Nothing -> withSystemTempDirectory "specification"
        Just folder -> \f -> f folder

  inFolder $ \tempDir -> do
    compile loggingSettings outputAsJSON $
      QueryTarget $
        QueryOptions
          { queryFormatID = queryFormatID,
            specification = specification,
            declarationsToCompile = properties,
            outputFolder = Just tempDir,
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
  Solver ->
  Maybe String ->
  Bool ->
  IO ()
verifyQueries loggingSettings outputAsJSON queryFolder solver maybeSolverExtraArgs noSatOutputs = do
  let solverExtraArgs = maybe [] words maybeSolverExtraArgs
  let solverSettings = VerificationSettings solver solverExtraArgs queryFolder noSatOutputs
  runLoggerT loggingSettings $ verifySpecification outputAsJSON solverSettings

locateSolver :: (MonadStdIO m, MonadLogger m) => SolverExecutable -> m Solver
locateSolver solverExecutable = do
  -- First try to treat as a path
  solverPath <- do
    absolutePath <- liftIO $ makeAbsolute solverExecutable
    exists <- liftIO $ doesFileExist absolutePath
    if exists
      then return absolutePath
      else do
        maybePath <- liftIO $ findExecutable solverExecutable
        case maybePath of
          Just path -> return path
          Nothing -> fatalError $ unlocatableSolverExecutableError absolutePath

  solver <-
    if "Marabou" `isInfixOf` solverPath
      then return $ marabouSolver solverExecutable
      else
        if "testVerifier" `isInfixOf` solverPath
          then return $ testSolver solverExecutable
          else do
            solverName <- do
              solverNameOutput <- liftIO $ readProcess solverExecutable ["--name"] ""
              return $ takeWhile (/= '\n') solverNameOutput

            solverVersion <- do
              solverVersionOutput <- liftIO $ readProcess solverExecutable ["--version"] ""
              let maybeVersion = readP_to_S (parseVersion <* eof) $ takeWhile (/= '\n') solverVersionOutput
              logDebug MidDetail $ pretty maybeVersion
              case maybeVersion of
                [(version, "")] -> return version
                _ ->
                  fatalError $
                    "There was an error interfacing with the solver" <+> quotePretty solverName
                      <> "."
                      <> line
                      <> "The command"
                        <+> squotes (pretty solverExecutable <+> "--version")
                        <+> "produced"
                        <+> lineIndent (pretty solverVersionOutput)
                      <> line
                      <> "which could not be parsed a valid version."

            return $ vnnlibSolver solverPath solverName solverVersion

  logDebug MinDetail $ "Found solver" <+> squotes (pretty (solverName solver) <> "-" <> pretty (solverVersion solver)) <+> "at" <+> pretty solverPath

  return solver
  where
    unlocatableSolverExecutableError :: FilePath -> Doc a
    unlocatableSolverExecutableError path =
      "Could not locate the solver either at the location:"
        <> lineIndent (pretty path)
        <> line
        <> "or as an executable named" <+> quotePretty solverExecutable <+> "on the system PATH."
        <> line
        <> "Please either provide the path to the solver"
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
            <+> "file generated via a `vehicle compile queries` command."
      )
