module Vehicle.Backend.Marabou.Interact
  ( verifySpec
  , writeSpecFiles
  ) where

import Control.Monad ( forM, forM_ )
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (hPutStrLn)
import Data.Map qualified as Map (fromList, lookup)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure, ExitCode (..))
import System.FilePath ((<.>), (</>), dropExtension)
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import GHC.IO.StdHandles

import Vehicle.Backend.Marabou.Core
import Vehicle.Backend.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.NeuralNetwork

--------------------------------------------------------------------------------
-- Writing out query files

writeSpecFiles :: Maybe FilePath -> MarabouSpec -> IO ()
writeSpecFiles filepath properties = forM_ properties $ writePropertyFiles filepath

writePropertyFiles :: Maybe FilePath -> MarabouProperty -> IO ()
writePropertyFiles filepath (MarabouProperty name _negated queries) = do
  -- Create the directory to store the queries
  let directory = fmap dropExtension filepath
  forM_ directory (createDirectoryIfMissing True)

  -- Write out the queries to the new directory
  let numberedQueries = zip [1..] queries
  forM_ numberedQueries (writeQueryFile directory name)

writeQueryFile :: Maybe FilePath -> Text -> (Int, MarabouQuery) -> IO ()
writeQueryFile directory name (queryID, query) = do
  let queryFilepath = fmap (queryFilePath name queryID) directory
  writeResultToFile MarabouBackend queryFilepath (doc query)

queryFilePath :: Text -> Int -> FilePath -> FilePath
queryFilePath propertyName queryID directory =
  directory </> unpack propertyName <> "-query" <> show queryID <.> "txt"

--------------------------------------------------------------------------------
-- Verification

-- | Uses Marabou to verify the specification. Failure of one property, does
-- not prevent the verification of the other properties.
verifySpec :: Maybe FilePath
           -> MarabouSpec
           -> NetworkLocations
           -> IO SpecificationStatus
verifySpec maybeMarabouExecutable spec networkLocations = do
  marabouExecutable <- verifyExecutable maybeMarabouExecutable
  withSystemTempDirectory "marabouSpec" $ \tempDir -> do
    writeSpecFiles (Just tempDir) spec
    results <- forM spec (verifyProperty marabouExecutable tempDir networkLocations)
    return $ SpecificationStatus (Map.fromList results)

verifyProperty :: FilePath
               -> FilePath
               -> NetworkLocations
               -> MarabouProperty
               -> IO (Text, PropertyStatus)
verifyProperty executable queryDirectory networkLocations (MarabouProperty propertyName negated queries) = do
  status <- verifyQueries (zip [1..] queries)
  return (propertyName, status)
  where
    verifyQueries :: [(Int, MarabouQuery)] -> IO PropertyStatus
    verifyQueries [] = return (Verified Nothing)
    verifyQueries ((queryID, query) : queryIDs) = do
      let queryFile = queryFilePath propertyName queryID queryDirectory
      result <- verifyQuery executable queryFile networkLocations negated query
      if isVerified result
        then verifyQueries queryIDs
        else return result

verifyQuery :: FilePath -> FilePath -> NetworkLocations -> Bool -> MarabouQuery -> IO PropertyStatus
verifyQuery marabouExecutable queryFile networkLocations negated query = do
  networkArg <- prepareNetworkArg networkLocations (metaNetwork query)
  marabouOutput <- readProcessWithExitCode marabouExecutable [networkArg, queryFile] ""
  result <- parseMarabouOutput marabouOutput
  return $ if negated
    then negateStatus result
    else result

verifyExecutable :: Maybe FilePath -> IO FilePath
verifyExecutable maybeLocation = do
  let location = fromMaybe "bin/Marabou" maybeLocation
  exists <- doesFileExist location
  if exists
    then return location
    else do
      hPutStrLn stderr $
        "Could not locate the Marabou executable at " <> pack location <> "'." <>
        (if isNothing maybeLocation
          then " Run 'cabal run build init' to install locally."
          else "")
      exitFailure

prepareNetworkArg :: NetworkLocations -> MetaNetwork -> IO String
prepareNetworkArg networkLocations [name] =
  case Map.lookup name networkLocations of
    Nothing -> do
      hPutStrLn stderr $
        "No file provided for neural network '" <> name <> "'. " <>
        "Please provide it via the '--network' command line option."
      exitFailure
    Just path -> return path
prepareNetworkArg _ _ = do
  hPutStrLn stderr $
    "Marabou currently doesn't support properties that involve" <>
    "multiple neural networks or multiple applications of the same network."
  exitFailure

parseMarabouOutput :: (ExitCode, String, String) -> IO PropertyStatus
parseMarabouOutput (ExitFailure _, out, _err) = do
  -- Marabou seems to output its error messages to stdout rather than stderr...
  hPutStrLn stderr
    ("Marabou threw the following error:\n" <>
    "  " <> pack out)
  exitFailure
parseMarabouOutput (ExitSuccess, out, _) = do
  let outputLines = lines out
  if null outputLines
    then do
      hPutStrLn stderr "No output from Marabou..."
      exitFailure
    else if last outputLines == "unsat"
      then return $ Failed   Nothing
      else return $ Verified Nothing
