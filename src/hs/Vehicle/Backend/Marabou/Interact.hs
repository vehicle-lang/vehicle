module Vehicle.Backend.Marabou.Interact
  ( verifySpec
  , writeSpecFiles
  ) where

import Control.Monad ( forM, forM_ )
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Map qualified as Map (fromList)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>), dropExtension)
import System.Process (readProcess)
import System.IO.Temp (withSystemTempDirectory)

import Vehicle.Backend.Marabou.Core
import Vehicle.Backend.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.Compile.Normalise.NetworkApplications (MetaNetwork)

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
           -> IO SpecificationStatus
verifySpec maybeMarabouExecutable spec = do
  marabouExecutable <- verifyExecutable maybeMarabouExecutable
  withSystemTempDirectory "marabouSpec" $ \tempDir -> do
    writeSpecFiles (Just tempDir) spec
    results <- forM spec (verifyProperty marabouExecutable tempDir)
    return $ SpecificationStatus (Map.fromList results)

verifyProperty :: FilePath
               -> FilePath
               -> MarabouProperty
               -> IO (Text, PropertyStatus)
verifyProperty executable queryDirectory (MarabouProperty propertyName negated queries) = do
  status <- verifyQueries (zip [1..] queries)
  return (propertyName, status)
  where
    verifyQueries :: [(Int, MarabouQuery)] -> IO PropertyStatus
    verifyQueries [] = return (Verified Nothing)
    verifyQueries ((queryID, query) : queryIDs) = do
      let queryFile = queryFilePath propertyName queryID queryDirectory
      result <- verifyQuery executable queryFile negated query
      if isVerified result
        then verifyQueries queryIDs
        else return result

verifyQuery :: FilePath -> FilePath -> Bool -> MarabouQuery -> IO PropertyStatus
verifyQuery marabouExecutable queryFile negated query = do
  let networkArg = prepareNetworkArg (metaNetwork query)
  output <- readProcess marabouExecutable [networkArg, queryFile] ""
  let result = parseMarabouOutput output
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
      -- TODO
      exitFailure

prepareNetworkArg :: MetaNetwork -> String
prepareNetworkArg _ =
  -- TODO
  ""

parseMarabouOutput :: String -> PropertyStatus
parseMarabouOutput _ =
  -- TODO
  Verified Nothing
