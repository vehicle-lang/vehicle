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
import System.IO (stderr)

import Vehicle.Backend.Marabou.Core
import Vehicle.Backend.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.Resource
import Vehicle.Resource.NeuralNetwork

--------------------------------------------------------------------------------
-- Query addresses

-- | Used to create the final filepath for a query.
data QueryAddress = QueryAddress
  { directory       :: FilePath
  , propertyName    :: Text
  , propertyIndices :: [Int]
  , queryIndex      :: Int
  }

basicAddress :: Text -> FilePath -> QueryAddress
basicAddress name directory = QueryAddress
  { directory       = directory
  , propertyName    = name
  , propertyIndices = []
  , queryIndex      = -1
  }

addPropertyIndex :: Int -> QueryAddress -> QueryAddress
addPropertyIndex index QueryAddress{..} = QueryAddress
  { propertyIndices = index : propertyIndices
  , ..
  }

addQueryIndex :: Int -> QueryAddress -> QueryAddress
addQueryIndex index QueryAddress{..} = QueryAddress
  { queryIndex = index
  , ..
  }

--------------------------------------------------------------------------------
-- Writing out query files

writeSpecFiles :: Maybe FilePath -> MarabouSpec -> IO ()
writeSpecFiles filepath properties = do
  -- Create the directory to store the queries
  let directory = fmap dropExtension filepath
  forM_ directory (createDirectoryIfMissing True)
  -- Write out the spec files
  forM_ properties $ \ (name, property) -> do
    let partialAddress = basicAddress name <$> directory
    writePropertyFiles partialAddress property

writePropertyFiles :: Maybe QueryAddress -> MarabouProperty -> IO ()
writePropertyFiles partialAddress (MultiProperty subproperties) = do
  let numberedSubproperties = zip [0..] subproperties
  forM_ numberedSubproperties $ \(i, p) ->
    writePropertyFiles (addPropertyIndex i <$> partialAddress) p
writePropertyFiles partialAddress (SingleProperty _negated queries) = do
  -- Write out the queries to the new directory
  let numberedQueries = zip [1..] queries
  forM_ numberedQueries $ \(i, q) -> do
    writeQueryFile (addQueryIndex i <$> partialAddress) q

writeQueryFile :: Maybe QueryAddress -> MarabouQuery -> IO ()
writeQueryFile address query = do
  let queryFile = fmap queryFilePath address
  writeResultToFile MarabouBackend queryFile (doc query)

queryFilePath :: QueryAddress -> FilePath
queryFilePath QueryAddress{..} =
  directory </>
  unpack propertyName <>
  propertyStr <>
  "-query" <> show queryIndex <.> "txt"
  where
    propertyStr = if null propertyIndices
      then ""
      else concatMap (\v -> "!" <> show v) (reverse propertyIndices)

--------------------------------------------------------------------------------
-- Verification

-- | Uses Marabou to verify the specification. Failure of one property, does
-- not prevent the verification of the other properties.
verifySpec :: Maybe FilePath
           -> MarabouSpec
           -> NetworkLocations
           -> IO SpecificationStatus
verifySpec maybeMarabouExecutable spec networks = do
  marabouExecutable <- verifyExecutable maybeMarabouExecutable
  withSystemTempDirectory "marabouSpec" $ \tempDir -> do
    writeSpecFiles (Just tempDir) spec
    results <- forM spec $ \(name, property) -> do
      let partialAddress = basicAddress name tempDir
      result <- verifyProperty marabouExecutable partialAddress networks property
      return (name, result)
    return $ SpecificationStatus (Map.fromList results)

verifyProperty :: FilePath
               -> QueryAddress
               -> NetworkLocations
               -> MarabouProperty
               -> IO PropertyStatus
verifyProperty executable partialAddress networks (MultiProperty subproperties) = do
  let numberedSubprops = zip [0..] subproperties
  MultiPropertyStatus <$> forM numberedSubprops (\(i, p) ->
    verifyProperty executable (addPropertyIndex i partialAddress) networks p)
verifyProperty executable partialAddress networks (SingleProperty negated queries) = do
  let numberedQueries = zipWith (\i q -> (addQueryIndex i partialAddress, q)) [1..] queries
  status <- verifyQueries numberedQueries
  return (SinglePropertyStatus status)
  where
    verifyQueries :: [(QueryAddress, MarabouQuery)] -> IO SinglePropertyStatus
    verifyQueries [] = return (Verified Nothing)
    verifyQueries  ((address, query) : qs) = do
      let queryFile = queryFilePath address
      result <- verifyQuery executable queryFile networks query

      let result' = if negated
          then negateStatus result
          else result

      if isVerified result'
        then verifyQueries qs
        else return result

verifyQuery :: FilePath
            -> FilePath
            -> NetworkLocations
            -> MarabouQuery
            -> IO SinglePropertyStatus
verifyQuery marabouExecutable queryFile networks query = do
  networkArg <- prepareNetworkArg networks (metaNetwork query)
  marabouOutput <- readProcessWithExitCode marabouExecutable [networkArg, queryFile] ""
  parseMarabouOutput marabouOutput

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
    Just path -> return path
    _ -> do
      hPutStrLn stderr $
        "No file provided for neural network '" <> name <> "'. " <>
        "Please provide it via the '--network' command line option."
      exitFailure
prepareNetworkArg _ _ = do
  hPutStrLn stderr $
    "Marabou currently doesn't support properties that involve" <>
    "multiple neural networks or multiple applications of the same network."
  exitFailure

parseMarabouOutput :: (ExitCode, String, String) -> IO SinglePropertyStatus
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
