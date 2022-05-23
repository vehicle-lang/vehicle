module Vehicle.Backend.Marabou.Interact
  ( verifySpec
  , writeSpecFiles
  ) where

import Control.Monad ( forM, forM_ )
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (hPutStrLn)
import Data.List (findIndex, elemIndex)
import Data.Text qualified as Text (pack, splitOn, strip, unpack)
import Data.Map qualified as Map (fromList, lookup)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure, ExitCode (..))
import System.FilePath ((<.>), (</>), dropExtension)
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.IO (stderr)

import Vehicle.Backend.Marabou.Core
import Vehicle.Backend.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.Compile.Linearity (reconstructUserVars, UserVarReconstructionInfo)
import Vehicle.Resource
import Vehicle.Resource.NeuralNetwork
import Vehicle.Prelude

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
verifyQuery marabouExecutable queryFile networks MarabouQuery{..} = do
  networkArg <- prepareNetworkArg networks metaNetwork
  marabouOutput <- readProcessWithExitCode marabouExecutable [networkArg, queryFile] ""
  parseMarabouOutput varReconstruction marabouOutput

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

parseMarabouOutput :: UserVarReconstructionInfo
                   -> (ExitCode, String, String)
                   -> IO SinglePropertyStatus
parseMarabouOutput _ (ExitFailure _, out, _err) = do
  -- Marabou seems to output its error messages to stdout rather than stderr...
  hPutStrLn stderr
    ("Marabou threw the following error:\n" <>
    "  " <> pack out)
  exitFailure
parseMarabouOutput reconstructionInfo (ExitSuccess, out, _) = do
  let outputLines = fmap Text.pack (lines out)
  let resultIndex = findIndex (\v -> v == "sat" || v == "unsat") outputLines
  case resultIndex of
    Nothing -> malformedOutputError "cannot find 'sat' or 'unsat'"
    Just i
      | outputLines !! i == "unsat" ->
        return $ Failed Nothing
      | otherwise -> do
        let assignmentOutput = drop (i+1) outputLines
        let ioVarAssignment = parseSATAssignment assignmentOutput
        let maybeLinearVars = reconstructUserVars reconstructionInfo ioVarAssignment
        case maybeLinearVars of
          Nothing -> return $ Failed Nothing
          -- TODO reverse normalisatio of quantified user tensor variables
          Just _x -> return $ Verified Nothing

parseSATAssignment :: [Text] -> Vector Double
parseSATAssignment output =
  let mInputIndex  = elemIndex "Input assignment:" output in
  let mOutputIndex = elemIndex "Output:" output in
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) ->
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output in
      let outputVarLines = drop (outputIndex + 1) output in
      let inputValues  = parseSATAssignmentLine Input  <$> inputVarLines  in
      let outputValues = parseSATAssignmentLine Output <$> outputVarLines in
      Vector.fromList (inputValues <> outputValues)
    _ -> malformedOutputError "could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine :: InputOrOutput -> Text -> Double
parseSATAssignmentLine _ txt =
  let parts = Text.strip <$> Text.splitOn "=" txt in
  case parts of
    [_namePart, valuePart] -> read (Text.unpack valuePart)
    _                      -> malformedOutputError "could not split assignment line on '=' sign"

malformedOutputError :: Doc a -> b
malformedOutputError x =
  error $ layoutAsString ("Unexpected output from Marabou..." <+> x)