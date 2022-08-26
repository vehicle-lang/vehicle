module Vehicle.Backend.Marabou.Interact
  ( verifySpec
  , writeSpecFiles
  ) where

import Control.Monad ( forM, forM_ )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (hPutStrLn)
import Data.List (findIndex, elemIndex)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (zip, zipWith)
import Data.Text qualified as Text (pack, splitOn, strip, unpack)
import Data.Map qualified as Map (fromList, lookup)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import System.Directory (createDirectoryIfMissing)
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
import Vehicle.Compile.Resource
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
writePropertyFiles partialAddress = \case
  MultiProperty subproperties -> do
    let numberedSubproperties = zip [0..] subproperties
    forM_ numberedSubproperties $ \(i, p) ->
      writePropertyFiles (addPropertyIndex i <$> partialAddress) p

  SingleProperty _negated queryState -> case queryState of
    Trivial{} -> return ()
    NonTrivial queries -> do
      -- Write out the queries to the new directory
      let numberedQueries = NonEmpty.zip [1..] queries
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

type MonadVerify m =
  ( MonadIO m
  , MonadReader (FilePath, NetworkLocations) m
  )

-- | Uses Marabou to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpec :: FilePath -> NetworkLocations -> MarabouSpec -> IO SpecificationStatus
verifySpec marabouExecutable networkLocations spec = do
  withSystemTempDirectory "marabouSpec" $ \tempDir -> do
    writeSpecFiles (Just tempDir) spec
    results <- forM spec $ \(name, property) -> do
      let partialAddress = basicAddress name tempDir
      result <- runReaderT (verifyProperty partialAddress property) (marabouExecutable, networkLocations)
      return (name, result)
    return $ SpecificationStatus (Map.fromList results)

verifyProperty :: MonadVerify m
               => QueryAddress
               -> MarabouProperty
               -> m PropertyStatus
verifyProperty partialAddress = \case
  MultiProperty subproperties -> do
    let numberedSubprops = zip [0..] subproperties
    MultiPropertyStatus <$> forM numberedSubprops (\(i, p) ->
      verifyProperty (addPropertyIndex i partialAddress) p)

  SingleProperty negated queryState -> SinglePropertyStatus negated <$> case queryState of
    Trivial s -> return $ Trivial s
    NonTrivial queries -> NonTrivial <$> do
      let queryFile i = queryFilePath $ addQueryIndex i partialAddress
      let numberedQueries = NonEmpty.zipWith (\i q -> (queryFile i, q)) [1..] queries
      performQueries numberedQueries

performQueries :: MonadVerify m => NonEmpty (FilePath, MarabouQuery) -> m SatisfiabilityStatus
performQueries (q :| []) = performQuery q
performQueries (q :| r : qs) = do
  result <- performQuery q
  case result of
    SAT{} -> return result
    UnSAT -> performQueries (r :| qs)

performQuery :: MonadVerify m
             => (FilePath, MarabouQuery)
             -> m SatisfiabilityStatus
performQuery (queryFile, MarabouQuery{..}) = do
  (marabouExecutable, networkLocations) <- ask
  liftIO $ do
    networkArg <- prepareNetworkArg networkLocations metaNetwork
    marabouOutput <- readProcessWithExitCode marabouExecutable [networkArg, queryFile] ""
    parseMarabouOutput varReconstruction marabouOutput

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
                   -> IO SatisfiabilityStatus
parseMarabouOutput reconstructionInfo (exitCode, out, _err) = case exitCode of
  ExitFailure _ -> do
    -- Marabou seems to output its error messages to stdout rather than stderr...
    hPutStrLn stderr
      ("Marabou threw the following error:\n" <>
      "  " <> pack out)
    exitFailure

  ExitSuccess -> do
    let outputLines = fmap Text.pack (lines out)
    let resultIndex = findIndex (\v -> v == "sat" || v == "unsat") outputLines
    case resultIndex of
      Nothing -> malformedOutputError "cannot find 'sat' or 'unsat'"
      Just i
        | outputLines !! i == "unsat" ->
          return UnSAT
        | otherwise -> do
          let assignmentOutput = drop (i+1) outputLines
          let ioVarAssignment = parseSATAssignment assignmentOutput
          let maybeLinearVars = reconstructUserVars reconstructionInfo ioVarAssignment
          case maybeLinearVars of
            Nothing -> return $ SAT Nothing
            -- TODO reverse normalisation of quantified user tensor variables
            Just _x -> return $ SAT Nothing

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