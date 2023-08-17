{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative (optional, (<**>))
import Control.Exception (IOException, try)
import Control.Monad (forM_, join, unless, when)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Tagged (Tagged (unTagged))
import Options.Applicative
  ( Parser,
    ParserInfo,
    ParserPrefs (..),
    command,
    defaultPrefs,
    execParserPure,
    flag,
    fullDesc,
    handleParseResult,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    maybeReader,
    metavar,
    option,
    strOption,
  )
import Options.Applicative.Types
  ( Backtracking (..),
  )
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath
  ( equalFilePath,
    isRelative,
    normalise,
    takeDirectory,
    takeExtension,
    (</>),
  )
import Test.Tasty (Timeout)
import Test.Tasty.Golden.Executable (GoldenFilePattern, TestSpec (..), TestSpecs (..), addOrReplaceTestSpec, readTestSpecsFile, writeTestSpecsFile)
import Test.Tasty.Options (IsOption (optionHelp, parseValue), safeRead)
import Text.Printf (printf)
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Backend.Prelude qualified as Backend
import Vehicle.CommandLine (commandLineOptionsParserInfo)
import Vehicle.CommandLine qualified as ModeOptions (ModeOptions (..))
import Vehicle.CommandLine qualified as Vehicle (ModeOptions, Options (..))
import Vehicle.Compile qualified as CompileOptions
  ( datasetLocations,
    networkLocations,
    outputFile,
    specification,
    target,
  )
import Vehicle.Compile qualified as Vehicle (CompileOptions)
import Vehicle.Export qualified as ExportOptions
  ( outputFile,
    target,
    verificationCache,
  )
import Vehicle.Export qualified as Vehicle (ExportOptions)
import Vehicle.Prelude (Pretty (pretty), layoutAsString, specificationFileExtension)
import Vehicle.TypeCheck qualified as TypeCheckOptions (specification)
import Vehicle.TypeCheck qualified as Vehicle
import Vehicle.Validate qualified as ValidateOptions
  ( verificationCache,
  )
import Vehicle.Validate qualified as Vehicle (ValidateOptions)
import Vehicle.Verify qualified as Vehicle (VerifyOptions)
import Vehicle.Verify qualified as VerifyOptions
  ( VerifyOptions (..),
    verifierID,
  )
import Vehicle.Verify.QueryFormat (QueryFormatID (MarabouQueries))

main :: IO ()
main = getArgs >>= newTestSpec

data NewTestSpecOptions = NewTestSpecOptions
  { newTestSpecDryRun :: Bool,
    newTestSpecTestPath :: Maybe FilePath,
    newTestSpecTestTimeout :: Maybe Timeout,
    newTestSpecVehicleOptions :: Vehicle.Options
  }

newTestSpecParserInfo :: ParserInfo NewTestSpecOptions
newTestSpecParserInfo =
  info
    (newTestSpecOptionsParser <**> helper)
    ( fullDesc
        <> header "add-vehicle-test - a utility for adding tests for Vehicle"
    )

newTestSpecOptionsParser :: Parser NewTestSpecOptions
newTestSpecOptionsParser =
  NewTestSpecOptions
    <$> dryRunParser
    <*> testPathParser
    <*> testTimeoutParser
    <*> hsubparser (command "vehicle" commandLineOptionsParserInfo)
  where
    dryRunParser :: Parser Bool
    dryRunParser = flag False True $ long "dry-run"

    testPathParser :: Parser (Maybe FilePath)
    testPathParser =
      optional . strOption $
        long "test-path"
          <> metavar "FILE"
          <> help "If specified, create the path & copy all needed files."

    testTimeoutParser :: Parser (Maybe Timeout)
    testTimeoutParser =
      optional . option (maybeReader parseValue) $
        long "test-timeout"
          <> metavar "TIME"
          <> help (unTagged $ optionHelp @Timeout)

newTestSpec :: [String] -> IO ()
newTestSpec args = do
  -- Parse the command line options:
  NewTestSpecOptions {..} <-
    handleParseResult $
      execParserPure defaultPrefs {prefBacktrack = NoBacktrack} newTestSpecParserInfo args

  -- Get the vehicle arguments:
  let testSpecRun = unwords $ dropWhile (/= "vehicle") args

  -- Get the target, needs, and produces:
  let TestSpecData
        { testSpecDataTarget,
          testSpecDataNeeds,
          testSpecDataProduces
        } = testSpecData newTestSpecVehicleOptions
  testSpecProduces <- maybe (fail "Could not infer 'produces'") return testSpecDataProduces

  -- Validate the 'needs' and 'produces':
  forM_ testSpecDataNeeds $ \testSpecNeed ->
    unless (isRelative testSpecNeed) $
      fail $
        printf "Test needs files at an absolute path: %s\n" testSpecNeed
  forM_ testSpecProduces $ \testSpecProducePattern ->
    unless (isRelative $ show testSpecProducePattern) $
      fail $
        printf "Test produces files at an absolute path: %s\n" (show testSpecProducePattern)

  -- Construct the test specification:
  let theNewTestSpec =
        TestSpec
          { testSpecName = testSpecDataTarget,
            testSpecRun = testSpecRun,
            testSpecEnabled = Nothing,
            testSpecNeeds = testSpecDataNeeds,
            testSpecProduces = testSpecProduces,
            testSpecExternal = [],
            testSpecTimeout = newTestSpecTestTimeout,
            testSpecIgnore = Nothing
          }

  -- Write the test:
  let testDirectory = fromMaybe "." newTestSpecTestPath
  let testSpecsFile = testDirectory </> "test.json"

  -- Copy over the needed files:
  let targetExistsError targetPath =
        fail $ printf "Refusing to overwrite %s\n" targetPath
  forM_ testSpecDataNeeds $ \testSpecNeed -> do
    let testSpecNeedSource = normalise testSpecNeed
    let testSpecNeedTarget = normalise $ testDirectory </> testSpecNeed
    canonicalSource <- canonicalizePath testSpecNeedSource
    canonicalTarget <- canonicalizePath testSpecNeedTarget
    unless (equalFilePath canonicalSource canonicalTarget) $ do
      printf "Copy %s to %s\n" canonicalSource canonicalTarget
      canonicalTargetExists <- doesFileExist canonicalTarget
      if canonicalTargetExists
        then targetExistsError canonicalTarget
        else do
          unless newTestSpecDryRun $ do
            createDirectoryRecursive (takeDirectory canonicalTarget)
            copyFile canonicalSource canonicalTarget

  -- Write or update the test specification:
  testSpecsFileExists <- doesFileExist testSpecsFile
  testSpecs <-
    if not testSpecsFileExists
      then return $ TestSpecs (theNewTestSpec :| [])
      else addOrReplaceTestSpec theNewTestSpec <$> readTestSpecsFile testSpecsFile

  writeTestSpecsFile testSpecsFile testSpecs

-- Inferred 'needs' and 'produces':

data TestSpecData = TestSpecData
  { testSpecDataTarget :: String,
    testSpecDataNeeds :: [FilePath],
    testSpecDataProduces :: Maybe [GoldenFilePattern]
  }
  deriving (Show)

class TestSpecLike a where
  targetName :: a -> String
  targetName = testSpecDataTarget . testSpecData

  needs :: a -> [FilePath]
  needs = testSpecDataNeeds . testSpecData

  produces :: a -> Maybe [GoldenFilePattern]
  produces = testSpecDataProduces . testSpecData

  testSpecData :: a -> TestSpecData
  testSpecData a = TestSpecData (targetName a) (needs a) (produces a)

instance TestSpecLike NewTestSpecOptions where
  testSpecData :: NewTestSpecOptions -> TestSpecData
  testSpecData = testSpecData . newTestSpecVehicleOptions

instance TestSpecLike Vehicle.Options where
  testSpecData :: Vehicle.Options -> TestSpecData
  testSpecData = maybe emptyTestSpecData testSpecData . Vehicle.modeOptions
    where
      emptyTestSpecData = TestSpecData "NoMode" [] (return [])

instance TestSpecLike Vehicle.ModeOptions where
  testSpecData :: Vehicle.ModeOptions -> TestSpecData
  testSpecData = \case
    ModeOptions.Check opts -> testSpecData opts
    ModeOptions.Compile opts -> testSpecData opts
    ModeOptions.Verify opts -> testSpecData opts
    ModeOptions.Export opts -> testSpecData opts
    ModeOptions.Validate opts -> testSpecData opts

instance TestSpecLike Vehicle.TypeCheckOptions where
  targetName :: Vehicle.TypeCheckOptions -> String
  targetName = layoutAsString . pretty . TypeCheckOptions.specification

  needs :: Vehicle.TypeCheckOptions -> [FilePath]
  needs opts =
    [ TypeCheckOptions.specification opts
    ]

  produces :: Vehicle.TypeCheckOptions -> Maybe [GoldenFilePattern]
  produces = const (return [])

instance TestSpecLike Vehicle.CompileOptions where
  targetName :: Vehicle.CompileOptions -> String
  targetName = layoutAsString . pretty . CompileOptions.target

  needs :: Vehicle.CompileOptions -> [FilePath]
  needs opts =
    join
      [ [CompileOptions.specification opts],
        Map.elems (CompileOptions.networkLocations opts),
        Map.elems (CompileOptions.datasetLocations opts)
      ]

  produces :: Vehicle.CompileOptions -> Maybe [GoldenFilePattern]
  produces opts = traverse safeRead filePatternStrings
    where
      outputFile = CompileOptions.outputFile opts
      filePatternStrings =
        case CompileOptions.target opts of
          VerifierQueries MarabouQueries ->
            [outputDir </> "*.txt" | outputDir <- maybeToList outputFile]
          _ -> maybeToList outputFile

instance TestSpecLike Vehicle.VerifyOptions where
  targetName :: Vehicle.VerifyOptions -> String
  targetName = layoutAsString . pretty . VerifyOptions.verifierID

  needs :: Vehicle.VerifyOptions -> [FilePath]
  needs opts
    | takeExtension (VerifyOptions.specification opts) == specificationFileExtension = do
        join
          [ [VerifyOptions.specification opts]
          -- TODO the verification plan also references resources and query files
          ]
    | otherwise =
        join
          [ [VerifyOptions.specification opts],
            Map.elems (VerifyOptions.networkLocations opts),
            Map.elems (VerifyOptions.datasetLocations opts)
          ]

  produces :: Vehicle.VerifyOptions -> Maybe [GoldenFilePattern]
  produces = traverse safeRead . maybeToList . VerifyOptions.verificationCache

instance TestSpecLike Vehicle.ExportOptions where
  targetName :: Vehicle.ExportOptions -> String
  targetName = layoutAsString . pretty . Backend.ITP . ExportOptions.target

  needs :: Vehicle.ExportOptions -> [FilePath]
  needs = (: []) . ExportOptions.verificationCache

  produces :: Vehicle.ExportOptions -> Maybe [GoldenFilePattern]
  produces = traverse safeRead . maybeToList . ExportOptions.outputFile

instance TestSpecLike Vehicle.ValidateOptions where
  targetName :: Vehicle.ValidateOptions -> String
  targetName = const "Check"

  needs :: Vehicle.ValidateOptions -> [FilePath]
  needs = (: []) . ValidateOptions.verificationCache

  produces :: Vehicle.ValidateOptions -> Maybe [GoldenFilePattern]
  produces = const (return [])

-- | Like @createDirectoryIfMissing True@ but faster, as it avoids
--   any work in the common case the directory already exists.
--
--   Taken from shake (BSD-3-Clause):
--   https://hackage.haskell.org/package/shake-0.19.7/docs/src/
--   General.Extra.html#createDirectoryRecursive
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
  x <- try @IOException $ doesDirectoryExist dir
  when (x /= Right True) $ createDirectoryIfMissing True dir
