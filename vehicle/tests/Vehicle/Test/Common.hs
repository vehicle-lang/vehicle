module Vehicle.Test.Common where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (assert)
import Control.Monad (filterM, forM, forM_, when)
import Data.Aeson (FromJSON (..), Object, Value, decodeFileStrict', withArray,
                   withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, (.!=))
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable (for)
import System.Directory (copyFile, doesFileExist, doesPathExist, listDirectory,
                         removeFile)
import System.FilePath (isExtensionOf, makeRelative, takeFileName, (-<.>),
                        (<.>), (</>))
import System.FilePath.Posix (takeBaseName, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), readCreateProcessWithExitCode,
                       shell)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden.Advanced (goldenTest, goldenTest2)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Vehicle.Backend.Prelude (Backend (..), pattern AgdaBackend,
                                pattern MarabouBackend)

type TestConfig        = GenericTestConfig Identity
type PartialTestConfig = GenericTestConfig Maybe
type TestConfigInferIO = GenericTestConfig IO

data GenericTestConfig (m :: Type -> Type) = GenericTestConfig
  { testDirectoryM     :: m FilePath
    -- ^ Directory containing the golden test.
  , testNameM          :: m TestName
    -- ^ Name of the golden test.
    --   Defaults to the basename of the only Vehicle file.
  , testSpecificationM :: m FilePath
    -- ^ Name of input Vehicle file.
    --   Defaults to the only Vehicle file.
  , testTargetsM       :: m [Backend]
    -- ^ Targets with which to test.
    --   Defaults to all targets for which output files exist.
  , testGoldenOutM     :: m FilePath
    -- ^ Golden file for the standard output stream.
    --   Defaults to the empty string.
  , testGoldenErrM     :: m FilePath
    -- ^ Golden file for the standard output stream.
    --   Defaults to the empty string.
  , testGoldenFilesM   :: m [FilePath]
    -- ^ Golden files for the files generated for each backend.
    --   See `inferTestGoldenTargetOut` for defaults.
  , testEnabledM       :: m Bool
    -- ^ Whether the test is enabled.
    --   Defaults to True.
  }

data TestOutput = TestOutput
  { stdout      :: String
  , stderr      :: String
  , outputFiles :: [(FilePath, String)]
  }
  deriving Eq

instance Semigroup TestOutput where
  (<>) :: TestOutput -> TestOutput -> TestOutput
  TestOutput o1 e1 f1 <> TestOutput o2 e2 f2
    = TestOutput (o1 <> o2) (e1 <> e2) (f1 <> f2)

instance Monoid TestOutput where
  mempty :: TestOutput
  mempty = TestOutput mempty mempty mempty

-- TODO: copy over comparison using diff
compareTestOutput :: TestOutput -> TestOutput -> IO (Maybe String)
compareTestOutput to1 to2
  | to1 == to2 = return (Just "WOOPSIE")
  | otherwise  = return Nothing

makeTest :: TestConfig -> TestTree
makeTest TestConfig{..} =
  goldenTest testName readGoldenFiles runTest compareTestOutput updateGolden
  where
    runTest :: IO TestOutput
    runTest = mconcat <$> runTests
      where
        runTests :: IO [TestOutput]
        runTests = do
          forM testTargets $ \testTarget -> do
            let tempDirTemplate = printf "vehicle-tests-%s-%s" testName (show testTarget)
            withSystemTempDirectory tempDirTemplate $ \tempDir -> do
              -- Copy resources:
              let testSpecificationName = takeFileName testSpecification
              copyFile testSpecification (tempDir </> testSpecificationName)
              -- Run command:
              let command = printf "vehicle -s %s -t %s -o %s" testSpecification (show testTarget) testName
              let createProcess = (shell command) { cwd = Just testDirectory }
              let stdin = ""
              (exitCode, stdout, stderr) <- readCreateProcessWithExitCode createProcess stdin
              -- TODO: test for extraneous files
              allFiles <- listDirectory tempDir
              let outputFileNames = [file | file <- allFiles, file /= testSpecificationName]
              outputFiles <-
                forM outputFileNames $ \outputFileName -> do
                  outputFileContents <- readFile (tempDir </> outputFileName)
                  return (outputFileName, outputFileContents)
              return TestOutput{..}

    readGoldenFiles :: IO TestOutput
    readGoldenFiles = do
      stdout <- readFile testGoldenOut
      stderr <- readFile testGoldenErr
      outputFiles <-
        forM testGoldenFiles $ \testGoldenFile -> do
          let testGoldenFileRelative = makeRelative testDirectory testGoldenFile
          fileContents <- readFile testGoldenFile
          return (testGoldenFileRelative, fileContents)
      return TestOutput{..}

    updateGolden :: TestOutput -> IO ()
    updateGolden TestOutput{..} = do
      writeFile testGoldenOut stdout
      writeFile testGoldenErr stdout
      outputFileNames <-
        forM outputFiles $ \(outputFileName, outputFileContents) -> do
          writeFile (testDirectory </> outputFileName) outputFileContents
          return (testDirectory </> outputFileName)
      forM_ testGoldenFiles $ \testGoldenFile ->
        when (testGoldenFile `notElem` outputFileNames) $
          removeFile testGoldenFile

-- Construct a test configuration (if the directory holds a test):
makeTestConfig :: FilePath -> IO (Maybe TestConfig)
makeTestConfig testDirectory = do
  partialConfig <- readPartialTestConfig testDirectory
  inferDefaults testDirectory partialConfig

-- Read a partial test configuration from a `test.json` file:
readPartialTestConfig :: FilePath -> IO (Maybe PartialTestConfig)
readPartialTestConfig testDirectory = do
  maybeConfig <- decodeFileStrict' (testDirectory </> "test.json")
  return $ fmap (\config -> config {testDirectoryM = Just testDirectory}) maybeConfig

-- Infer default configuration settings:
inferDefaults :: FilePath -> Maybe PartialTestConfig -> IO (Maybe TestConfig)
inferDefaults testDirectory partialConfig =
  assert (maybe True (== testDirectory) $ partialConfig >>= testDirectoryM) $ do

    -- Determine the test name:
    let testName = (partialConfig >>= testNameM) `orDefault` takeBaseName testDirectory

    -- Determine the test specification file:
    maybeTestSpecification <-
      (partialConfig >>= Just . testSpecificationM)
        `orInferViaIO` inferTestSpecification testDirectory testName

    -- Every test must have either a test command or a specification file:
    traverse (withSpecification testDirectory testName partialConfig) maybeTestSpecification

withSpecification :: FilePath -> TestName -> Maybe PartialTestConfig -> FilePath -> IO TestConfig
withSpecification testDirectory testName partialConfig testSpecification = do
  -- Determine the golden file for the stdout stream:
  let testGoldenOut =
        (partialConfig >>= testGoldenOutM)
          `orDefault` defaultGoldenOutPath testDirectory testName
  -- Determine the golden file for the stderr stream:
  let testGoldenErr =
        (partialConfig >>= testGoldenErrM)
          `orDefault` defaultGoldenErrPath testDirectory testName
  -- Determine the golden file for each possible target:
  goldenFilesPerTarget
    <- inferTestGoldenFilesPerTarget testDirectory testName
  -- Determine the targets as each target for which there are golden files:
  let testTargets =
        (partialConfig >>= testTargetsM)
          `orDefault` map fst (filter (not . null . snd) goldenFilesPerTarget)
  let testGoldenFiles =
        (partialConfig >>= testGoldenFilesM)
          `orDefault` concatMap snd goldenFilesPerTarget
  -- Set the default test command:
  let testCommands = defaultTestCommands testName testSpecification testTargets
  -- And check whether or not the test is enabled:
  let testEnabled = (partialConfig >>= testEnabledM) `orDefault` True
  return TestConfig{..}

orInferViaIO :: Monad m => Maybe a -> m a -> m a
orInferViaIO value defaultIO = maybe defaultIO return value

orDefault :: Maybe a -> a -> a
orDefault = flip fromMaybe

defaultSpecificationPath :: FilePath -> FilePath -> FilePath
defaultSpecificationPath testPath testName = testPath </> testName <.> "vcl"

defaultGoldenOutPath :: FilePath -> FilePath -> FilePath
defaultGoldenOutPath testPath testName = testPath </> testName <.> "out"

defaultGoldenErrPath :: FilePath -> FilePath -> FilePath
defaultGoldenErrPath testPath testName = testPath </> testName <.> "err"

defaultTestCommands :: TestName -> FilePath -> [Backend] -> [String]
defaultTestCommands testName testSpecification testTargets =
  for testTargets $ \testTarget ->
    printf "vehicle -s %s -t %s -o %s" testSpecification (show testTarget) testName

inferTestSpecification :: FilePath -> TestName -> IO (Maybe FilePath)
inferTestSpecification testDirectory testName = do
  let specificationPath = testDirectory </> testName <.> "vcl"
  doesFileExist specificationPath >>= \case
     True  -> return (Just specificationPath)
     False -> return Nothing

allTestTargets :: [Backend]
allTestTargets = [AgdaBackend, LossFunction, MarabouBackend]

inferTestGoldenFilesPerTarget :: FilePath -> TestName -> IO [(Backend, [FilePath])]
inferTestGoldenFilesPerTarget testPath testName =
  mapM (\backend -> (backend,) <$> forTestTarget backend) allTestTargets
  where
    forTestTarget = \case
      AgdaBackend    -> let agdaFile = testPath </> testName <.> "agda" in
                        filterM doesFileExist [agdaFile]
      LossFunction   -> let jsonFile = testPath </> testName <.> "json" in
                        filterM doesFileExist [jsonFile]
      MarabouBackend -> let inputQueryDir = testPath </> testName <.> "inputquery" in
                        concatMapM listDirectory =<< filterM doesPathExist [inputQueryDir]
      TypeCheck      -> return []
      backend        -> error $ "Unsupported backend: " <> show backend

    concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = concat <$> mapM f xs


-- Helper for parsing test.json files:

instance FromJSON PartialTestConfig where
    parseJSON :: Value -> Parser PartialTestConfig
    parseJSON = withObject "TestConfig" $ \v ->
      GenericTestConfig Nothing
        <$> v .:? "name"
        <*> v .:? "specification"
        <*> parseTargets v
        <*> v .:? "stdout"
        <*> v .:? "stderr"
        <*> v .:? "target-files"
        <*> v .:? "enabled"
      where
        parseTargets :: Object -> Parser (Maybe [Backend])
        parseTargets o = do
          maybeBackendNames <- o .:? "targets" :: Parser (Maybe [Value])
          traverse (traverse parseBackend) maybeBackendNames
          where
            parseBackend :: Value -> Parser Backend
            parseBackend v =
              case v of
                Value.String txt ->
                  case readMaybe (Text.unpack txt) of
                    Nothing      -> fail unsupportedBackend
                    Just backend -> return backend
                _ -> fail unsupportedBackend
              where
                unsupportedBackend = "Unsupported backend " <> show v

-- Pattern synonym for total TestConfig

pattern TestConfig ::
  FilePath ->
  TestName ->
  FilePath ->
  [Backend] ->
  FilePath ->
  FilePath ->
  [FilePath] ->
  Bool ->
  TestConfig
pattern TestConfig
  { testDirectory
  , testName
  , testSpecification
  , testTargets
  , testGoldenOut
  , testGoldenErr
  , testGoldenFiles
  , testEnabled
  } = GenericTestConfig
      (Identity testDirectory)
      (Identity testName)
      (Identity testSpecification)
      (Identity testTargets)
      (Identity testGoldenOut)
      (Identity testGoldenErr)
      (Identity testGoldenFiles)
      (Identity testEnabled)

{-# COMPLETE TestConfig #-}
