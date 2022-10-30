{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Vehicle.Test.Golden
  ( makeTestTreesFromFile
  , makeTestTreeFromDirectoryRecursive
  )
  where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (IOException, assert, try)
import Control.Monad (filterM, forM, forM_, join, unless, when)
import Data.Aeson (eitherDecodeFileStrict', eitherDecodeStrict')
import Data.Aeson.Encode.Pretty (Config (confCompare), defConfig, encodePretty',
                                 encodePrettyToTextBuilder,
                                 encodePrettyToTextBuilder', keyOrder)
import Data.Aeson.Types (FromJSON (..), KeyValue ((.=)), Object, Pair, Parser,
                         ToJSON (toJSON), Value, object, withObject, (.!=),
                         (.:), (.:?))
import Data.Aeson.Types qualified as Value
import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiff,
                            getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (asum, for_, traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Traversable (for)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, getModificationTime, listDirectory,
                         removeFile)
import System.FilePath (isExtensionOf, isRelative, joinPath, makeRelative,
                        takeBaseName, takeDirectory, (-<.>), (<.>), (</>))
import System.FilePath.Glob (globDir, globDir1)
import System.IO (IOMode (ReadMode), withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), readCreateProcessWithExitCode,
                       shell)
import Test.Tasty (TestName, TestTree, Timeout (Timeout), localOption,
                   testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Options (IsOption (parseValue))
import Text.Printf (printf)
import Text.Regex.TDFA (MatchArray,
                        RegexOptions (defaultCompOpt, defaultExecOpt))
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.Regex.TDFA.Text qualified as Regex (compile, execute)

-- Type of test specifications:

newtype TestSpecs = TestSpecs (NonEmpty TestSpec)

data TestSpec = TestSpec
  { testSpecName     :: TestName
    -- ^ Test name.
    --   In a file with multiple test specifications, each name must be unique.
  , testSpecEnabled  :: Maybe Bool
    -- ^ Whether or not the test is enabled.
    --   By default, the test is assumed to be enabled.
  , testSpecRun      :: String
    -- ^ Test command to run.
  , testSpecNeeds    :: [Relative FilePath]
    -- ^ Files needed by the test command.
    --   Paths should be relative to the test specification file.
  , testSpecProduces :: [Relative FilePath]
    -- ^ Files produced by the test command.
    --   Paths should be relative to the test specification file,
    --   and should not contain the .golden file extension.
  , testSpecTimeout  :: Maybe Timeout
    -- ^ Local options for the test.
  , testSpecDiffSpec :: Maybe DiffSpec
    -- ^ Options for the `diff` algorithm.
  } deriving (Show)

newtype DiffSpec = DiffSpec
  { diffSpecIgnore :: Maybe DiffSpecIgnore
    -- ^ A regular expression to apply to each line before testing for
    --   equality.
  } deriving (Show)

data DiffSpecIgnore = DiffSpecIgnore
  { diffSpecIgnoreRegexText :: Text
  , diffSpecIgnoreRegex     :: Regex
  }

instance Show DiffSpecIgnore where
  show :: DiffSpecIgnore -> String
  show DiffSpecIgnore{..} = show diffSpecIgnoreRegexText

data TestOutput = TestOutput
  { stdout :: Text
    -- ^ Standard output stream produced by a test.
  , stderr :: Text
    -- ^ Standard error stream produced by a test.
  , files  :: HashMap (Relative FilePath) Text
    -- ^ Files produced by a test.
  } deriving (Show)

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: TestName -> Absolute FilePath -> IO TestTree
makeTestTreeFromDirectoryRecursive testGroupLabel testDirectory = do
  -- List all paths in `testDirectory`
  testDirectoryEntries <- listDirectory testDirectory

  -- Construct test trees for each .test.json file in the current directory:
  testTreesFromHere <-
    return testDirectoryEntries
      -- Filter directory entries to only test specifications
      >>= filterM (isTestSpecFile . (testDirectory </>))
      -- Make test trees
      >>= traverse (\testSpecFileName ->
            let testSpecFile = testDirectory </> testSpecFileName in
                makeTestTreesFromFile (testDirectory </> testSpecFileName))
      <&> concatMap NonEmpty.toList

  -- Construct test trees for all subdirectories:
  testTreesFromFurther <-
    return testDirectoryEntries
      -- Filter directory entries to only test specifications:
      >>= filterM (doesDirectoryExist . (testDirectory </>))
      -- Make test trees for each subdirectory:
      >>= traverse (\subDirectoryName ->
            let testSubDirectory = testDirectory </> subDirectoryName in
              makeTestTreeFromDirectoryRecursive subDirectoryName testSubDirectory)

  -- Combine all test trees:
  return $ testGroup testGroupLabel (testTreesFromHere <> testTreesFromFurther)

-- | Test whether a path refers to an existing test specification file.
isTestSpecFile :: Absolute FilePath -> IO Bool
isTestSpecFile path =
  (".test.json" `isExtensionOf` path &&) <$> doesFileExist path

-- | Read a test specification and return a TestTree.
makeTestTreesFromFile :: Absolute FilePath -> IO (NonEmpty TestTree)
makeTestTreesFromFile testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  return $ toTestTree testSpecFile <$> testSpecs

-- | Read TestSpecs from a file.
readTestSpecsFile :: FilePath -> IO TestSpecs
readTestSpecsFile testSpecFile = do
  eitherTestSpecs <- eitherDecodeFileStrict' testSpecFile
  let parseError msg = fail $ printf "Could not parse %s: %s" testSpecFile msg
  testSpecs <- either parseError return eitherTestSpecs
  validateTestSpecs testSpecFile testSpecs
  return testSpecs

-- | Check that each test specification has a unique name.
validateTestSpecs :: Absolute FilePath -> TestSpecs -> IO ()
validateTestSpecs testSpecFile (TestSpecs testSpecs) = do
  -- Print diffSpecIgnore
  forM_ testSpecs $ \testSpec -> do
    traverse_ Text.putStrLn (diffSpecIgnoreRegexText <$> (diffSpecIgnore =<< testSpecDiffSpec testSpec))
  -- Check for duplicate testSpecNames:
  let duplicateTestSpecNames =
        duplicates (NonEmpty.toList (testSpecName <$> testSpecs))
  unless (null duplicateTestSpecNames) $ fail $
    printf "Duplicate names %s in %s" (show duplicateTestSpecNames) testSpecFile

-- | Whether or not the test is enabled.
isEnabled :: TestSpec -> Bool
isEnabled = fromMaybe True . testSpecEnabled

-- | Convert a test specifications to a test tree.
toTestTree :: Absolute FilePath -> TestSpec -> TestTree
toTestTree testSpecFile testSpec =
  someLocalOptions someTestOptions goldenTestTree
  where
    someTestOptions :: [SomeOption]
    someTestOptions = catMaybes [SomeOption <$> testSpecTimeout testSpec]

    goldenTestTree :: TestTree
    goldenTestTree =
      goldenTest (testSpecName testSpec) readGoldenFiles runTestSpec compareTestOutput updateGoldenFiles

    testDirectory :: Absolute FilePath
    testDirectory = takeDirectory testSpecFile

    goldenStdoutPath, goldenStderrPath :: Relative FilePath
    goldenStdoutPath = testSpecName testSpec <.> "out" <.> "golden"
    goldenStderrPath = testSpecName testSpec <.> "err" <.> "golden"

    readGoldenFile :: Absolute FilePath -> IO Text
    readGoldenFile goldenFile = do
      goldenFileExists <- doesFileExist goldenFile
      if goldenFileExists
        then Text.readFile goldenFile
        else return ""

    readGoldenFiles :: IO TestOutput
    readGoldenFiles = do
      stdout <- readGoldenFile $ testDirectory </> goldenStdoutPath
      stderr <- readGoldenFile $ testDirectory </> goldenStderrPath
      fileList <- forM (testSpecProduces testSpec) $ \filePath -> do
        fileContents <- readGoldenFile (testDirectory </> filePath <.> "golden")
        return (filePath, fileContents)
      let files = HashMap.fromList fileList
      return TestOutput {..}

    runTestSpec :: IO TestOutput
    runTestSpec = do
      -- Create a temporary directory:
      let tempDirectoryNameTemplate = printf "vehicle-test-%s" (testSpecName testSpec)
      withSystemTempDirectory tempDirectoryNameTemplate $ \tempDirectory -> do
        -- Copy over all needed files:
        forM_ (testSpecNeeds testSpec) $ \neededFile -> do
          createDirectoryRecursive (tempDirectory </> takeDirectory neededFile)
          copyFile (testDirectory </> neededFile) (tempDirectory </> neededFile)
        -- Run the command in the specified directory:
        let cmdSpec = (shell $ testSpecRun testSpec) {cwd = Just tempDirectory}
        (exitCode, stdoutString, stderrString)
          <- readCreateProcessWithExitCode cmdSpec ""
        let stdout = Text.pack stdoutString
        let stderr = Text.pack stderrString
        -- List all files in tempDirectory:
        allFiles
          -- NOTE: we filter by `doesFileExist` to check that the entries are FILES
          --       not to check that they exist, as they clearly do
          <- filterM doesFileExist . concat =<< globDir ["*", "**/*"] tempDirectory
          :: IO [Absolute FilePath]
        let outputFiles :: [Relative FilePath]
            outputFiles = do
              absoluteFilePath <- allFiles
              let relativeFilePath = makeRelative tempDirectory absoluteFilePath
              if relativeFilePath `notElem` testSpecNeeds testSpec
                then return relativeFilePath
                else fail $ printf "File %s is an input" relativeFilePath
        outputFilePathsAndContent <-
          forM outputFiles $ \outputFileName -> do
            outputFileContents <- Text.readFile (tempDirectory </> outputFileName)
            return (outputFileName, outputFileContents)
            :: IO (Relative FilePath, Text)
        let files = HashMap.fromList outputFilePathsAndContent
        return TestOutput{..}

    -- Unpack the DiffSpec for use in compareTestOutput:
    maybeDiffSpecIgnoreRegex =
      diffSpecIgnoreRegex <$> (diffSpecIgnore =<< testSpecDiffSpec testSpec)

    compareLine :: Text -> Text -> Bool
    compareLine = case maybeDiffSpecIgnoreRegex of
      Nothing -> (==)
      Just re -> \l1 l2 -> l1 == l2 ||  strikeOut re l1 == strikeOut re l2

    compareTestOutput :: TestOutput -> TestOutput -> IO (Maybe String)
    compareTestOutput golden actual = do
      let goldenFiles = HashMap.keysSet (files golden)
      let actualFiles = HashMap.keysSet (files actual)
      -- Compute missing files:
      let missingFiles = HashSet.difference goldenFiles actualFiles
      let missingMessages :: [String]
          missingMessages =
            [ printf "Missing output file %s" missingFile
            | missingFile <- HashSet.toList missingFiles
            ]
      -- Compute extraneous files:
      let extraFiles = HashSet.difference actualFiles goldenFiles
      let extraMessages :: [String]
          extraMessages =
            [ printf "Extraneous output file %s" extraFile
            | extraFile <- HashSet.toList extraFiles
            ]
      -- Compare file content:
      let stdoutMessage :: Maybe String
          stdoutMessage =
            printf "Contents of stdout differ:\n%s" <$>
              compareText (stdout golden) (stdout actual)
      let stderrMessage :: Maybe String
          stderrMessage =
            printf "Contents of stderr differ:\n%s" <$>
              compareText (stderr golden) (stderr actual)
      let allFiles = HashSet.intersection goldenFiles actualFiles
      let allFilesMessages :: [String]
          allFilesMessages = catMaybes
            [ printf "Content of %s differs:\n%s" file <$>
              compareText
                (files golden HashMap.! file)
                (files actual HashMap.! file)
            | file <- HashSet.toList allFiles
            ]
      -- Combine all messages:
      let allMessages :: [String]
          allMessages = join
            [ missingMessages
            , extraMessages
            , maybeToList stdoutMessage
            , maybeToList stderrMessage
            , allFilesMessages
            ]
      return $
        if null allMessages
          then Nothing
          else Just $ unlines allMessages
      where
        compareText :: Text -> Text -> Maybe String
        compareText golden actual
          | goldenLines == actualLines = Nothing
          | otherwise = return prettyDiff
          where
            goldenLines = Text.lines golden
            actualLines = Text.lines actual
            diffGroups = getGroupedDiffBy compareLine goldenLines actualLines
            prettyDiff =
              -- ASSERT: we should not be computing the pretty diff unless
              -- there is an actual difference, guarded by the comparison
              assert (not (all isBoth diffGroups)) $
                ppDiff (mapDiff (fmap Text.unpack) <$> diffGroups)
            -- TODO: upstream DiffOutput to work with Text
            isBoth :: PolyDiff a b -> Bool
            isBoth (Both _ _) = True
            isBoth _          = False
            mapDiff :: (a -> b) -> Diff a -> Diff b
            mapDiff f (First x)  = First (f x)
            mapDiff f (Second y) = Second (f y)
            mapDiff f (Both x y) = Both (f x) (f y)

    updateTestSpecFile :: TestSpec -> IO ()
    updateTestSpecFile newTestSpec = do
      TestSpecs oldTestSpecs <- readTestSpecsFile testSpecFile
      let replaceOldTestSpec oldTestSpec
            | testSpecName oldTestSpec == testSpecName newTestSpec = newTestSpec
            | otherwise = oldTestSpec
      let newTestSpecs = TestSpecs $ fmap replaceOldTestSpec oldTestSpecs
      writeFileChanged testSpecFile (encodeTestSpecsPretty newTestSpecs)

    updateGoldenFiles :: TestOutput -> IO ()
    updateGoldenFiles TestOutput{..} = do
      -- Update test specification:
      let newTestSpecProduces = HashMap.keys files
      let newTestSpec = testSpec {testSpecProduces = newTestSpecProduces}
      updateTestSpecFile newTestSpec
      -- Update golden files for stdout and stderr:
      writeFileChanged (testDirectory </> goldenStdoutPath) stdout
      writeFileChanged (testDirectory </> goldenStderrPath) stderr
      -- Update other golden files:
      for_ (HashMap.toList files) $ \(path, contents) -> do
        writeFileChanged (testDirectory </> path <.> "golden") contents

-- Conversion from TestSpecs to JSON.

instance FromJSON TestSpecs where
  parseJSON :: Value -> Parser TestSpecs
  parseJSON v = TestSpecs <$> (parse1 <|> parseN)
    where
      parse1 = (:| []) <$> parseJSON v
      parseN = parseJSON v

instance ToJSON TestSpecs where
  toJSON :: TestSpecs -> Value
  toJSON (TestSpecs (testSpec :| []))   = toJSON testSpec
  toJSON (TestSpecs testSpecs@(_ :| _)) = toJSON testSpecs

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \v ->
    TestSpec
      <$> v .: "name"
      <*> v .:? "enabled"
      <*> v .: "run"
      <*> v .:? "needs" .!= []
      <*> v .:? "produces" .!= []
      <*> (traverse parseTimeout =<< v .:? "timeout")
      <*> (traverse parseDiffSpec =<< v .:? "diff")
    where
      parseTimeout :: Text -> Parser Timeout
      parseTimeout txt = maybe parseError return (parseValue str)
        where
          str = Text.unpack txt
          parseError = fail $ unlines
            [
              "Could not parse value of 'timeout'.",
              "Expected a number, optionally followed by a suffix (ms, s, m, h).",
              "Found: " <> str
            ]

      parseDiffSpec :: Value -> Parser DiffSpec
      parseDiffSpec = withObject "Diff" $ \v ->
          DiffSpec
            <$> (traverse parseDiffSpecIgnore =<< v .:? "ignore")

      parseDiffSpecIgnore :: Text -> Parser DiffSpecIgnore
      parseDiffSpecIgnore txt =
        case Regex.compile defaultCompOpt defaultExecOpt txt of
          Left err -> fail $ "Failed to parse regular expression 'ignore': " <> err
          Right re -> return $ DiffSpecIgnore txt re

instance ToJSON TestSpec where
  toJSON :: TestSpec -> Value
  toJSON TestSpec{..} =
    object $ catMaybes [
      Just $ "name" .= testSpecName,
      ("enabled" .=) <$> testSpecEnabled,
      Just $ "run" .= testSpecRun,
      -- Include "needs" only if it is non-empty:
      if null testSpecNeeds
        then Nothing
        else Just ("needs" .= testSpecNeeds),
      -- Include "produces" only if it is non-empty:
      if null testSpecProduces
        then Nothing
        else Just ("produces" .= testSpecProduces),
      -- Include "timeout" only if it is non-empty:
      case testSpecTimeout of
        Just (Timeout _ms timeout) -> Just $ "timeout" .= timeout
        _                          -> Nothing,
      -- Include "diff" only if it is non-empty:
      ("diff" .=) <$> (diffSpecToJSON =<< testSpecDiffSpec)
    ]

diffSpecToJSON :: DiffSpec -> Maybe Value
diffSpecToJSON DiffSpec {..}
  | null diffSpecFields = Nothing
  | otherwise = Just $ object diffSpecFields
  where
    diffSpecFields :: [Pair]
    diffSpecFields = catMaybes [("ignore" .=) . toJSON <$> diffSpecIgnore]

instance ToJSON DiffSpecIgnore where
  toJSON :: DiffSpecIgnore -> Value
  toJSON DiffSpecIgnore{..} = Value.String diffSpecIgnoreRegexText

-- | Type-level tag to mark absolute FilePath types.
type Absolute a = a

-- | Type-level tag to mark relative FilePath types.
type Relative a = a

-- | Dynamic type for test options.
data SomeOption = forall v. IsOption v => SomeOption v

someLocalOptions :: [SomeOption] -> TestTree -> TestTree
someLocalOptions someOptions testTree =
  foldr (\(SomeOption v) -> localOption v) testTree someOptions


strikeOut :: Regex -> Text -> Text
strikeOut re txt = strikeOutAcc (Regex.matchAll re txt) txt []
  where
    strikeOutAcc :: [MatchArray] -> Text -> [Text] -> Text
    strikeOutAcc []                txt acc = Text.concat (reverse (txt : acc))
    strikeOutAcc (match : matches) txt acc = strikeOutAcc matches rest newAcc
      where
        newAcc = "[IGNORE]" : before : acc
        (before, matchTextAndRest) = Text.splitAt offset txt
        (_matchText, rest) = Text.splitAt length matchTextAndRest
        (offset, length) = match ! 0

-- | Encode a TestSpec as JSON using aeson-pretty.
encodeTestSpecsPretty :: TestSpecs -> Text
encodeTestSpecsPretty =
  Lazy.toStrict . Builder.toLazyText . encodePrettyToTextBuilder

-- | Write a file, but only if the contents would change.
--
--   Adapted from shake (BSD-3-Clause):
--   https://hackage.haskell.org/package/shake-0.19.7/docs/src/
--   Development.Shake.Internal.Derived.html#writeFileChanged
writeFileChanged :: FilePath -> Text -> IO ()
writeFileChanged name x = do
  createDirectoryRecursive $ takeDirectory name
  b <- doesFileExist name
  if not b then Text.writeFile name x else do
    -- Cannot use ByteString here, since it has different line handling
    -- semantics on Windows
    b <- withFile name ReadMode $ \h -> do
        src <- Text.hGetContents h
        pure $! src /= x
    when b $ do
      removeFile name -- symlink safety
      Text.writeFile name x

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

-- | Find the duplicate elements in a list:
duplicates :: (Eq a, Hashable a) => [a] -> [a]
duplicates = duplicatesAcc HashSet.empty
  where
    duplicatesAcc seen [] = []
    duplicatesAcc seen (x : xs)
      | x `HashSet.member` seen = x : duplicatesAcc seen xs
      | otherwise = duplicatesAcc (HashSet.insert x seen) xs
