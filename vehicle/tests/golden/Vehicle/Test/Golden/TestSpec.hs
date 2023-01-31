module Vehicle.Test.Golden.TestSpec
  ( TestSpecs (..),
    TestSpec (..),
    FilePattern (..),
    parseFilePattern,
    DiffSpec (..),
    DiffSpecIgnore (..),
    TestOutput (..),
    mergeTestSpecs,
    addOrReplaceTestSpec,
    testSpecOptions,
    testSpecIsEnabled,
    testSpecDiffTestOutput,
    readTestSpecsFile,
    writeTestSpecsFile,
    readGoldenFiles,
    writeGoldenFiles,
    encodeTestSpecsPretty,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (assert)
import Control.Monad (forM, join, unless)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.Aeson (FromJSON (parseJSONList), eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty
  ( Config (..),
    defConfig,
    encodePrettyToTextBuilder',
    keyOrder,
  )
import Data.Aeson.Encode.Pretty qualified as Indent (Indent (..))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Object,
    Pair,
    Parser,
    ToJSON (toJSON),
    Value,
    object,
    typeMismatch,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Array qualified as Array ((!))
import Data.Foldable (for_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid (Any (Any))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Builder
import GHC.Stack.Types (HasCallStack)
import System.Directory (doesFileExist)
import System.FilePath
  ( dropExtension,
    makeRelative,
    takeFileName,
    (<.>),
    (</>),
  )
import System.FilePath.Glob (CompOptions (..))
import System.FilePath.Glob qualified as Glob
import Test.Tasty (TestName, Timeout (Timeout))
import Test.Tasty.Options (IsOption (parseValue))
import Text.Printf (printf)
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.Regex.TDFA.Text qualified as Regex
import Vehicle.Test.Golden.Extra
  ( SomeOption (SomeOption),
    boolToMaybe,
    duplicates,
    writeFileChanged,
  )

newtype TestSpecs = TestSpecs (NonEmpty TestSpec)

data TestSpec = TestSpec
  { -- | Test name.
    --   In a file with multiple test specifications, each name must be unique.
    testSpecName :: TestName,
    -- | Test command to run.
    testSpecRun :: String,
    -- | Whether or not the test is enabled.
    --   By default, the test is assumed to be enabled.
    testSpecEnabled :: Maybe Bool,
    -- | Files needed by the test command.
    --   Paths should be relative to the test specification file.
    testSpecNeeds :: [FilePath],
    -- | Files produced by the test command.
    --   Paths should be relative to the test specification file,
    --   and should not contain the .golden file extension.
    testSpecProduces :: [FilePattern],
    -- | Local options for the test.
    testSpecTimeout :: Maybe Timeout,
    -- | Options for the `diff` algorithm.
    testSpecDiffSpec :: Maybe DiffSpec
  }
  deriving (Show)

-- | Type of file patterns.
--
--   Consists of a pair of the original string and the parsed glob pattern,
--   so that we can output the original string in `show` and `toJSON`.
data FilePattern = FilePattern
  { filePatternString :: String,
    filePattern :: Glob.Pattern
  }

parseFilePattern :: String -> Either String FilePattern
parseFilePattern patternString = do
  globPattern <- eitherGlobPattern
  return $ FilePattern patternString globPattern
  where
    eitherGlobPattern = Glob.tryCompileWith compOptions (patternString <.> "golden")
    compOptions =
      CompOptions
        { characterClasses = False,
          characterRanges = False,
          numberRanges = False,
          wildcards = True,
          recursiveWildcards = True,
          pathSepInRanges = False,
          errorRecovery = False
        }

instance Show FilePattern where
  show :: FilePattern -> String
  show FilePattern {..} = show filePatternString

-- | Type of options for the comparison between the produced output and the golden output.
newtype DiffSpec = DiffSpec
  { -- | A regular expression to apply to each line before testing for
    --   equality.
    diffSpecIgnore :: Maybe DiffSpecIgnore
  }
  deriving (Show)

-- | Type of regular expression to ignore in the comparison.
--
--   Consists of a pair of the original string and the parsed regular expression,
--   so that we can output the original string in `show` and `toJSON`.
data DiffSpecIgnore = DiffSpecIgnore
  { diffSpecIgnoreRegexText :: Text,
    diffSpecIgnoreRegex :: Regex
  }

instance Show DiffSpecIgnore where
  show :: DiffSpecIgnore -> String
  show DiffSpecIgnore {..} = show diffSpecIgnoreRegexText

-- | The output of running a test.
--
--   Consists of the contents of the standard output stream, the standard error
--   stream, and the contents of each of the files produced by the test.
data TestOutput = TestOutput
  { -- | Standard output stream produced by a test.
    testOutputStdout :: Text,
    -- | Standard error stream produced by a test.
    testOutputStderr :: Text,
    -- | Files produced by a test.
    --   Should be relative to the test specification file.
    testOutputFiles :: HashMap FilePath Text
  }
  deriving (Show)

testSpecOptions :: TestSpec -> [SomeOption]
testSpecOptions testSpec =
  catMaybes [SomeOption <$> testSpecTimeout testSpec]

-- Helper functions for accessing fields:

-- | Whether or not the test is enabled.
testSpecIsEnabled :: TestSpec -> Bool
testSpecIsEnabled = fromMaybe True . testSpecEnabled

-- | Find the files matching the file patterns in a test specification 'produces' field.
testSpecProducesGlobDir :: FilePath -> TestSpec -> IO [FilePath]
testSpecProducesGlobDir testDirectory testSpec =
  concat <$> Glob.globDir (filePattern <$> testSpecProduces testSpec) testDirectory

-- | Compare two test outputs using the options set in DiffSpec.
testSpecDiffTestOutput :: TestSpec -> TestOutput -> TestOutput -> IO (Maybe String)
testSpecDiffTestOutput testSpec golden actual = do
  let goldenFiles = HashMap.keysSet (testOutputFiles golden)
  let actualFiles = HashMap.keysSet (testOutputFiles actual)
  -- Compute missing files:
  let missingOutputFileErrors =
        [ printf "Missing output file %s" missingFile
          | missingFile <- HashSet.toList $ HashSet.difference goldenFiles actualFiles
        ]
  -- Compute extraneous files:
  let extraOutputFileErrors =
        [ printf "Extraneous output file %s" extraFile
          | extraFile <- HashSet.toList $ HashSet.difference actualFiles goldenFiles
        ]
  -- Compare output & error stream content:
  let differentStdoutError =
        printf "Contents of stdout differ:\n%s"
          <$> testSpecDiffText testSpec (testOutputStdout golden) (testOutputStdout actual)
  let differentStderrError =
        printf "Contents of stderr differ:\n%s"
          <$> testSpecDiffText testSpec (testOutputStderr golden) (testOutputStderr actual)
  -- Compare file content:
  let differentOutputFileErrors =
        catMaybes
          [ printf "Content of %s differs:\n%s" file
              <$> testSpecDiffText
                testSpec
                (testOutputFiles golden HashMap.! file)
                (testOutputFiles actual HashMap.! file)
            | file <- HashSet.toList $ HashSet.intersection goldenFiles actualFiles
          ]
  -- Combine all messages:
  let messages =
        join
          [ maybeToList differentStdoutError,
            maybeToList differentStderrError,
            differentOutputFileErrors,
            missingOutputFileErrors,
            extraOutputFileErrors
          ]
  return $ boolToMaybe (not $ null messages) (unlines messages)

-- | Compare two texts using the options set in DiffSpec.
testSpecDiffText :: TestSpec -> Text -> Text -> Maybe String
testSpecDiffText testSpec golden actual
  | goldenLines == actualLines = Nothing
  | otherwise = return prettyDiff
  where
    goldenLines, actualLines :: [Text]
    goldenLines = Text.lines golden
    actualLines = Text.lines actual

    diffGroups :: [Diff [Text]]
    diffGroups = getGroupedDiffBy (testSpecCompareLine testSpec) goldenLines actualLines

    prettyDiff :: String
    prettyDiff =
      -- ASSERT: we should not be computing the pretty diff unless
      -- there is an actual difference, guarded by the comparison
      assert (not (all isBoth diffGroups)) $
        ppDiff (mapDiff (fmap Text.unpack) <$> diffGroups)

    -- TODO: upstream DiffOutput to work with Text
    isBoth :: Diff a -> Bool
    isBoth (Both _ _) = True
    isBoth _ = False

    mapDiff :: (a -> b) -> Diff a -> Diff b
    mapDiff f (First x) = First (f x)
    mapDiff f (Second y) = Second (f y)
    mapDiff f (Both x y) = Both (f x) (f y)

-- | Compare two lines using the options set in DiffSpec.
testSpecCompareLine :: TestSpec -> Text -> Text -> Bool
testSpecCompareLine testSpec =
  (==) `on` testSpecDiffSpecStrikeOut testSpec

-- | Strike out matches for the DiffSpecIgnore expression.
testSpecDiffSpecStrikeOut :: TestSpec -> Text -> Text
testSpecDiffSpecStrikeOut testSpec = maybe id strikeOut maybeRegex
  where
    maybeRegex = diffSpecIgnoreRegex <$> (diffSpecIgnore =<< testSpecDiffSpec testSpec)

-- | Strike out matches for a regular expression.
strikeOut :: Regex -> Text -> Text
strikeOut re txt = strikeOutAcc (Regex.matchAll re txt) txt []

strikeOutAcc :: [Regex.MatchArray] -> Text -> [Text] -> Text
strikeOutAcc [] txt acc = Text.concat (reverse (txt : acc))
strikeOutAcc (match : matches) txt acc = strikeOutAcc matches rest newAcc
  where
    (matchOffset, matchLength) = match Array.! 0
    (beforeMatch, matchAndAfterMatch) = Text.splitAt matchOffset txt
    (_matchText, rest) = Text.splitAt matchLength matchAndAfterMatch
    newAcc = "[IGNORE]" : beforeMatch : acc

-- Reading and writing test specifications:

-- | Read TestSpecs from a file.
readTestSpecsFile :: FilePath -> IO TestSpecs
readTestSpecsFile testSpecFile = do
  eitherTestSpecs <- eitherDecodeFileStrict' testSpecFile
  let parseError msg = fail $ printf "Could not parse %s: %s" testSpecFile msg
  testSpecs <- either parseError return eitherTestSpecs
  validateTestSpecs testSpecFile testSpecs
  return testSpecs

writeTestSpecsFile :: FilePath -> TestSpecs -> IO ()
writeTestSpecsFile testSpecFile testSpecs = do
  Text.writeFile testSpecFile (encodeTestSpecsPretty testSpecs)

mergeTestSpecs :: HasCallStack => TestSpecs -> TestSpecs -> TestSpecs
mergeTestSpecs (TestSpecs testSpecs1) testSpecs2 =
  foldr addOrReplaceTestSpec testSpecs2 testSpecs1

addOrReplaceTestSpec :: HasCallStack => TestSpec -> TestSpecs -> TestSpecs
addOrReplaceTestSpec newTestSpec (TestSpecs oldTestSpecs)
  | replaced = TestSpecs newTestSpecs
  | otherwise = TestSpecs $ newTestSpec <| oldTestSpecs
  where
    (newTestSpecs, Any replaced) = runWriter (traverse (substByName newTestSpec) oldTestSpecs)

substByName :: TestSpec -> TestSpec -> Writer Any TestSpec
substByName newTestSpec oldTestSpec
  | testSpecName newTestSpec == testSpecName oldTestSpec =
      tell (Any True) >> return newTestSpec
  | otherwise = return oldTestSpec

-- | Check that each test specification has a unique name.
validateTestSpecs :: FilePath -> TestSpecs -> IO ()
validateTestSpecs testSpecFile (TestSpecs testSpecs) = do
  -- Check the filename:
  unless (takeFileName testSpecFile == "test.json") $
    fail $
      printf "Test specification file should be named 'test.json', found: %s" testSpecFile
  -- Check for duplicate testSpecNames:
  let duplicateTestSpecNames =
        duplicates (NonEmpty.toList (testSpecName <$> testSpecs))
  unless (null duplicateTestSpecNames) $
    fail $
      printf "Duplicate names %s in %s" (show duplicateTestSpecNames) testSpecFile

-- Reading and writing .golden files:

-- | Read a golden file, if it exists.
readGoldenFile :: FilePath -> IO Text
readGoldenFile goldenFile = do
  goldenFileExists <- doesFileExist goldenFile
  if goldenFileExists then Text.readFile goldenFile else return ""

goldenStdoutFileName, goldenStderrFileName :: TestSpec -> FilePath
goldenStdoutFileName testSpec = testSpecName testSpec <.> "out" <.> "golden"
goldenStderrFileName testSpec = testSpecName testSpec <.> "err" <.> "golden"

-- | Read the golden files for a test specification.
readGoldenFiles :: FilePath -> TestSpec -> IO TestOutput
readGoldenFiles testDirectory testSpec = do
  testOutputStdout <- readGoldenFile $ testDirectory </> goldenStdoutFileName testSpec
  testOutputStderr <- readGoldenFile $ testDirectory </> goldenStderrFileName testSpec
  testOutputFiles <-
    fmap HashMap.fromList $ do
      goldenFiles <- testSpecProducesGlobDir testDirectory testSpec
      forM goldenFiles $ \goldenFile -> do
        goldenFileContents <- readGoldenFile goldenFile
        -- let relativeFilePath = makeRelative testDirectory $ goldenFile -<.> "golden"
        let relativeFilePath = makeRelative testDirectory goldenFile
        -- Convert the golden file path back to the corresponding output file path.
        let outputFilePath = goldenFileToOutputFile relativeFilePath

        return (outputFilePath, goldenFileContents)
  return TestOutput {..}

goldenFileToOutputFile :: FilePath -> FilePath
goldenFileToOutputFile = dropExtension

writeGoldenFiles :: FilePath -> TestSpec -> TestOutput -> IO ()
writeGoldenFiles testDirectory testSpec testOutput@TestOutput {..} = do
  validateTestSpecProduces testSpec testOutput
  writeFileChanged (testDirectory </> goldenStdoutFileName testSpec) testOutputStdout
  writeFileChanged (testDirectory </> goldenStderrFileName testSpec) testOutputStderr
  for_ (HashMap.toList testOutputFiles) $ \(file, fileContents) -> do
    writeFileChanged (testDirectory </> file <.> "golden") fileContents

-- | Test whether or not the 'produces' patterns still match all the produced files.
validateTestSpecProduces :: TestSpec -> TestOutput -> IO ()
validateTestSpecProduces testSpec testOutput
  | null unmatchFileErrors = return ()
  | otherwise = fail $ unlines unmatchFileErrors
  where
    isMatched filePath =
      let goldenFilePath = filePath <.> "golden"
       in any (`Glob.match` goldenFilePath) (filePattern <$> testSpecProduces testSpec)
    unmatchFileErrors = do
      outputFilePath <- HashMap.keys $ testOutputFiles testOutput
      if isMatched outputFilePath
        then mempty
        else return $ printf "Output file %s is not matched by file patterns in 'produces'" outputFilePath

-- Conversion from TestSpecs to JSON.

instance FromJSON TestSpecs where
  parseJSON :: Value -> Parser TestSpecs
  parseJSON v = TestSpecs <$> (parse1 <|> parseN)
    where
      parse1 = (:| []) <$> parseJSON v
      parseN = parseJSON v

instance ToJSON TestSpecs where
  toJSON :: TestSpecs -> Value
  toJSON (TestSpecs (testSpec :| [])) = toJSON testSpec
  toJSON (TestSpecs testSpecs@(_ :| _)) = toJSON testSpecs

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \o ->
    TestSpec
      <$> o
        .: "name"
      <*> o
        .: "run"
      <*> o
        .:? "enabled"
      <*> o
        .:? "needs"
        .!= []
      <*> produces o
      <*> timeout o
      <*> o
        .:? "ignore"
    where
      produces :: Object -> Parser [FilePattern]
      produces o =
        o .:? "produces" >>= \case
          Nothing -> return []
          Just v -> fmap (: []) (parseJSON v) <|> parseJSONList v
      timeout :: Object -> Parser (Maybe Timeout)
      timeout o = o .:? "timeout" >>= traverse parseJSONTimeout

instance ToJSON TestSpec where
  toJSON :: TestSpec -> Value
  toJSON TestSpec {..} =
    object $
      catMaybes
        [ Just $ "name" .= testSpecName,
          Just $ "run" .= testSpecRun,
          ("enabled" .=) <$> testSpecEnabled,
          -- Include "needs" only if it is non-empty:
          boolToMaybe (not $ null testSpecNeeds) ("needs" .= testSpecNeeds),
          -- Include "produces" only if it is non-empty:
          boolToMaybe (not $ null testSpecProduces) ("produces" .= testSpecProduces),
          -- Include "timeout" only if it is non-empty:
          ("timeout" .=) . timeoutToJSON <$> testSpecTimeout,
          -- Include "diff" only if it is non-empty:
          ("diff" .=) <$> (diffSpecToJSON =<< testSpecDiffSpec)
        ]

instance FromJSON FilePattern where
  parseJSON :: Value -> Parser FilePattern
  parseJSON (Value.String patternText) =
    either fail return $ parseFilePattern (Text.unpack patternText)
  parseJSON v = typeMismatch "String" v

instance ToJSON FilePattern where
  toJSON :: FilePattern -> Value
  toJSON = toJSON . filePatternString

parseJSONTimeout :: Value -> Parser Timeout
parseJSONTimeout (Value.String timeoutText) =
  maybe parseError return (parseValue timeoutString)
  where
    timeoutString = Text.unpack timeoutText
    parseError =
      fail $
        unlines
          [ "Could not parse value of 'timeout'.",
            "Expected a number, optionally followed by a suffix (ms, s, m, h).",
            "Found: " <> timeoutString
          ]
parseJSONTimeout v = typeMismatch "String" v

timeoutToJSON :: Timeout -> Maybe Value
timeoutToJSON (Timeout _ms timeoutString) = return $ Value.String (Text.pack timeoutString)
timeoutToJSON _ = Nothing

instance FromJSON DiffSpec where
  parseJSON :: Value -> Parser DiffSpec
  parseJSON = withObject "diff" $ \v ->
    DiffSpec <$> v .:? "ignore"

diffSpecToJSON :: DiffSpec -> Maybe Value
diffSpecToJSON DiffSpec {..} =
  boolToMaybe (null diffSpecFields) (object diffSpecFields)
  where
    diffSpecFields :: [Pair]
    diffSpecFields = catMaybes [("ignore" .=) . toJSON <$> diffSpecIgnore]

instance FromJSON DiffSpecIgnore where
  parseJSON :: Value -> Parser DiffSpecIgnore
  parseJSON (Value.String regexText) =
    case Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt regexText of
      Left compileError -> fail $ "Failed to parse regular expression 'ignore': " <> compileError
      Right regex -> return $ DiffSpecIgnore regexText regex
  parseJSON v = typeMismatch "String" v

instance ToJSON DiffSpecIgnore where
  toJSON :: DiffSpecIgnore -> Value
  toJSON DiffSpecIgnore {..} = Value.String diffSpecIgnoreRegexText

-- | Encode a TestSpec as JSON using aeson-pretty.
encodeTestSpecsPretty :: TestSpecs -> Text
encodeTestSpecsPretty =
  Lazy.toStrict
    . Builder.toLazyText
    . encodePrettyToTextBuilder'
      defConfig
        { confIndent = Indent.Spaces 2,
          confCompare = keyOrder ["name", "run", "enabled", "needs", "produces", "timeout", "diff"],
          confTrailingNewline = True
        }
