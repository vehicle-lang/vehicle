module Vehicle.Test.Golden
  ( readTestTree
  )
  where

import Control.Exception (IOException, assert, try)
import Control.Monad (forM, forM_, join, when)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Types (FromJSON (..), Object, Parser, Value, withObject,
                         (.!=), (.:), (.:?))
import Data.Aeson.Types qualified as Value
import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiff,
                            getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Array ((!))
import Data.Foldable (asum, for_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (foldl')
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, getModificationTime, listDirectory,
                         removeFile)
import System.FilePath (isRelative, makeRelative, takeDirectory, (-<.>), (<.>),
                        (</>))
import System.FilePath.Glob (globDir)
import System.IO (IOMode (ReadMode), withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), readCreateProcessWithExitCode,
                       shell)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Printf (printf)
import Text.Regex.TDFA (MatchArray,
                        RegexOptions (defaultCompOpt, defaultExecOpt))
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.Regex.TDFA.Text qualified as Regex (compile, execute)

type Absolute a = a
type Relative a = a

data TestSpec = TestSpec
  { testSpecName     :: TestName
  , testSpecRun      :: String
  , testSpecNeeds    :: [Relative FilePath]
  , testSpecProduces :: [Relative FilePath]
  , testSpecDiffSpec :: DiffSpec
  } deriving (Show)

readTestSpec :: FilePath -> IO TestSpec
readTestSpec file = do
  eitherErrorOrTestSpec <- eitherDecodeFileStrict' file
  either
    (fail . printf "Could not parse %s: %s" file)
    return
    eitherErrorOrTestSpec

newtype DiffSpec = DiffSpec
  { diffSpecIgnore :: Maybe (Text, Regex)
  }

instance Show DiffSpec where
  show :: DiffSpec -> String
  show DiffSpec{diffSpecIgnore = Nothing} = "DiffSpec {diffSpecIgnore = Nothing}"
  show DiffSpec{diffSpecIgnore = Just (txt, re)} =
    printf "DiffSpec {diffSpecIgnore = Just %s}" (show txt)

emptyDiffSpec :: DiffSpec
emptyDiffSpec = DiffSpec
  { diffSpecIgnore = Nothing
  }

data TestOutput = TestOutput
  { stdout :: Text
  , stderr :: Text
  , files  :: HashMap (Relative FilePath) Text
  } deriving (Show)

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \v ->
    TestSpec
      <$> v .: "name"
      <*> v .: "run"
      <*> v .:? "needs" .!= []
      <*> v .:? "produces" .!= []
      <*> (traverse parseDiffSpec =<< v .:? "diff") .!= emptyDiffSpec
    where
      parseDiffSpec :: Value -> Parser DiffSpec
      parseDiffSpec = withObject "Diff" $ \v ->
          DiffSpec
            <$> (traverse parseDiffSpecIgnore =<< v .:? "ignore")

      parseDiffSpecIgnore :: Text -> Parser (Text, Regex)
      parseDiffSpecIgnore txt =
        case Regex.compile defaultCompOpt defaultExecOpt txt of
          Left err -> fail $ "Failed to parse regular expression 'ignore': " <> err
          Right re -> return (txt, re)

readTestTree :: Absolute FilePath -> IO TestTree
readTestTree testSpecFilePath = do
  testSpec <- readTestSpec testSpecFilePath
  print testSpec
  let testDirectory = takeDirectory testSpecFilePath
  return $ makeTestTree testDirectory testSpec

makeTestTree :: Absolute FilePath -> TestSpec -> TestTree
makeTestTree testDirectory testSpec@TestSpec{..} =
  goldenTest testSpecName readGoldenFiles runTestSpec compareTestOutput updateGoldenFiles
  where
    goldenStdoutPath, goldenStderrPath :: Relative FilePath
    goldenStdoutPath = testSpecName <.> "out" <.> "golden"
    goldenStderrPath = testSpecName <.> "err" <.> "golden"

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
      fileList <- forM testSpecProduces $ \filePath -> do
        fileContents <- readGoldenFile (testDirectory </> filePath <.> "golden")
        return (filePath, fileContents)
      let files = HashMap.fromList fileList
      return TestOutput {..}

    runTestSpec :: IO TestOutput
    runTestSpec = do
      -- Create a temporary directory:
      let tempDirectoryNameTemplate = printf "vehicle-test-%s" testSpecName
      withSystemTempDirectory tempDirectoryNameTemplate $ \tempDirectory -> do
        -- Copy over all needed files:
        forM_ testSpecNeeds $ \neededFile -> do
          createDirectoryRecursive (tempDirectory </> takeDirectory neededFile)
          copyFile (testDirectory </> neededFile) (tempDirectory </> neededFile)
        -- Run the command in the specified directory:
        let cmdSpec = (shell testSpecRun) {cwd = Just tempDirectory}
        (exitCode, stdoutString, stderrString)
          <- readCreateProcessWithExitCode cmdSpec ""
        let stdout = Text.pack stdoutString
        let stderr = Text.pack stderrString
        -- List all files in tempDirectory:
        allFiles
          <- concat <$> globDir ["*", "**/*"] tempDirectory
          :: IO [Absolute FilePath]
        let outputFiles :: [Relative FilePath]
            outputFiles = do
              absoluteFilePath <- allFiles
              let relativeFilePath = makeRelative tempDirectory absoluteFilePath
              if relativeFilePath `notElem` testSpecNeeds
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
    DiffSpec{..} = testSpecDiffSpec

    compareLine :: Text -> Text -> Bool
    compareLine = case diffSpecIgnore of
      Nothing -> (==)
      Just (_txt, re) -> (==) `on` strikeOut re

    strikeOut :: Regex -> Text -> Text
    strikeOut re txt = strikeOutAcc (Regex.matchAll re txt) txt []
      where
        strikeOutAcc :: [MatchArray] -> Text -> [Text] -> Text
        strikeOutAcc []                txt acc = Text.concat (reverse (txt : acc))
        strikeOutAcc (match : matches) txt acc = strikeOutAcc matches rest newAcc
          where
            newAcc = Text.replicate length "_" : before : acc
            (before, matchTextAndRest) = Text.splitAt offset txt
            (_matchText, rest) = Text.splitAt length matchTextAndRest
            (offset, length) = match ! 0

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

    updateGoldenFiles :: TestOutput -> IO ()
    updateGoldenFiles TestOutput{..} = do
      writeFileChanged (testDirectory </> goldenStdoutPath) stdout
      writeFileChanged (testDirectory </> goldenStderrPath) stderr
      for_ (HashMap.toList files) $ \(path, contents) -> do
        writeFileChanged (testDirectory </> path <.> "golden") contents


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
