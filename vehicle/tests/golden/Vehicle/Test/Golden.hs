{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Vehicle.Test.Golden
  ( makeTestTreesFromFile
  , makeTestTreeFromDirectoryRecursive
  )
  where
import Control.Monad (filterM, forM, forM_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (copyFile, doesDirectoryExist, doesFileExist,
                         listDirectory)
import System.FilePath (makeRelative, takeBaseName, takeDirectory, takeFileName,
                        (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Printf (printf)
import Vehicle.Test.Golden.Extra (SomeOption, createDirectoryRecursive,
                                  listFilesRecursive, someLocalOptions)
import Vehicle.Test.Golden.TestSpec (TestOutput (..), TestSpec,
                                     TestSpecs (TestSpecs), readGoldenFiles,
                                     readTestSpecsFile, testSpecDiffTestOutput,
                                     testSpecIsEnabled, testSpecName,
                                     testSpecNeeds, testSpecOptions,
                                     testSpecRun, writeGoldenFiles)

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: TestName -> FilePath -> IO TestTree
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
      <&> concat

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

-- | Read a test specification and return a TestTree.
makeTestTreesFromFile :: FilePath -> IO [TestTree]
makeTestTreesFromFile testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  let enabledTestSpec = filter testSpecIsEnabled $ NonEmpty.toList testSpecs
  return $ toTestTree testSpecFile <$> enabledTestSpec

-- | Test whether a path refers to an existing test specification file.
isTestSpecFile :: FilePath -> IO Bool
isTestSpecFile path = (takeFileName path == "test.json" &&) <$> doesFileExist path

-- | Convert a test specifications to a test tree.
toTestTree :: FilePath -> TestSpec -> TestTree
toTestTree testSpecFile testSpec = someLocalOptions testOptions testTree
  where
    testOptions :: [SomeOption]
    testOptions = testSpecOptions testSpec

    testDirectory :: FilePath
    testDirectory = takeDirectory testSpecFile

    testTree :: TestTree
    testTree = goldenTest testName readGolden runTest compareTestOuput updateGolden
      where
        testName         = testSpecName testSpec
        readGolden       = readGoldenFiles testDirectory testSpec
        compareTestOuput = testSpecDiffTestOutput testSpec
        updateGolden     = writeGoldenFiles testDirectory testSpec
        runTest          = do
          -- Create a temporary directory:
          let tempDirectoryNameTemplate = printf "vehicle-test-%s" (testSpecName testSpec)
          withSystemTempDirectory tempDirectoryNameTemplate $ \tempDirectory -> do
            -- Copy over all needed files:
            let neededFiles = Set.fromList $ testSpecNeeds testSpec
            forM_ neededFiles $ \neededFile -> do
              createDirectoryRecursive (tempDirectory </> takeDirectory neededFile)
              copyFile (testDirectory </> neededFile) (tempDirectory </> neededFile)
            -- Run the command in the specified directory:
            let cmdSpec = (shell $ testSpecRun testSpec) {cwd = Just tempDirectory}
            (_exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""
            -- Gather the outputs
            let testOutputStdout = Text.pack stdoutString
            let testOutputStderr = Text.pack stderrString
            testOutputFiles <- HashMap.fromList <$> getTestOutputFiles neededFiles tempDirectory
            return TestOutput{..}

getTestOutputFiles :: Set FilePath -> FilePath -> IO [(FilePath, Text)]
getTestOutputFiles ignoredFiles tempDirectory = do
  absoluteFilePaths <- listFilesRecursive tempDirectory
  let filePaths = fmap (makeRelative tempDirectory) absoluteFilePaths
  let outputFilePaths = filter (`Set.notMember` ignoredFiles) filePaths
  forM outputFilePaths $ \filePath -> do
    fileContents <- Text.readFile $ tempDirectory </> filePath
    return (filePath, fileContents)
