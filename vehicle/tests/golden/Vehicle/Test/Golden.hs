module Vehicle.Test.Golden
  ( TestFilter,
    makeTestTreesFromFile,
    makeTestTreeFromDirectoryRecursive,
  )
where

import Control.Exception (throw)
import Control.Monad (filterM, forM, when)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Data.List (isSuffixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getDirectoryContents,
    listDirectory,
  )
import System.FilePath
  ( makeRelative,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Printf (printf)
import Vehicle.Prelude (vehicleObjectFileExtension)
import Vehicle.Test.Golden.Extra
  ( SomeOption,
    createDirectoryRecursive,
    listFilesRecursive,
    someLocalOptions,
  )
import Vehicle.Test.Golden.TestSpec
  ( TestOutput (..),
    TestSpec,
    TestSpecs (TestSpecs),
    readGoldenFiles,
    readTestSpecsFile,
    testSpecDiffTestOutput,
    testSpecIsEnabled,
    testSpecName,
    testSpecNeeds,
    testSpecOptions,
    testSpecRun,
    writeGoldenFiles,
  )

type TestFilter = TestName -> Bool

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: TestName -> TestFilter -> FilePath -> IO TestTree
makeTestTreeFromDirectoryRecursive testGroupLabel testFilter testDirectory = do
  -- List all paths in `testDirectory`
  testDirectoryEntries <- listDirectory testDirectory

  -- Construct test trees for each .test.json file in the current directory:
  testTreesFromHere <-
    -- Filter directory entries to only test specifications
    filterM (isTestSpecFile . (testDirectory </>)) testDirectoryEntries
      -- Make test trees
      >>= traverse
        ( \testSpecFileName ->
            makeTestTreesFromFile testFilter (testDirectory </> testSpecFileName)
        )
      <&> concat

  -- Construct test trees for all subdirectories:
  testTreesFromFurther <-
    -- Filter directory entries to only test specifications:
    filterM (doesDirectoryExist . (testDirectory </>)) testDirectoryEntries
      -- Make test trees for each subdirectory:
      >>= traverse
        ( \subDirectoryName ->
            let testSubDirectory = testDirectory </> subDirectoryName
             in makeTestTreeFromDirectoryRecursive subDirectoryName testFilter testSubDirectory
        )

  -- Combine all test trees:
  let result = testGroup testGroupLabel (testTreesFromHere <> testTreesFromFurther)

  return result

-- | Read a test specification and return a TestTree.
makeTestTreesFromFile :: TestFilter -> FilePath -> IO [TestTree]
makeTestTreesFromFile testFilter testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  let enabledTestSpec = filter testSpecIsEnabled $ NonEmpty.toList testSpecs
  let filteredTestSpec = filter (testFilter . testSpecName) enabledTestSpec
  return $ toTestTree testSpecFile <$> filteredTestSpec

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
        testName = testSpecName testSpec
        readGolden = readGoldenFiles testDirectory testSpec
        compareTestOuput = testSpecDiffTestOutput testSpec
        updateGolden = writeGoldenFiles testDirectory testSpec
        runTest = do
          -- Create a temporary directory:
          let tempDirectoryNameTemplate = printf "vehicle-test-%s" (testSpecName testSpec)
          withSystemTempDirectory tempDirectoryNameTemplate $ \tempDirectory -> do
            -- Copy over all needed files:
            let neededFiles = Set.fromList $ testSpecNeeds testSpec
            allCopiedFiles <- forM (Set.toList neededFiles) $ \neededFile -> do
              createDirectoryRecursive (tempDirectory </> takeDirectory neededFile)
              let originalFilePath = testDirectory </> neededFile
              let finalFilePath = tempDirectory </> neededFile
              copyRecursively removeGoldenPrefix originalFilePath finalFilePath
            let copiedFiles = Set.fromList (drop (length tempDirectory + 1) <$> concat allCopiedFiles)

            -- Run the command in the specified directory:
            let cmdSpec = (shell $ testSpecRun testSpec) {cwd = Just tempDirectory}
            (_exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""
            -- Gather the outputs
            let testOutputStdout = Text.pack stdoutString
            let testOutputStderr = Text.pack stderrString
            testOutputFiles <- HashMap.fromList <$> getTestOutputFiles copiedFiles tempDirectory
            return TestOutput {..}

-- | Copies the file or directory recursively, applying the provided function to file names
-- when files are found.
copyRecursively :: (FilePath -> FilePath) -> FilePath -> FilePath -> IO [FilePath]
copyRecursively updateName src dst = do
  whenM (not <$> doesPathExist src) $
    throw (userError $ "test source file '" <> src <> "' does not exist")

  isDirectory <- doesDirectoryExist src
  if isDirectory
    then do
      createDirectory dst
      content <- getDirectoryContents src
      let xs = filter (`notElem` ([".", ".."] :: [FilePath])) content
      copiedFiles <- forM xs $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        copyRecursively updateName srcPath dstPath
      return $ concat copiedFiles
    else do
      let finalDst = updateName dst
      copyFile src finalDst
      return [finalDst]
  where
    whenM s r = s >>= flip when r

removeGoldenPrefix :: FilePath -> FilePath
removeGoldenPrefix path
  | golden `isSuffixOf` path = take (length path - length golden) path
  | otherwise = path
  where
    golden = ".golden"

getTestOutputFiles :: Set FilePath -> FilePath -> IO [(FilePath, Text)]
getTestOutputFiles ignoredFiles tempDirectory = do
  absoluteFilePaths <- listFilesRecursive tempDirectory
  let filePaths = fmap (makeRelative tempDirectory) absoluteFilePaths
  let outputFilePaths = filter (isOutputFile ignoredFiles) filePaths
  forM outputFilePaths $ \filePath -> do
    fileContents <- Text.readFile $ tempDirectory </> filePath
    return (filePath, fileContents)

isOutputFile :: Set FilePath -> FilePath -> Bool
isOutputFile inputFiles file =
  let extension = takeExtension file
   in file `Set.notMember` inputFiles
        &&
        -- Exclude profiling files
        extension /= ".prof"
        &&
        -- Exclude interface files
        extension /= vehicleObjectFileExtension
