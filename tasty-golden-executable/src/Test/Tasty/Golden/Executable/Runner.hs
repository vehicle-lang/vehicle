{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Test.Tasty.Golden.Executable.Runner where

import Control.Exception (Exception, throw)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..), execWriterT)
import Data.Algorithm.Diff (getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup qualified as Set
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.IO qualified as LazyIO
import Data.Traversable (for)
import General.Extra.Diff (isBoth, mapDiff)
import General.Extra.File (createDirectoryRecursive)
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.Exit qualified as ExitCode
import System.FilePath (isAbsolute, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern, addExtension, glob, match)
import Test.Tasty.Golden.Executable.TestSpec.TextPattern (TextPattern, strikeOut)
import Test.Tasty.Providers (TestName)
import Text.Printf (printf)

data TestEnvironment = TestEnvironment
  { testName :: TestName,
    testNeeds :: [FilePath],
    testDirectory :: FilePath,
    tempDirectory :: FilePath
  }

-- | Monad for running tests in an isolated environment.
newtype TestT m a = TestT {unTest :: StateT TestEnvironment m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans)

-- | Alias for @'TestT' 'IO' a@.
type TestIO a = TestT IO a

-- | Raised when the needed file for a test is not found.
newtype NeededFilesNotFound = NeededFilesNotFound (NonEmpty FilePath)
  deriving (Show, Semigroup)

instance Exception NeededFilesNotFound

-- | Raised when the golden file for a test is not found.
newtype GoldenFilesNotFound = GoldenFilesNotFound (NonEmpty FilePattern)
  deriving (Show, Semigroup)

instance Exception GoldenFilesNotFound

-- | Raised when the test run does not produce a file.
newtype FilesNotProduced = FilesNotProduced (NonEmpty FilePath)
  deriving (Show, Semigroup)

instance Exception FilesNotProduced

-- | Raised when the test run exits with a non-zero exit code.
newtype ExitFailure = ExitFailure Int
  deriving (Show)

instance Exception ExitFailure

-- | Raise when an output differs from its corresponding golden file.
newtype Diff = Diff String
  deriving (Show)

instance Exception Diff

-- | Create a temporary directory to execute the test.
runTestIO :: FilePath -> TestName -> TestIO r -> IO r
runTestIO testDirectory testName (TestT testIO) = do
  withSystemTempDirectory testName $ \tempDirectory -> do
    createDirectoryRecursive tempDirectory
    evalStateT testIO TestEnvironment {testNeeds = [], ..}

-- | Copy the needed files over to the temporary directory.
copyTestNeeds :: [FilePath] -> TestIO ()
copyTestNeeds neededFiles = TestT $ do
  TestEnvironment {..} <- get
  -- Register the 'neededFiles' with the environment.
  put TestEnvironment {testNeeds = neededFiles <> testNeeds, ..}
  lift $ do
    maybeError <- execWriterT $ do
      for_ neededFiles $ \neededFile -> do
        let neededFileTarget = tempDirectory </> neededFile
        -- Try to find the needed file in the test directory
        let neededFileSource = testDirectory </> neededFile
        neededFileExists <- lift $ doesFileExist (testDirectory </> neededFile)
        -- Try to find the needed file in the test directory as a golden file
        let neededFileSourceAsGolden = neededFileSource <.> "golden"
        neededFileExistsAsGolden <- lift $ doesFileExist neededFileSourceAsGolden
        -- If either is successful, copy the file. Otherwise, throw an error.
        if
          | neededFileExists -> lift $ copyFile neededFileSource neededFileTarget
          | neededFileExistsAsGolden -> lift $ copyFile neededFileSourceAsGolden neededFileTarget
          | otherwise -> tell $ Just $ NeededFilesNotFound (NonEmpty.singleton $ testDirectory </> neededFile)
    -- If errors were raised, throw them.
    maybe (return ()) throw maybeError

-- | Run the test command in the temporary directory.
runTestRun :: String -> TestIO (Lazy.Text, Lazy.Text)
runTestRun cmd = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    -- Run the test command
    let cmdSpec = (shell cmd) {cwd = Just tempDirectory}
    (exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""
    -- If the exit code is zero, return the stdout and stderr. Otherwise, throw an error.
    case exitCode of
      ExitCode.ExitSuccess -> return (Lazy.pack stdoutString, Lazy.pack stderrString)
      ExitCode.ExitFailure code -> throw $ ExitFailure code

-- | Compare the standard output to the golden file.
--
-- NOTE: The loose equality must extend equality.
diffStdout :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> TestIO ()
diffStdout maybeLooseEq actual = do
  golden <- readGoldenStdout
  lift $ diffText maybeLooseEq golden actual

-- | Read the golden file for the standard output.
readGoldenStdout :: TestIO Lazy.Text
readGoldenStdout = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    let stdoutGoldenFile = testDirectory </> testName <.> "out.golden"
    stdoutGoldenFileExists <- doesFileExist stdoutGoldenFile
    if stdoutGoldenFileExists
      then LazyIO.readFile stdoutGoldenFile
      else return Lazy.empty

-- | Compare the standard output to the golden file.
--
-- NOTE: The loose equality must extend equality.
diffStderr :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> TestIO ()
diffStderr maybeLooseEq actual = do
  golden <- readGoldenStderr
  lift $ diffText maybeLooseEq golden actual

-- | Read the golden file for the standard error.
readGoldenStderr :: TestIO Lazy.Text
readGoldenStderr = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    let stderrGoldenFile = testDirectory </> testName <.> "err.golden"
    stderrGoldenFileExists <- doesFileExist stderrGoldenFile
    if stderrGoldenFileExists
      then LazyIO.readFile stderrGoldenFile
      else return ""

-- | Find the files produced by the test.
diffTestProduced :: Maybe (Text -> Text -> Bool) -> [FilePattern] -> [FilePattern] -> TestIO ()
diffTestProduced maybeLooseEq testProduces testIgnores = do
  goldenFiles <- findTestProducesGolden testProduces
  actualFiles <- findTestProducesActual testIgnores
  -- Compute sets of files:
  let goldenFileSet = Set.fromList goldenFiles
  let actualFileSet = Set.fromList goldenFiles
  -- Compute missing files:
  let missingFileSet = Set.difference goldenFileSet actualFileSet
  let extraFileSet = Set.difference actualFileSet goldenFileSet
  -- let missingOutputFileErrors =
  --       [ printf "Missing output file %s" missingFile
  --         | missingFile <- sort $ HashSet.toList $ HashSet.difference goldenFiles actualFiles
  --       ]
  -- Compute extraneous files:
  -- let extraOutputFileErrors =
  --       [ printf "Extraneous output file %s" extraFile
  --         | extraFile <- sort $ HashSet.toList $ HashSet.difference actualFiles goldenFiles
  --       ]
  undefined

-- TestT $ do

-- let findGoldenFilesFor pat = glob testDirectory (pat <.> "golden")
-- goldenFiles <- traverse (\pat -> lift $ glob testDirectory (pat <.> "golden")) pats
-- _

-- | Find the actual files produced by the test command.
findTestProducesActual :: [FilePattern] -> TestIO [FilePath]
findTestProducesActual testIgnoreFiles = TestT $ do
  TestEnvironment {..} <- get
  tempFiles <- lift $ listDirectory tempDirectory
  -- Assert that the file paths are relative.
  for_ tempFiles $ \file -> do
    when (isAbsolute file) $
      fail $
        printf "glob: found absolute path %s" file
  -- Filter out the files copied by 'copyTestNeeds' or matched by 'testIgnoreFiles'.
  let tempFilesProducedAndNotIgnored =
        [ file
          | file <- tempFiles,
            file `notElem` testNeeds,
            and [not (ignore `match` file) | ignore <- testIgnoreFiles]
        ]
  return tempFilesProducedAndNotIgnored

-- | Find the golden files for the files produced by the test command.
findTestProducesGolden :: [FilePattern] -> TestIO [FilePath]
findTestProducesGolden testProduces = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    (filesByPattern, maybeError) <-
      runWriterT $ do
        filesByPattern <-
          for testProduces $ \testProduce -> do
            filesForPattern <- lift $ glob [addExtension testProduce "golden"] testDirectory
            -- If the pattern does not result in any matches, throw an error.
            when (null filesForPattern) $
              tell $
                Just $
                  GoldenFilesNotFound (NonEmpty.singleton testProduce)
            -- Assert that the file paths are relative.
            for_ filesForPattern $ \file ->
              when (isAbsolute file) $
                fail $
                  printf "glob: found absolute path %s" file
            return filesForPattern
        return $ concat filesByPattern
    -- If errors were raised, throw them.
    maybe (return filesByPattern) throw maybeError

-- | Compare two files.
--
-- NOTE: The loose equality must extend equality.
diffFile :: Maybe (Text -> Text -> Bool) -> FilePath -> FilePath -> IO ()
diffFile maybeLooseEq golden actual = undefined

-- | Compare two texts.
--
-- NOTE: The loose equality must extend equality.
diffText :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> Lazy.Text -> IO ()
diffText maybeLooseEq golden actual = do
  -- Lazily split the golden and actual texts into lines
  let goldenLines = Lazy.toStrict <$> Lazy.lines golden
  let actualLines = Lazy.toStrict <$> Lazy.lines actual
  -- Create an equality operation which short circuits the loose equality
  let shortCircuitLooseEq = shortCircuitWithEq maybeLooseEq
  -- Compute the diff
  let groupedDiff = getGroupedDiffBy shortCircuitLooseEq goldenLines actualLines
  -- If both files are the same, the diff should be just "Both":
  unless (all isBoth groupedDiff) $
    throw $
      Diff (ppDiff $ mapDiff (Text.unpack <$>) <$> groupedDiff)
  return ()

-- | Make a loose equality which ignores text matching the provided text patterns.
makeLooseEq :: [TextPattern] -> Text -> Text -> Bool
makeLooseEq patterns golden actual = strikeOutAll golden == strikeOutAll actual
  where
    strikeOutAll line = foldr strikeOut line patterns

-- | Make a loose equality which short-circuits using equality.
shortCircuitWithEq :: (Eq a) => Maybe (a -> a -> Bool) -> a -> a -> Bool
shortCircuitWithEq maybeEq x y = x == y || maybe False (\eq -> x `eq` y) maybeEq
