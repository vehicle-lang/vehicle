{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.Error where

import Control.Exception (Exception)
import Data.List qualified as List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import General.Extra (boolToMaybe)
import Test.Tasty (TestName)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern)
import Test.Tasty.Providers (Result, testFailed)
import Text.Printf (printf)

-- | Raised when the needed file for a test is not found.
newtype NeededFilesError = NeededFilesError {neededFilesNotFound :: [FilePath]}
  deriving (Show, Semigroup)

neededFileNotFound :: FilePath -> NeededFilesError
neededFileNotFound file = NeededFilesError [file]

instance Exception NeededFilesError

handleNeededFilesError :: NeededFilesError -> IO Result
handleNeededFilesError NeededFilesError {..} = do
  let message =
        printf "Could not find needed files: %s" $
          List.intercalate ", " (show <$> neededFilesNotFound)
  return $ testFailed message

-- | Raised when the golden file for a test is not found.
newtype GoldenFilesNotFoundError = GoldenFilesNotFoundError {goldenFilesNotFound :: [FilePattern]}
  deriving (Show, Semigroup)

goldenFileNotFound :: FilePattern -> GoldenFilesNotFoundError
goldenFileNotFound filePattern = GoldenFilesNotFoundError [filePattern]

instance Exception GoldenFilesNotFoundError

handleGoldenFilesNotFoundError :: GoldenFilesNotFoundError -> IO Result
handleGoldenFilesNotFoundError GoldenFilesNotFoundError {..} = do
  let message =
        printf "Could not find golden files: %s" $
          List.intercalate ", " (show <$> goldenFilesNotFound)
  return $ testFailed message

-- | Raised when the test run does not produce an expected file or does not
--   expect a produced file, or when the produced file differs from the
--   expected file.
data ProducedFilesError = ProducedFilesError
  { expectedFilesNotProduced :: [FilePath],
    producedFilesNotExpected :: [FilePath],
    producedAndExpectedDiffs :: Map FilePath Diff
  }
  deriving (Show)

instance Semigroup ProducedFilesError where
  (<>) :: ProducedFilesError -> ProducedFilesError -> ProducedFilesError
  e1 <> e2 =
    ProducedFilesError
      { expectedFilesNotProduced = expectedFilesNotProduced e1 <> expectedFilesNotProduced e2,
        producedFilesNotExpected = producedFilesNotExpected e1 <> producedFilesNotExpected e2,
        producedAndExpectedDiffs = producedAndExpectedDiffs e1 <> producedAndExpectedDiffs e2
      }

expectedFileNotProduced :: FilePath -> ProducedFilesError
expectedFileNotProduced file = ProducedFilesError [file] [] Map.empty

producedFileNotExpected :: FilePath -> ProducedFilesError
producedFileNotExpected file = ProducedFilesError [] [file] Map.empty

producedAndExpectedDiffer :: FilePath -> Diff -> ProducedFilesError
producedAndExpectedDiffer file diff = ProducedFilesError [] [] (Map.singleton file diff)

instance Exception ProducedFilesError

handleProducedFilesError :: ProducedFilesError -> IO Result
handleProducedFilesError ProducedFilesError {..} = do
  let message =
        unlines . catMaybes $
          [ boolToMaybe (not $ null expectedFilesNotProduced) $
              printf "Did not produce expected files: %s" $
                List.intercalate ", " (show <$> expectedFilesNotProduced),
            boolToMaybe (not $ null expectedFilesNotProduced) $
              printf "Did not expect produced files: %s" $
                List.intercalate ", " (show <$> producedFilesNotExpected),
            boolToMaybe (not $ null producedAndExpectedDiffs) $
              unlines . flip foldMap (Map.assocs producedAndExpectedDiffs) $ \(file, diff) ->
                return $ printf "Expected and produced files differ for %s:\n%s" file (show diff)
          ]
  return $ testFailed message

data Diff
  = Diff String
  | NoDiff String

instance Show Diff where
  show :: Diff -> String
  show (Diff prettyDiff) = prettyDiff
  show (NoDiff reason) = "No diff: " <> reason

instance Exception Diff

-- | Raised when the test run exits with a non-zero exit code.
newtype ExitFailure = ExitFailure Int
  deriving (Show)

instance Exception ExitFailure

handleExitFailure :: ExitFailure -> IO Result
handleExitFailure (ExitFailure code) = do
  let message = printf "Test terminated with exit code %d" code
  return $ testFailed message

data AmbiguousGoldenFile = AmbiguousGoldenFile
  { thisTestName :: TestName,
    thisTestGoldenFile :: FilePath,
    otherTestName :: TestName,
    otherTestProducesPattern :: FilePattern
  }
  deriving (Show)

-- | Raised when a golden file is matched by the produces pattern of an unrelated test.
newtype AmbiguousGoldenFilesError = AmbiguousGoldenFilesError [AmbiguousGoldenFile]
  deriving (Show, Semigroup)

instance Exception AmbiguousGoldenFilesError

goldenFileIsAmbiguous :: AmbiguousGoldenFile -> AmbiguousGoldenFilesError
goldenFileIsAmbiguous ambiguousGoldenFile = AmbiguousGoldenFilesError [ambiguousGoldenFile]
