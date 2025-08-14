{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.Error where

import Control.Applicative ((<|>))
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
newtype GoldenFilesNotFoundError = GoldenFilesNotFoundError {goldenFilesNotFound :: [FilePath]}
  deriving (Show, Semigroup)

instance Exception GoldenFilesNotFoundError

handleGoldenFilesNotFoundError :: GoldenFilesNotFoundError -> IO Result
handleGoldenFilesNotFoundError GoldenFilesNotFoundError {..} = do
  let message =
        printf "Could not run test as could not find the required golden files:\n  %s" $
          List.intercalate "\n  " goldenFilesNotFound
  return $ testFailed message

-- | Raised when no golden files provided.
newtype GoldenFilesNotProvidedError = GoldenFilesNotProvidedError {goldenFilesNotProvided :: [FilePattern]}
  deriving (Show, Semigroup)

instance Exception GoldenFilesNotProvidedError

handleGoldenFilesNotProvidedError :: GoldenFilesNotProvidedError -> IO Result
handleGoldenFilesNotProvidedError GoldenFilesNotProvidedError {..} = do
  let cause = case goldenFilesNotProvided of
        [] -> "no golden files were specified via the 'produces' field"
        _ : _ -> do
          let patternString = List.intercalate "\n  " (show <$> goldenFilesNotProvided)
          printf "no golden files were matched via the patterns:\n  %s" patternString
  let message = "Could not run test as " <> cause
  return $ testFailed message

-- | Raised when the test run does not produce an expected file or does not
--   expect a produced file, or when the produced file differs from the
--   expected file.
data ProducedFilesError = ProducedFilesError
  { stdoutDiff :: Maybe Diff,
    stderrDiff :: Maybe Diff,
    expectedFilesNotProduced :: [FilePath],
    producedFilesNotExpected :: [FilePath],
    producedAndExpectedDiffs :: Map FilePath Diff
  }
  deriving (Show)

instance Semigroup ProducedFilesError where
  (<>) :: ProducedFilesError -> ProducedFilesError -> ProducedFilesError
  e1 <> e2 =
    ProducedFilesError
      { stdoutDiff = stdoutDiff e1 <|> stdoutDiff e2,
        stderrDiff = stderrDiff e1 <|> stderrDiff e2,
        expectedFilesNotProduced = expectedFilesNotProduced e1 <> expectedFilesNotProduced e2,
        producedFilesNotExpected = producedFilesNotExpected e1 <> producedFilesNotExpected e2,
        producedAndExpectedDiffs = producedAndExpectedDiffs e1 <> producedAndExpectedDiffs e2
      }

expectedFileNotProduced :: FilePath -> ProducedFilesError
expectedFileNotProduced file = ProducedFilesError Nothing Nothing [file] [] Map.empty

producedFileNotExpected :: FilePath -> ProducedFilesError
producedFileNotExpected file = ProducedFilesError Nothing Nothing [] [file] Map.empty

producedAndExpectedDiffer :: FilePath -> Diff -> ProducedFilesError
producedAndExpectedDiffer file diff = ProducedFilesError Nothing Nothing [] [] (Map.singleton file diff)

stdoutDiffer :: Diff -> ProducedFilesError
stdoutDiffer diff = ProducedFilesError (Just diff) Nothing [] [] mempty

stderrDiffer :: Diff -> ProducedFilesError
stderrDiffer diff = ProducedFilesError Nothing (Just diff) [] [] mempty

instance Exception ProducedFilesError

handleProducedFilesError :: ProducedFilesError -> IO Result
handleProducedFilesError ProducedFilesError {..} = do
  let message =
        unlines . catMaybes $
          [ case stderrDiff of
              Nothing -> Nothing
              Just diff -> Just $ printf "The contents of stderr differ: \n%s" $ show diff,
            case stdoutDiff of
              Nothing -> Nothing
              Just diff -> Just $ printf "The contents of stdout differ: \n%s" $ show diff,
            boolToMaybe (not $ null expectedFilesNotProduced) $
              printf "Did not produce expected files: %s" $
                List.intercalate ", " (show <$> expectedFilesNotProduced),
            boolToMaybe (not $ null producedFilesNotExpected) $
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

ambiguousGoldenFileToString :: AmbiguousGoldenFile -> String
ambiguousGoldenFileToString AmbiguousGoldenFile {..} =
  printf "The golden file '%s' in this test is also produced by test '%s' via pattern '%s'" thisTestGoldenFile otherTestName (show otherTestProducesPattern)

goldenFileIsAmbiguous :: AmbiguousGoldenFile -> AmbiguousGoldenFilesError
goldenFileIsAmbiguous ambiguousGoldenFile = AmbiguousGoldenFilesError [ambiguousGoldenFile]

handleAmbiguousGoldenFile :: AmbiguousGoldenFilesError -> IO Result
handleAmbiguousGoldenFile (AmbiguousGoldenFilesError files) = do
  let fileMessages = List.intercalate "\n  " (ambiguousGoldenFileToString <$> files)
  let message = "Cannot run tests as golden files overlap:\n  " <> fileMessages
  return $ testFailed message
