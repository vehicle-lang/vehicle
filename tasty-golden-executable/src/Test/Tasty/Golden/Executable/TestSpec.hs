module Test.Tasty.Golden.Executable.TestSpec
  ( TestSpec (..),
    derivedOptions,
    isEnabled,
  )
where

import Control.Exception (Exception, throw)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Parser,
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Tagged (Tagged (..))
import General.Extra (boolToMaybe)
import General.Extra.Option (SomeOption (..))
import Test.Tasty (TestName)
import Test.Tasty.Golden.Executable.Runner (acceptStderr, acceptStdout, acceptTestProduced, copyTestNeeds, diffStderr, diffStdout, diffTestProduced, makeLooseEq, runTestIO, runTestRun)
import Test.Tasty.Golden.Executable.TestSpec.Accept (Accept (..))
import Test.Tasty.Golden.Executable.TestSpec.External (AllowlistExternals (..), External)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern)
import Test.Tasty.Golden.Executable.TestSpec.Ignore (Ignore (..), IgnoreFiles, IgnoreLines)
import Test.Tasty.Golden.Executable.TestSpec.Timeout (Timeout, toSomeOption)
import Test.Tasty.Options (OptionDescription (..), OptionSet, lookupOption)
import Test.Tasty.Providers (IsTest (..), Result, testPassed)
import Test.Tasty.Runners (Progress, Result (..))

data TestSpec = TestSpec
  { -- | Test directory.
    testSpecDirectory :: FilePath,
    -- | Test name.
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
    -- | A list of external tools needed by the test command.
    --   Paths should be the names of executables on the PATH.
    testSpecExternals :: [External],
    -- | Timeout for the test.
    testSpecTimeout :: Timeout,
    -- | Options that configure what differences to ignore.
    testSpecIgnore :: Ignore
  }
  deriving (Eq, Show)

instance IsTest TestSpec where
  run :: OptionSet -> TestSpec -> (Progress -> IO ()) -> IO Result
  run options TestSpec {testSpecIgnore = Ignore {..}, ..} _progress
    | areExternalsAllowed (lookupOption options) testSpecExternals = do
        -- Create loose equality based on the ignore options
        let maybeLooseEq
              | ignoreLines == mempty = Nothing
              | otherwise = Just $ makeLooseEq (ignoreLines <> lookupOption options)
        -- Create test environment
        runTestIO testSpecDirectory testSpecName $ do
          -- Copy needs to test environment
          copyTestNeeds testSpecNeeds
          -- Run test command
          (stdout, stderr) <- runTestRun testSpecRun
          -- Check if --accept was passed, and act accordingly:
          if unAccept (lookupOption options)
            then do
              -- Update .golden file for stdout
              acceptStdout stdout
              -- Update .golden file for stderr
              acceptStderr stderr
              -- Update .golden file for stderr
              acceptTestProduced testSpecProduces (ignoreFiles <> lookupOption options)
            else do
              -- Diff stdout
              diffStdout maybeLooseEq stdout
              -- Diff stderr
              diffStderr maybeLooseEq stderr
              -- Diff produced files
              diffTestProduced maybeLooseEq testSpecProduces (ignoreFiles <> lookupOption options)
    | otherwise = return testSkip

  testOptions :: Tagged TestSpec [OptionDescription]
  testOptions =
    return
      [ Option (Proxy :: Proxy Accept),
        Option (Proxy :: Proxy AllowlistExternals),
        Option (Proxy :: Proxy IgnoreFiles),
        Option (Proxy :: Proxy IgnoreLines)
      ]

-- | Local options derived from the test specification.
derivedOptions :: TestSpec -> [SomeOption]
derivedOptions testSpec = [toSomeOption (testSpecTimeout testSpec)]

-- | 'Result' of a skipped test.
testSkip :: Result
testSkip = (testPassed "") {resultShortDescription = "SKIP"}

-- | Whether or not the required externals are allowed.
areExternalsAllowed :: AllowlistExternals -> [External] -> Bool
areExternalsAllowed (AllowlistExternals allowlistExternals) externals =
  Set.fromList externals `Set.isSubsetOf` Set.fromList allowlistExternals

-- | Whether or not the test is enabled.
isEnabled :: TestSpec -> Bool
isEnabled = fromMaybe True . testSpecEnabled

-- | Error raised when 'testSpecDirectory' is not set.
data TestSpecDirectoryMissing = TestSpecDirectoryMissing
  deriving (Show)

instance Exception TestSpecDirectoryMissing

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \o -> do
    TestSpec (throw TestSpecDirectoryMissing)
      <$> o .: "name"
      <*> o .: "run"
      <*> o .:? "enabled"
      <*> o .:? "needs" .!= mempty
      <*> o .:? "produces" .!= mempty
      <*> o .:? "external" .!= mempty
      <*> o .:? "timeout" .!= mempty
      <*> o .:? "ignore" .!= mempty

instance ToJSON TestSpec where
  toJSON :: TestSpec -> Value
  toJSON TestSpec {..} =
    object $
      catMaybes
        [ Just $ "name" .= testSpecName,
          Just $ "run" .= testSpecRun,
          ("enabled" .=) <$> testSpecEnabled,
          -- Include "needs" only if it is non-empty:
          boolToMaybe (testSpecNeeds /= mempty) ("needs" .= testSpecNeeds),
          -- Include "produces" only if it is non-empty:
          boolToMaybe (testSpecProduces /= mempty) ("produces" .= testSpecProduces),
          -- Include "external" only if it is non-empty:
          boolToMaybe (testSpecExternals /= mempty) ("external" .= testSpecNeeds),
          -- Include "timeout" only if it is non-empty:
          boolToMaybe (testSpecTimeout /= mempty) ("timeout" .= testSpecTimeout),
          -- Include "ignore" only if it is non-empty:
          boolToMaybe (testSpecIgnore /= mempty) ("ignore" .= testSpecIgnore)
        ]
