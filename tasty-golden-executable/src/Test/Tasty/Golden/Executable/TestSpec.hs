module Test.Tasty.Golden.Executable.TestSpec
  ( TestSpec (..),
    derivedOptions,
    isEnabled,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (FromJSON (parseJSONList))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Object,
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
import Data.Data (Typeable)
-- import Test.Tasty.Golden.Executable.TestSpec.External (External)

import Data.Function (on)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import General.Extra (boolToMaybe)
import General.Extra.Option (SomeOption (..))
import Test.Tasty (TestName)
import Test.Tasty qualified as Tasty (Timeout (NoTimeout, Timeout))
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (GoldenFilePattern)
import Test.Tasty.Golden.Executable.TestSpec.Ignore (Ignore)
import Test.Tasty.Golden.Executable.TestSpec.Ignore qualified as Ignore
import Test.Tasty.Options (IsOption (parseValue))

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
    testSpecProduces :: [GoldenFilePattern],
    -- | External tools needed by the test command.
    --   Paths should be the names of executables on the PATH.
    testSpecExternal :: [External],
    -- | Timeout for the test.
    testSpecTimeout :: Timeout,
    -- | Options that configure what differences to ignore.
    testSpecIgnore :: Maybe Ignore
  }
  deriving (Show)

-- | Local options derived from the test specification.
derivedOptions :: TestSpec -> [SomeOption]
derivedOptions testSpec =
  catMaybes [LocalOption <$> testSpecTimeout testSpec]

-- | Whether or not the test is enabled.
isEnabled :: TestSpec -> Bool
isEnabled = fromMaybe True . testSpecEnabled

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \o ->
    TestSpec
      <$> o .: "name"
      <*> o .: "run"
      <*> o .:? "enabled"
      <*> o .:? "needs" .!= []
      <*> produces o
      <*> o .:? "external" .!= []
      <*> timeout o
      <*> o .:? "ignore"
    where
      produces :: Object -> Parser [GoldenFilePattern]
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
          -- Include "external" only if it is non-empty:
          boolToMaybe (not $ null testSpecExternal) ("external" .= testSpecNeeds),
          -- Include "timeout" only if it is non-empty:
          boolToMaybe (isTimeout testSpecTimeout) ("timeout" .= testSpecTimeout),
          -- Include "ignore" only if it is non-empty:
          boolToMaybe (not $ Ignore.null testSpecIgnore) ("ignore" .= testSpecIgnore)
        ]

-- * External Programs

-- | The name of an external program.
newtype External = External {unExternal :: Text}
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON External where
  parseJSON :: Value -> Parser External
  parseJSON (Value.String text) = return $ External text
  parseJSON value = typeMismatch "String" value

instance ToJSON External where
  toJSON :: External -> Value
  toJSON = toJSON . unExternal

-- * Timeout

newtype Timeout = Timeout Tasty.Timeout
  deriving (Show, Typeable)

-- | Get the timeout in milliseconds.
getTimeoutMS :: Timeout -> Maybe Integer
getTimeoutMS (Timeout (Tasty.Timeout ms _)) = Just ms
getTimeoutMS _ = Nothing

-- | Check whether a timeout represents an finite timeout.
isTimeout :: Timeout -> Bool
isTimeout = isJust . getTimeoutMS

instance Eq Timeout where
  (==) :: Timeout -> Timeout -> Bool
  (==) = (==) `on` getTimeoutMS

instance Ord Timeout where
  compare :: Timeout -> Timeout -> Ordering
  compare = compare `on` getTimeoutMS

instance FromJSON Timeout where
  parseJSON :: Value -> Parser Timeout
  parseJSON (Value.String timeoutText) =
    let timeoutString = Text.unpack timeoutText
     in case parseValue timeoutString of
          Nothing ->
            fail $
              unlines
                [ "Could not parse value of 'timeout'.",
                  "Expected a number, optionally followed by a suffix (ms, s, m, h).",
                  "Found: " <> timeoutString
                ]
          Just tastyTimeout -> return $ Timeout tastyTimeout
  parseJSON value = typeMismatch "String" value

instance ToJSON Timeout where
  toJSON :: Timeout -> Value
  toJSON (Timeout (Tasty.Timeout _ms timeoutString)) = Value.String (Text.pack timeoutString)
  toJSON (Timeout Tasty.NoTimeout) = Value.Null
