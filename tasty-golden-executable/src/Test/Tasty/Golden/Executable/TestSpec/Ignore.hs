{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.TestSpec.Ignore
  ( Ignore (..),
    IgnoreFiles (..),
    ignoreFilesToIgnore,
    IgnoreLines (..),
    ignoreLinesToIgnore,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Parser,
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    (.!=),
    (.:?),
  )
import Data.Data (Typeable)
import Data.Maybe (catMaybes)
import Data.Tagged (Tagged)
import Data.Text qualified as Text (pack, splitOn, strip, unpack)
import General.Extra (boolToMaybe)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern)
import Test.Tasty.Golden.Executable.TestSpec.TextPattern (TextPattern)
import Test.Tasty.Options (IsOption (..), safeRead)

data Ignore = Ignore
  { -- | Files produced by the test command that should be ignored.
    ignoreFiles :: IgnoreFiles,
    -- | Lines produced by the test command that should be ignored.
    ignoreLines :: IgnoreLines
  }
  deriving (Eq, Show, Typeable)

instance Semigroup Ignore where
  (<>) :: Ignore -> Ignore -> Ignore
  ignore1 <> ignore2 =
    Ignore
      { ignoreFiles = ignoreFiles ignore1 <> ignoreFiles ignore2,
        ignoreLines = ignoreLines ignore1 <> ignoreLines ignore2
      }

instance Monoid Ignore where
  mempty :: Ignore
  mempty = Ignore mempty mempty

instance FromJSON Ignore where
  parseJSON :: Value -> Parser Ignore
  parseJSON = withObject "Ignore" $ \o ->
    Ignore
      <$> o .:? "files" .!= mempty
      <*> o .:? "lines" .!= mempty

instance ToJSON Ignore where
  toJSON :: Ignore -> Value
  toJSON Ignore {..} =
    object $
      catMaybes
        [ boolToMaybe (ignoreFiles /= mempty) ("files" .= ignoreFiles),
          boolToMaybe (ignoreLines /= mempty) ("lines" .= ignoreLines)
        ]

newtype IgnoreFiles = IgnoreFiles [FilePattern]
  deriving (Eq, Show, Typeable, Semigroup, Monoid)

ignoreFilesToIgnore :: IgnoreFiles -> Ignore
ignoreFilesToIgnore ignoreFiles = Ignore ignoreFiles mempty

instance FromJSON IgnoreFiles where
  parseJSON :: Value -> Parser IgnoreFiles
  parseJSON v = parse1 v <|> parseN v
    where
      parse1 = fmap (IgnoreFiles . (: [])) . parseJSON
      parseN = fmap IgnoreFiles . parseJSON

instance ToJSON IgnoreFiles where
  toJSON :: IgnoreFiles -> Value
  toJSON (IgnoreFiles [ignoreFile]) = toJSON ignoreFile
  toJSON (IgnoreFiles ignoreFiles) = toJSON ignoreFiles

instance IsOption IgnoreFiles where
  defaultValue :: IgnoreFiles
  defaultValue = mempty

  parseValue :: String -> Maybe IgnoreFiles
  parseValue input = IgnoreFiles <$> traverse safeRead filePatternStrings
    where
      filePatternStrings = Text.unpack . Text.strip <$> Text.splitOn "," (Text.pack input)

  optionName :: Tagged IgnoreFiles String
  optionName = return "ignore-files"

  optionHelp :: Tagged IgnoreFiles String
  optionHelp = return "A list of files to ignore."

newtype IgnoreLines = IgnoreLines [TextPattern]
  deriving (Eq, Show, Typeable, Semigroup, Monoid)

ignoreLinesToIgnore :: IgnoreLines -> Ignore
ignoreLinesToIgnore = Ignore mempty

instance FromJSON IgnoreLines where
  parseJSON :: Value -> Parser IgnoreLines
  parseJSON v = parse1 v <|> parseN v
    where
      parse1 = fmap (IgnoreLines . (: [])) . parseJSON
      parseN = fmap IgnoreLines . parseJSON

instance ToJSON IgnoreLines where
  toJSON :: IgnoreLines -> Value
  toJSON (IgnoreLines [ignoreLine]) = toJSON ignoreLine
  toJSON (IgnoreLines ignoreLines) = toJSON ignoreLines

instance IsOption IgnoreLines where
  defaultValue :: IgnoreLines
  defaultValue = mempty

  parseValue :: String -> Maybe IgnoreLines
  parseValue input = IgnoreLines <$> traverse safeRead filePatternStrings
    where
      filePatternStrings = Text.unpack . Text.strip <$> Text.splitOn "," (Text.pack input)

  optionName :: Tagged IgnoreLines String
  optionName = return "ignore-lines"

  optionHelp :: Tagged IgnoreLines String
  optionHelp = return "A list of lines to ignore."
