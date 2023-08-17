{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.TestSpec.Ignore
  ( Ignore (..),
    IgnoreLine,
    IgnoreLineOption (..),
    ignoreLineOption,
    ignoreLineOptionIngredient,
    IgnoreFile,
    IgnoreFileOption (..),
    ignoreFileOption,
    ignoreFileOptionIngredient,
    matchLine,
    Test.Tasty.Golden.Executable.TestSpec.Ignore.null,
  )
where

import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Parser,
    ToJSON (toJSON),
    Value,
    object,
    typeMismatch,
    withObject,
    (.:?),
  )
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Array qualified as Array ((!))
import Data.Data (Typeable)
import Data.Function (on)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Text qualified as Text
import General.Extra (splitOn)
import General.Extra.Aeson (fromStringOrStringArray)
import General.Extra.Option (SomeOption (..), appendOption)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern, IsFilePattern)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)
import Text.Printf (printf)
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.Regex.TDFA.Text qualified as Regex

-- * Types

-- | Type of options for the comparison between the produced output and the golden output.
data Ignore = Ignore
  { -- | Any text matching these regular expressions is ignored.
    ignoreLines :: [IgnoreLine],
    -- | Any file matching these file patterns is ignored.
    ignoreFiles :: [IgnoreFile]
  }
  deriving (Show, Eq, Ord, Typeable)

-- | Type of regular expression for lines to ignore in the comparison.
--
--   Consists of a pair of the original string and the parsed regular expression,
--   so that we can output the original string in `show` and `toJSON`.
data IgnoreLine = IgnoreLine
  { ignoreLineRegexText :: Text,
    ignoreLineRegex :: Regex
  }
  deriving (Typeable)

instance Eq IgnoreLine where
  (==) :: IgnoreLine -> IgnoreLine -> Bool
  (==) = (==) `on` ignoreLineRegexText

instance Ord IgnoreLine where
  compare :: IgnoreLine -> IgnoreLine -> Ordering
  compare = compare `on` ignoreLineRegexText

instance Show IgnoreLine where
  show :: IgnoreLine -> String
  show IgnoreLine {..} = show ignoreLineRegexText

parseIgnoreLine :: Text -> Either String IgnoreLine
parseIgnoreLine regexText =
  IgnoreLine regexText <$> Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt regexText

instance Read IgnoreLine where
  readsPrec :: Int -> ReadS IgnoreLine
  readsPrec _prec s = case parseIgnoreLine (Text.pack s) of
    Left _err -> []
    Right re -> [(re, "")]

instance IsString IgnoreLine where
  fromString :: String -> IgnoreLine
  fromString s = case parseIgnoreLine (Text.pack s) of
    Left err -> error $ printf "Cannot parse regular expression '%s'" err
    Right re -> re

-- | Type of file patterns for files to ignore in the comparison.
newtype IgnoreFile = IgnoreFile FilePattern
  deriving (Eq, Ord, Typeable, IsFilePattern, FromJSON, ToJSON)

instance Show IgnoreFile where
  show :: IgnoreFile -> String
  show (IgnoreFile pat) = show pat

instance Read IgnoreFile where
  readsPrec :: Int -> ReadS IgnoreFile
  readsPrec prec s = [(IgnoreFile value, rest) | (value, rest) <- readsPrec prec s]

instance IsString IgnoreFile where
  fromString :: String -> IgnoreFile
  fromString s = case safeRead s of
    Just opt -> opt
    Nothing -> error $ printf "Cannot file pattern '%s'" s

-- * Basic

null :: Maybe Ignore -> Bool
null = maybe True (\Ignore {..} -> Prelude.null ignoreLines && Prelude.null ignoreFiles)

-- | Compare two lines using the options set in Ignore.
matchLine :: Ignore -> Text -> Text -> Bool
matchLine ignore = (==) `on` strikeOutMatches ignore
  where
    -- Strike out matches for the IgnoreLine expression.
    strikeOutMatches :: Ignore -> Text -> Text
    strikeOutMatches testIgnore txt =
      foldr strikeOut txt (ignoreLineRegex <$> ignoreLines testIgnore)

    -- Strike out matches for a regular expression.
    strikeOut :: Regex -> Text -> Text
    strikeOut = strikeOutTop
      where
        strikeOutTop re txt = strikeOutAcc (Regex.matchAll re txt) txt []

        strikeOutAcc :: [Regex.MatchArray] -> Text -> [Text] -> Text
        strikeOutAcc [] txt acc = Text.concat (reverse (txt : acc))
        strikeOutAcc (match : matches) txt acc = strikeOutAcc matches rest newAcc
          where
            (matchOffset, matchLength) = match Array.! 0
            (beforeMatch, matchAndAfterMatch) = Text.splitAt matchOffset txt
            (_matchText, rest) = Text.splitAt matchLength matchAndAfterMatch
            newAcc = "[IGNORE]" : beforeMatch : acc

-- * Test options

ignoreLineOption :: [String] -> SomeOption
ignoreLineOption opt = appendOption (IgnoreLineOption (fromString <$> opt))

ignoreLineOptionIngredient :: Ingredient
ignoreLineOptionIngredient = includingOptions [Option (Proxy :: Proxy IgnoreLineOption)]

newtype IgnoreLineOption = IgnoreLineOption [IgnoreLine]
  deriving (Eq, Ord, Semigroup, Monoid, Typeable)

instance Read IgnoreLineOption where
  readsPrec :: Int -> ReadS IgnoreLineOption
  readsPrec _prec s = case traverse safeRead (splitOn (== ',') s) of
    Just opts -> [(IgnoreLineOption opts, "")]
    Nothing -> []

instance IsString IgnoreLineOption where
  fromString :: String -> IgnoreLineOption
  fromString s = case safeRead s of
    Just opt -> opt
    Nothing -> error $ printf "Cannot parse ignore line option '%s'" s

instance IsOption IgnoreLineOption where
  defaultValue :: IgnoreLineOption
  defaultValue = IgnoreLineOption []

  parseValue :: String -> Maybe IgnoreLineOption
  parseValue = safeRead

  optionName :: Tagged IgnoreLineOption String
  optionName = return "ignore-lines"

  optionHelp :: Tagged IgnoreLineOption String
  optionHelp = return "Ignore produced lines that match the regular expression."

newtype IgnoreFileOption = IgnoreFileOption [IgnoreFile]
  deriving (Eq, Ord, Semigroup, Monoid, Typeable)

ignoreFileOption :: [String] -> SomeOption
ignoreFileOption ls = appendOption (IgnoreFileOption (fromString <$> ls))

ignoreFileOptionIngredient :: Ingredient
ignoreFileOptionIngredient = includingOptions [Option (Proxy :: Proxy IgnoreFileOption)]

instance Read IgnoreFileOption where
  readsPrec :: Int -> ReadS IgnoreFileOption
  readsPrec _prec s = case traverse safeRead (splitOn (== ',') s) of
    Just opts -> [(IgnoreFileOption opts, "")]
    Nothing -> []

instance IsString IgnoreFileOption where
  fromString :: String -> IgnoreFileOption
  fromString s = case safeRead s of
    Just opt -> opt
    Nothing -> error $ printf "Cannot parse ignore file option '%s'" s

instance IsOption IgnoreFileOption where
  defaultValue :: IgnoreFileOption
  defaultValue = IgnoreFileOption []

  parseValue :: String -> Maybe IgnoreFileOption
  parseValue = safeRead

  optionName :: Tagged IgnoreFileOption String
  optionName = return "ignore-files"

  optionHelp :: Tagged IgnoreFileOption String
  optionHelp = return "Ignore produced files that match the pattern."

-- * Conversion to/from JSON

instance FromJSON Ignore where
  parseJSON :: Value -> Parser Ignore
  parseJSON = withObject "ignore" $ \o -> do
    -- Parse "lines"
    let parseIgnoreLinesOption = fromStringOrStringArray $ \txt ->
          case safeRead (Text.unpack txt) of
            Just opt -> return opt
            Nothing -> fail $ printf "Could not parse lines: '%s'"
    ignoreLines <-
      o .:? "lines"
        >>= maybe (return []) parseIgnoreLinesOption

    -- Parse "files"
    let parseIgnoreFilesOption = fromStringOrStringArray $ \txt ->
          case safeRead (Text.unpack txt) of
            Just opt -> return opt
            Nothing -> fail $ printf "Could not parse files: '%s'"
    ignoreFiles <-
      o .:? "files"
        >>= maybe (return []) parseIgnoreFilesOption
    return $ Ignore ignoreLines ignoreFiles

instance ToJSON Ignore where
  toJSON :: Ignore -> Value
  toJSON Ignore {..} =
    object
      [ "lines" .= ignoreLines,
        "files" .= ignoreFiles
      ]

instance FromJSON IgnoreLine where
  parseJSON :: Value -> Parser IgnoreLine
  parseJSON (Value.String regexText) =
    case Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt regexText of
      Left compileError -> fail $ "Failed to parse regular expression 'matches': " <> compileError
      Right regex -> return $ IgnoreLine regexText regex
  parseJSON v = typeMismatch "String" v

instance ToJSON IgnoreLine where
  toJSON :: IgnoreLine -> Value
  toJSON IgnoreLine {..} = Value.String ignoreLineRegexText
