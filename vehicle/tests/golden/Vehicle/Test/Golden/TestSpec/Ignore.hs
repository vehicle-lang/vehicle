{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Test.Golden.TestSpec.Ignore
  ( Ignore (..),
    IgnoreLine,
    IgnoreLineOption (..),
    ignoreLineOption,
    ignoreLineOptionIngredient,
    IgnoreFile,
    IgnoreFileOption (..),
    ignoreFileOption,
    ignoreFileOptionIngredient,
    matchFile,
    matchLine,
    Vehicle.Test.Golden.TestSpec.Ignore.null,
  )
where

import Control.Monad ((<=<))
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
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative (help, long, option, str)
import Options.Applicative.NonEmpty (some1)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (IsOption (..), OptionDescription (..))
import Text.Printf (printf)
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.Regex.TDFA.Text qualified as Regex
import Vehicle.Test.Golden.Extra (SomeOption (..))
import Vehicle.Test.Golden.TestSpec.FilePattern (FilePattern)
import Vehicle.Test.Golden.TestSpec.FilePattern qualified as FilePattern

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

instance IsString IgnoreLine where
  fromString :: String -> IgnoreLine
  fromString s = case parseIgnoreLine (Text.pack s) of
    Just opt -> opt
    Nothing -> error $ printf "Cannot regular expression '%s'" s

-- | Type of file patterns for files to ignore in the comparison.
newtype IgnoreFile = IgnoreFile FilePattern
  deriving (Show, Eq, Ord, Typeable, FromJSON, ToJSON)

instance IsString IgnoreFile where
  fromString :: String -> IgnoreFile
  fromString s = case parseIgnoreFile s of
    Just opt -> opt
    Nothing -> error $ printf "Cannot file pattern '%s'" s

-- * Basic

null :: Maybe Ignore -> Bool
null = maybe True (\Ignore {..} -> Prelude.null ignoreLines && Prelude.null ignoreFiles)

-- | Check if a file should be ignored.
matchFile :: IgnoreFile -> FilePath -> Bool
matchFile (IgnoreFile filePattern) = FilePattern.match filePattern

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
ignoreLineOption ls = SomeOption (IgnoreLineOption (fromString <$> ls))

ignoreLineOptionIngredient :: Ingredient
ignoreLineOptionIngredient = includingOptions [Option (Proxy :: Proxy IgnoreLineOption)]

newtype IgnoreLineOption = IgnoreLineOption [IgnoreLine]
  deriving (Eq, Ord, Semigroup, Monoid, Typeable)

instance IsString IgnoreLineOption where
  fromString :: String -> IgnoreLineOption
  fromString s = case parseIgnoreLineOption (Text.pack s) of
    Just opt -> opt
    Nothing -> error $ printf "Cannot parse ignore line option '%s'" s

instance IsOption IgnoreLineOption where
  defaultValue = IgnoreLineOption []
  parseValue = parseIgnoreLineOption . Text.pack
  optionName = return "ignore-lines"
  optionHelp :: Tagged IgnoreLineOption String
  optionHelp = return "Ignore produced lines that match the regular expression."
  optionCLParser =
    mconcat . toList
      <$> some1
        ( option
            (parseIgnoreLineOption =<< str)
            ( long (untag (optionName :: Tagged IgnoreLineOption String))
                <> help (untag (optionHelp :: Tagged IgnoreLineOption String))
            )
        )

parseIgnoreLineOption :: (MonadFail m) => Text -> m IgnoreLineOption
parseIgnoreLineOption = return . IgnoreLineOption . (: []) <=< parseIgnoreLine

parseIgnoreLine :: (MonadFail m) => Text -> m IgnoreLine
parseIgnoreLine regexText =
  case Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt regexText of
    Left compileError -> fail $ "Failed to parse regular expression 'matches': " <> compileError
    Right regex -> return $ IgnoreLine regexText regex

newtype IgnoreFileOption = IgnoreFileOption [IgnoreFile]
  deriving (Eq, Ord, Semigroup, Monoid, Typeable)

ignoreFileOption :: [String] -> SomeOption
ignoreFileOption ls = SomeOption (IgnoreFileOption (fromString <$> ls))

ignoreFileOptionIngredient :: Ingredient
ignoreFileOptionIngredient = includingOptions [Option (Proxy :: Proxy IgnoreFileOption)]

instance IsString IgnoreFileOption where
  fromString :: String -> IgnoreFileOption
  fromString s = case parseIgnoreFileOption s of
    Just opt -> opt
    Nothing -> error $ printf "Cannot parse ignore file option '%s'" s

instance IsOption IgnoreFileOption where
  defaultValue = IgnoreFileOption []
  parseValue = parseIgnoreFileOption
  optionName = return "ignore-files"
  optionHelp = return "Ignore produced files that match the pattern."
  optionCLParser =
    mconcat . toList
      <$> some1
        ( option
            (parseIgnoreFileOption =<< str)
            ( long (untag (optionName :: Tagged IgnoreFileOption String))
                <> help (untag (optionHelp :: Tagged IgnoreFileOption String))
            )
        )

parseIgnoreFileOption :: (MonadFail m) => String -> m IgnoreFileOption
parseIgnoreFileOption = return . IgnoreFileOption . (: []) <=< parseIgnoreFile

parseIgnoreFile :: (MonadFail m) => String -> m IgnoreFile
parseIgnoreFile patternString =
  either fail (return . IgnoreFile) $ FilePattern.readEither patternString

-- * Conversion to/from JSON

instance FromJSON Ignore where
  parseJSON :: Value -> Parser Ignore
  parseJSON = withObject "ignore" $ \o -> do
    ignoreLines <-
      o .:? "lines"
        >>= maybe (return []) (fromStringOrStringArray parseIgnoreLine)
    ignoreFiles <-
      o .:? "files"
        >>= maybe (return []) (fromStringOrStringArray (parseIgnoreFile . Text.unpack))
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

-- | Parse values from either a String or an Array of Strings.
fromStringOrStringArray :: (Text -> Parser a) -> Value -> Parser [a]
fromStringOrStringArray textParser = \case
  Value.String text -> (: []) <$> textParser text
  Value.Array items -> traverse valueParser (toList items)
  v -> typeMismatch "String or Array" v
  where
    valueParser (Value.String text) = textParser text
    valueParser v = typeMismatch "String" v
