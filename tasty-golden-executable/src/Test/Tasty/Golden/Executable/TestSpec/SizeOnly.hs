{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.TestSpec.SizeOnly
  ( SizeOnlyExtensions (..),
    toSizeOnlyExtensionsSet,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value)
import Data.Set (Set)
import Data.Set qualified as Set (fromList)
import Data.Tagged (Tagged)
import Data.Text qualified as Text
import Test.Tasty.Options (IsOption (..), safeRead)

-- SizeOnlyExtensions

newtype SizeOnlyExtensions = SizeOnlyExtensions
  { fileExtensions :: [FilePath]
  }
  deriving (Eq, Show, Semigroup, Monoid)

instance FromJSON SizeOnlyExtensions where
  parseJSON :: Value -> Parser SizeOnlyExtensions
  parseJSON v = parse1 v <|> parseN v
    where
      parse1 = fmap (SizeOnlyExtensions . (: [])) . parseJSON
      parseN = fmap SizeOnlyExtensions . parseJSON

instance ToJSON SizeOnlyExtensions where
  toJSON :: SizeOnlyExtensions -> Value
  toJSON (SizeOnlyExtensions [ignoreFile]) = toJSON ignoreFile
  toJSON (SizeOnlyExtensions ignoreFiles) = toJSON ignoreFiles

instance IsOption SizeOnlyExtensions where
  defaultValue :: SizeOnlyExtensions
  defaultValue = mempty

  parseValue :: String -> Maybe SizeOnlyExtensions
  parseValue input = SizeOnlyExtensions <$> traverse safeRead filePatternStrings
    where
      filePatternStrings = Text.unpack . Text.strip <$> Text.splitOn "," (Text.pack input)

  optionName :: Tagged SizeOnlyExtensions String
  optionName = return "sizeOnly"

  optionHelp :: Tagged SizeOnlyExtensions String
  optionHelp = return "A list of file extensions for which diffs should only display the sizes of the old and new files."

toSizeOnlyExtensionsSet :: SizeOnlyExtensions -> Set String
toSizeOnlyExtensionsSet (SizeOnlyExtensions exts) = Set.fromList exts
