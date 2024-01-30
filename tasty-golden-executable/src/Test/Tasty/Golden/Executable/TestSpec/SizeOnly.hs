{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.TestSpec.SizeOnly
  ( SizeOnlyExtension (..),
    SizeOnlyExtensions (..),
    toSizeOnlyExtensionsSet,
  )
where

import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, typeMismatch)
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Data (Typeable)
import Data.Set (Set)
import Data.Set qualified as Set (fromList)
import Data.String (IsString (..))
import Data.Tagged (Tagged)
import Data.Text qualified as Text
import Test.Tasty.Options (IsOption (..), safeRead)

-- | Extension for which only diffs should show the size.
newtype SizeOnlyExtension = SizeOnlyExtension {extension :: FilePath}
  deriving (Eq, Ord, Typeable)

instance Show SizeOnlyExtension where
  show :: SizeOnlyExtension -> String
  show (SizeOnlyExtension programName) = programName

instance Read SizeOnlyExtension where
  readsPrec :: Int -> ReadS SizeOnlyExtension
  readsPrec _prec programName = [(SizeOnlyExtension programName, "")]

instance IsString SizeOnlyExtension where
  fromString :: String -> SizeOnlyExtension
  fromString = SizeOnlyExtension

instance FromJSON SizeOnlyExtension where
  parseJSON :: Value -> Parser SizeOnlyExtension
  parseJSON (Value.String name) = return $ SizeOnlyExtension (Text.unpack name)
  parseJSON value = typeMismatch "String" value

instance ToJSON SizeOnlyExtension where
  toJSON :: SizeOnlyExtension -> Value
  toJSON = toJSON . extension

newtype SizeOnlyExtensions = SizeOnlyExtensions [SizeOnlyExtension]
  deriving (Eq, Ord, Show, Typeable, Semigroup, Monoid)

instance IsOption SizeOnlyExtensions where
  defaultValue :: SizeOnlyExtensions
  defaultValue = mempty

  parseValue :: String -> Maybe SizeOnlyExtensions
  parseValue input = SizeOnlyExtensions <$> traverse safeRead names
    where
      names = Text.unpack . Text.strip <$> Text.splitOn "," (Text.pack input)

  optionName :: Tagged SizeOnlyExtensions String
  optionName = return "sizeOnly"

  optionHelp :: Tagged SizeOnlyExtensions String
  optionHelp = return "A list of file extensions for which diffs should only display the sizes of the old and new files."

toSizeOnlyExtensionsSet :: SizeOnlyExtensions -> Set String
toSizeOnlyExtensionsSet (SizeOnlyExtensions exts) = Set.fromList (fmap extension exts)
