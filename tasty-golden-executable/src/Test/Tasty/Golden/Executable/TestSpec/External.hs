{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.TestSpec.External
  ( External (..),
    AllowlistExternals (..),
  )
where

import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, typeMismatch)
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Data (Typeable)
import Data.String (IsString (..))
import Data.Tagged (Tagged)
import Data.Text qualified as Text
import Test.Tasty.Options (IsOption (..), safeRead)

-- | The name of an external program.
newtype External = External {programName :: FilePath}
  deriving (Eq, Ord, Typeable)

instance Show External where
  show :: External -> String
  show (External programName) = programName

instance Read External where
  readsPrec :: Int -> ReadS External
  readsPrec _prec programName = [(External programName, "")]

instance IsString External where
  fromString :: String -> External
  fromString = External

instance FromJSON External where
  parseJSON :: Value -> Parser External
  parseJSON (Value.String name) = return $ External (Text.unpack name)
  parseJSON value = typeMismatch "String" value

instance ToJSON External where
  toJSON :: External -> Value
  toJSON = toJSON . programName

newtype AllowlistExternals = AllowlistExternals [External]
  deriving (Eq, Ord, Show, Typeable, Semigroup, Monoid)

instance IsOption AllowlistExternals where
  defaultValue :: AllowlistExternals
  defaultValue = mempty

  parseValue :: String -> Maybe AllowlistExternals
  parseValue input = AllowlistExternals <$> traverse safeRead names
    where
      names = Text.unpack . Text.strip <$> Text.splitOn "," (Text.pack input)

  optionName :: Tagged AllowlistExternals String
  optionName = return "allowlist-externals"

  optionHelp :: Tagged AllowlistExternals String
  optionHelp = return "A list of allowed external programs."
