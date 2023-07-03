{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.Golden.Executable.TestSpec.External where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Data (Typeable)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative.Types qualified as Options (Parser)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), flagCLParser, safeReadBool)

externalOptionIngredient :: Ingredient
externalOptionIngredient =
  includingOptions
    [ Option (Proxy :: Proxy ExternalOption),
      Option (Proxy :: Proxy ExternalOnlyOption)
    ]

newtype External = External {unExternal :: Text}
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, IsString)

newtype ExternalOption = ExternalOption {unExternalOption :: Set External}
  deriving (Eq, Ord, Show, Typeable, Semigroup, Monoid)

instance IsOption ExternalOption where
  defaultValue :: ExternalOption
  defaultValue = ExternalOption Set.empty

  parseValue :: String -> Maybe ExternalOption
  parseValue value =
    Just $
      ExternalOption $
        Set.fromList
          [ External (Text.strip external)
            | external <- Text.splitOn "," (Text.pack value)
          ]

  optionName :: Tagged ExternalOption String
  optionName = return "external"

  optionHelp :: Tagged ExternalOption String
  optionHelp = return "Run tests with an external dependency that matches the argument."

newtype ExternalOnlyOption = ExternalOnlyOption {unExternalOnlyOption :: Bool}

instance IsOption ExternalOnlyOption where
  defaultValue :: ExternalOnlyOption
  defaultValue = ExternalOnlyOption False

  parseValue :: String -> Maybe ExternalOnlyOption
  parseValue = fmap ExternalOnlyOption . safeReadBool

  optionName :: Tagged ExternalOnlyOption String
  optionName = return "external-only"

  optionHelp :: Tagged ExternalOnlyOption String
  optionHelp = return "Run only tests with the specified external dependencies."

  optionCLParser :: Options.Parser ExternalOnlyOption
  optionCLParser = flagCLParser Nothing (ExternalOnlyOption True)
