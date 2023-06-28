{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Test.Golden.TestSpec.External where

import Control.Applicative (Alternative (..))
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Data (Typeable)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Tagged (Tagged, untag)
import Options.Applicative (help, long, maybeReader, option)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (IsOption (..), OptionDescription (..))
import Vehicle.Test.Golden.Extra (SomeOption (..))

newtype External = External {unExternal :: FilePath}
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, IsString)

newtype ExternalOption = ExternalOption {unExternalOption :: Set External}
  deriving (Eq, Ord, Show, Typeable, Semigroup, Monoid)

externalOption :: [String] -> SomeOption
externalOption ls = SomeOption (ExternalOption (Set.fromList (fromString <$> ls)))

externalOptionIngredient :: Ingredient
externalOptionIngredient = includingOptions [Option (Proxy :: Proxy ExternalOption)]

instance IsOption ExternalOption where
  defaultValue = ExternalOption []
  parseValue = Just . ExternalOption . Set.singleton . fromString
  optionName = return "external"
  optionHelp = return "Run tests with an external dependency that matches the argument."
  optionCLParser =
    mconcat
      <$> many
        ( option
            (maybeReader parseValue)
            ( long (untag (optionName :: Tagged ExternalOption String))
                <> help (untag (optionHelp :: Tagged ExternalOption String))
            )
        )
