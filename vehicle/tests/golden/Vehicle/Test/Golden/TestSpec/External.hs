{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Test.Golden.TestSpec.External where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Data (Typeable)
import Data.Foldable (Foldable (toList))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Tagged (Tagged, untag)
import Options.Applicative (help, long, maybeReader, option)
import Options.Applicative.NonEmpty (some1)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)

externalOptionIngredient :: Ingredient
externalOptionIngredient =
  includingOptions
    [ Option (Proxy :: Proxy ExternalOption),
      Option (Proxy :: Proxy ExternalOnlyOption)
    ]

newtype External = External {unExternal :: FilePath}
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, IsString)

newtype ExternalOption = ExternalOption {unExternalOption :: Set External}
  deriving (Eq, Ord, Show, Typeable, Semigroup, Monoid)

instance IsOption ExternalOption where
  defaultValue = ExternalOption []
  parseValue = Just . ExternalOption . Set.singleton . fromString
  optionName = return "external"
  optionHelp = return "Run tests with an external dependency that matches the argument."
  optionCLParser =
    mconcat . toList
      <$> some1
        ( option
            (maybeReader parseValue)
            ( long (untag (optionName :: Tagged ExternalOption String))
                <> help (untag (optionHelp :: Tagged ExternalOption String))
            )
        )

newtype ExternalOnlyOption = ExternalOnlyOption {unExternalOnlyOption :: Bool}

instance IsOption ExternalOnlyOption where
  defaultValue = ExternalOnlyOption False
  parseValue = fmap ExternalOnlyOption . safeRead
  optionName = return "external-only"
  optionHelp = return "Run only tests with the specified external dependencies."
