module Test.Tasty.Golden.Executable.TestSpec.Accept
  ( Accept,
  )
where

import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, typeMismatch)
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text

acceptOptionIngredient :: Ingredient
acceptOptionIngredient =
  includingOptions
    [ Option (Proxy :: Proxy Accept)
    ]

newtype Accept = Accept {unAccept :: Bool}
  deriving (Eq, Ord, Show, Typeable, Semigroup, Monoid)

instance IsOption Accept where
  defaultValue :: Accept
  defaultValue = Accept False

  parseValue :: String -> Maybe Accept
  parseValue = fmap Accept . safeReadBool

  optionName :: Tagged Accept String
  optionName = return "accept"

  optionHelp :: Tagged Accept String
  optionHelp = return "Update the golden files."

  optionCLParser :: Options.Parser Accept
  optionCLParser = flagCLParser Nothing (Accept True)
