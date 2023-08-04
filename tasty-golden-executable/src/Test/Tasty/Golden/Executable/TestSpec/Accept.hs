module Test.Tasty.Golden.Executable.TestSpec.Accept
  ( Accept,
    acceptOptionIngredient,
  )
where

import Data.Data (Typeable)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import Options.Applicative.Types qualified as Options
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), flagCLParser, safeReadBool)

acceptOptionIngredient :: Ingredient
acceptOptionIngredient =
  includingOptions
    [ Option (Proxy :: Proxy Accept)
    ]

newtype Accept = Accept {unAccept :: Bool}
  deriving (Eq, Ord, Show, Typeable)

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
