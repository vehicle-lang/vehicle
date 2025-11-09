{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Proxy (Proxy (..))
import Data.Tagged
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions)
import Test.Tasty.Golden.Executable (IgnoreFiles (..), SomeOption (..), makeTestTreeFromDirectoryRecursive)
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Text.Read (readMaybe)
import Vehicle.Prelude.Logging

vehicleLoggingIngredient :: Ingredient
vehicleLoggingIngredient =
  includingOptions [Option (Proxy :: Proxy LoggingLevel)]

instance IsOption LoggingLevel where
  defaultValue :: LoggingLevel
  defaultValue = defaultLoggingLevel

  parseValue :: String -> Maybe LoggingLevel
  parseValue = readMaybe

  optionName :: Tagged LoggingLevel String
  optionName = Tagged "vehicle-logging"

  optionHelp :: Tagged LoggingLevel String
  optionHelp = Tagged "set the logging level"

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

options :: [SomeOption]
options =
  [ AppendOption $ IgnoreFiles ["*.vclo", "**/*.vclo", "**/.vcl-cache-index"]
  ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive options "Compiler" testDirectory
  defaultMainWithIngredients
    (vehicleLoggingIngredient : defaultIngredients)
    testTree
