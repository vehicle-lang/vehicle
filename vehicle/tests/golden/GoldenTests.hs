module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultIngredients, defaultMainWithIngredients)
import Test.Tasty.Ingredients (Ingredient)
import Vehicle.Test.Golden (SomeOption (..), externalOptionIngredient, ignoreFileOption, makeTestTreeFromDirectoryRecursive)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

options :: [SomeOption]
options =
  [ ignoreFileOption ["*.vclo", "**/*.vclo"]
  ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive options "Compiler" testDirectory
  defaultMainWithIngredients ingredients testTree

ingredients :: [Ingredient]
ingredients = externalOptionIngredient : defaultIngredients
