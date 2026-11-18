module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMainWithIngredients)
import Tests qualified

main :: IO ()
main = do
  setLocaleEncoding utf8
  Tests.tests >>= defaultMainWithIngredients Tests.ingredients
