module Tests
  ( ingredients,
    tests,
  )
where

import Session.Smoke (test_sessionSmoke)
import Test.Tasty (TestTree, defaultIngredients, testGroup)
import Test.Tasty.Ingredients (Ingredient)

tests :: IO TestTree
tests =
  pure $
    testGroup
      "Tests"
      [ test_sessionSmoke
      ]

ingredients :: [Ingredient]
ingredients = defaultIngredients
