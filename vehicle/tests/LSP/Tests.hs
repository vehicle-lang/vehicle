module Tests
  ( ingredients,
    tests,
  )
where

import SemanticTokens.Simple (test_tokensSimple)
import Session.Smoke (test_sessionSmoke)
import Test.Tasty (TestTree, defaultIngredients, testGroup)
import Test.Tasty.Ingredients (Ingredient)

tests :: IO TestTree
tests =
  pure $
    testGroup
      "Tests"
      [ test_sessionSmoke,
        test_tokensSimple
      ]

ingredients :: [Ingredient]
ingredients = defaultIngredients
