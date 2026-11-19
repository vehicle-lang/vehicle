module Tests
  ( ingredients,
    tests,
  )
where

import SemanticTokens.NameAndTypeBinder (test_tokensNameAndTypeBinder)
import SemanticTokens.NameOnlyBinder (test_tokensNameOnlyBinder)
import SemanticTokens.Simple (test_tokensSimple)
import Session.Smoke (test_sessionSmoke)
import Test.Tasty (TestTree, defaultIngredients, testGroup)
import Test.Tasty.Ingredients (Ingredient)

tests :: IO TestTree
tests =
  pure $
    testGroup
      "LSP Tests"
      [ test_sessionSmoke,
        test_tokensSimple,
        test_tokensNameOnlyBinder,
        test_tokensNameAndTypeBinder
      ]

ingredients :: [Ingredient]
ingredients = defaultIngredients
