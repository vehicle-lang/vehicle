module Tests
  ( ingredients,
    tests,
  )
where

import SemanticTokens.Binders (test_tokensBinders)
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
        test_tokensBinders
      ]

ingredients :: [Ingredient]
ingredients = defaultIngredients
