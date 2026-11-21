module Tests
  ( ingredients,
    tests,
  )
where

import SemanticTokens.Binders (test_tokensBinders)
import SemanticTokens.Declarations (test_tokensDeclarations)
import SemanticTokens.EdgeCases (test_tokenEdgeCases)
import SemanticTokens.Simple (test_tokensSimple)
import SemanticTokens.TokenTypes (test_tokenTypes)
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
        test_tokensBinders,
        test_tokensDeclarations,
        test_tokenTypes,
        test_tokenEdgeCases
      ]

ingredients :: [Ingredient]
ingredients = defaultIngredients
