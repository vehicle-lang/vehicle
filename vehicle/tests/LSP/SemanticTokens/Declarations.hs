module SemanticTokens.Declarations where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (OverlapMode (..), as, expectTokens, xas)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

test_tokensDeclarations :: TestTree
test_tokensDeclarations =
  testGroup "declarations" $
    [ testCase "single-argument function" $
        expectTokens
          [ "foo" `as` (SemanticTokenTypes_Function, []),
            " : ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            " -> ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "foo" `xas` (SemanticTokenTypes_Function, []),
            " ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            "\n"
          ]
          AllowStrictOverlaps,
      testCase "@ annotations" $
        expectTokens
          [ "@parameter" `xas` (SemanticTokenTypes_Decorator, []),
            "\n",
            "p" `as` (SemanticTokenTypes_Variable, []),
            " : ",
            "Nat" `as` (SemanticTokenTypes_Type, []),
            "\n"
          ]
          NoOverlaps,
      testCase "type aliases" $
        expectTokens
          [ "type" `xas` (SemanticTokenTypes_Keyword, []),
            " ",
            "Alias" `as` (SemanticTokenTypes_Type, []),
            " = ",
            "Nat" `as` (SemanticTokenTypes_Type, []),
            "\n"
          ]
          NoOverlaps
    ]
