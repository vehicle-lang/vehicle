module SemanticTokens.EdgeCases where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (OverlapMode (..), as, expectTokens, xas)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

test_tokenEdgeCases :: TestTree
test_tokenEdgeCases =
  testGroup "edge cases" $
    [ testCase "multi-token syntax" $
        expectTokens
          [ "choose" `as` (SemanticTokenTypes_Function, []),
            " : ",
            "Bool" `as` (SemanticTokenTypes_Type, []),
            " -> ",
            "List" `as` (SemanticTokenTypes_Type, []),
            " ",
            "Nat" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "choose" `xas` (SemanticTokenTypes_Function, []),
            " ",
            "condition" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "let" `as` (SemanticTokenTypes_Keyword, []),
            " ",
            "singleton" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "[" `as` (SemanticTokenTypes_Keyword, []),
            "1" `as` (SemanticTokenTypes_Number, []),
            "]" `xas` (SemanticTokenTypes_Keyword, []),
            " ",
            "in" `xas` (SemanticTokenTypes_Keyword, []),
            "\n  ",
            "if" `as` (SemanticTokenTypes_Keyword, []),
            " ",
            "condition" `as` (SemanticTokenTypes_Variable, []),
            " ",
            "then" `xas` (SemanticTokenTypes_Keyword, []),
            " ",
            "singleton" `as` (SemanticTokenTypes_Variable, []),
            " ",
            "else" `xas` (SemanticTokenTypes_Keyword, []),
            " ",
            "[" `as` (SemanticTokenTypes_Keyword, []),
            "]" `xas` (SemanticTokenTypes_Keyword, []),
            "\n"
          ]
          AllowStrictOverlaps,
      -- This one is awkward, because `forallT` is only a keyword currently because it is
      -- misidentified as a lambda's `\` token. But eventually we want to classify it as a keyword
      -- for the correct reason.
      testCase "forallT" $
        expectTokens
          [ "identity1" `as` (SemanticTokenTypes_Function, []),
            " : ",
            "forallT" `xas` (SemanticTokenTypes_Keyword, []),
            " {",
            "t" `as` (SemanticTokenTypes_Type, []),
            " : ",
            "Type" `as` (SemanticTokenTypes_Type, []),
            "} . ",
            "t" `as` (SemanticTokenTypes_Type, []),
            " -> ",
            "t" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "identity1" `xas` (SemanticTokenTypes_Function, []),
            " {",
            "t" `as` (SemanticTokenTypes_Type, []),
            "} ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            "\n\n",
            "identity2" `as` (SemanticTokenTypes_Function, []),
            " : ",
            "forallT" `as` (SemanticTokenTypes_Keyword, []),
            " {",
            "t" `as` (SemanticTokenTypes_Type, []),
            " : ",
            "Type" `as` (SemanticTokenTypes_Type, []),
            "} . ",
            "t" `as` (SemanticTokenTypes_Type, []),
            " -> ",
            "t" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "identity2" `xas` (SemanticTokenTypes_Function, []),
            " ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            "\n"
          ]
          AllowLaxOverlaps
    ]
