module SemanticTokens.Binders where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (OverlapMode (..), as, expectTokens)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

test_tokensBinders :: TestTree
test_tokensBinders =
  testGroup "binders" $
    [ testCase "name-only binder" $
        expectTokens
          [ "foo" `as` (SemanticTokenTypes_Function, []),
            " ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            "\n"
          ]
          AllowLaxOverlaps,
      testCase "name-and-type binder" $
        expectTokens
          [ "foo" `as` (SemanticTokenTypes_Function, []),
            " (",
            "x" `as` (SemanticTokenTypes_Variable, []),
            " : ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            ") = ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            "\n"
          ]
          AllowStrictOverlaps
    ]
