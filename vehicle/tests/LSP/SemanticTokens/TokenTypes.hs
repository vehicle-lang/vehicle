module SemanticTokens.TokenTypes where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (OverlapMode (..), as, eventually, expectTokens)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

test_tokenTypes :: TestTree
test_tokenTypes =
  testGroup "token types" $
    [ testCase "class" $
        expectTokens
          [ "something" `as` (SemanticTokenTypes_Function, []),
            " ",
            ":" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "NonCastingReal" `as` (SemanticTokenTypes_Class, []),
            " ",
            "->" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "Bool" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "something" `eventually` (SemanticTokenTypes_Function, []),
            " ",
            "x" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "True" `as` (SemanticTokenTypes_Keyword, []),
            "\n"
          ]
          NoOverlaps,
      testCase "record fields and projections are methods" $
        expectTokens
          [ "record" `as` (SemanticTokenTypes_Keyword, []),
            " ",
            "Pair" `as` (SemanticTokenTypes_Type, []),
            " ",
            "where" `as` (SemanticTokenTypes_Keyword, []),
            "\n  { ",
            "left" `as` (SemanticTokenTypes_Method, []),
            " ",
            ":" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n  , ",
            "right" `as` (SemanticTokenTypes_Method, []),
            " ",
            ":" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n  }\n\n",
            "pair" `as` (SemanticTokenTypes_Variable, []),
            " ",
            ":" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "Pair" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "pair" `eventually` (SemanticTokenTypes_Variable, []),
            " = { ",
            "left" `as` (SemanticTokenTypes_Method, []),
            " = ",
            "1" `as` (SemanticTokenTypes_Number, []),
            ", ",
            "right" `as` (SemanticTokenTypes_Method, []),
            " = ",
            "2" `as` (SemanticTokenTypes_Number, []),
            " }\n\n",
            "getLeft" `as` (SemanticTokenTypes_Function, []),
            " ",
            ":" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "Pair" `as` (SemanticTokenTypes_Type, []),
            " ",
            "->" `as` (SemanticTokenTypes_Operator, []),
            " ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "getLeft" `eventually` (SemanticTokenTypes_Function, []),
            " ",
            "p" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "p" `as` (SemanticTokenTypes_Variable, []),
            ".left" `as` (SemanticTokenTypes_Method, []),
            "\n"
          ]
          AllowStrictOverlaps
    ]
