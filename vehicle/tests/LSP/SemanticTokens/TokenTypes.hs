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
          [ "usesMap" `as` (SemanticTokenTypes_Function, []),
            " : ",
            "HasMap" `as` (SemanticTokenTypes_Class, []),
            " ",
            "List" `as` (SemanticTokenTypes_Type, []),
            " -> ",
            "Bool" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "usesMap" `eventually` (SemanticTokenTypes_Function, []),
            " ",
            "evidence" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "True" `as` (SemanticTokenTypes_Keyword, []),
            "\n"
          ]
          AllowStrictOverlaps,
      testCase "record fields and projections are methods" $
        expectTokens
          [ "record" `eventually` (SemanticTokenTypes_Keyword, []),
            " ",
            "Pair" `as` (SemanticTokenTypes_Type, []),
            " ",
            "where" `eventually` (SemanticTokenTypes_Keyword, []),
            "\n  { ",
            "left" `as` (SemanticTokenTypes_Method, []),
            " : ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n  , ",
            "right" `as` (SemanticTokenTypes_Method, []),
            " : ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n  }\n\n",
            "pair" `as` (SemanticTokenTypes_Variable, []),
            " : ",
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
            " : ",
            "Pair" `as` (SemanticTokenTypes_Type, []),
            " -> ",
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
          AllowStrictOverlaps,
      testCase "operator" $
        expectTokens
          [ "useAt" `as` (SemanticTokenTypes_Function, []),
            " : ",
            "Vector" `as` (SemanticTokenTypes_Type, []),
            " ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            " ",
            "5" `as` (SemanticTokenTypes_Number, []),
            " -> ",
            "Real" `as` (SemanticTokenTypes_Type, []),
            "\n",
            "useAt" `eventually` (SemanticTokenTypes_Function, []),
            " ",
            "v" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "v" `as` (SemanticTokenTypes_Variable, []),
            "!" `as` (SemanticTokenTypes_Operator, []),
            "3" `as` (SemanticTokenTypes_Number, []),
            "\n"
          ]
          AllowStrictOverlaps,
      testCase "comments" $
        expectTokens
          [ "-- this is a single-line comment\n" `eventually` (SemanticTokenTypes_Comment, []),
            "{-\nand this is\n\na multi-line\ncomment -}" `eventually` (SemanticTokenTypes_Comment, []),
            "\n\n",
            "foo" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "True" `as` (SemanticTokenTypes_Keyword, []),
            "  ",
            "-- this is an inline comment\n" `eventually` (SemanticTokenTypes_Comment, []),
            "bar" `as` (SemanticTokenTypes_Variable, []),
            " ",
            "{- comments can -}" `eventually` (SemanticTokenTypes_Comment, []),
            " = ",
            "{- go anywhere! -}" `eventually` (SemanticTokenTypes_Comment, []),
            "False" `as` (SemanticTokenTypes_Keyword, [])
          ]
          NoOverlaps
    ]
