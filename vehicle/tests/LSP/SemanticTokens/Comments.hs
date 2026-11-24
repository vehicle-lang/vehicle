module SemanticTokens.Comments where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (OverlapMode (..), as, expectTokens)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

test_tokensComments :: TestTree
test_tokensComments =
  testGroup "comments" $
    [ testCase "single line with comment" $
        expectTokens
          [ "-- test" `as` (SemanticTokenTypes_Comment, []),
            "\n"
          ]
          NoOverlaps,
      testCase "single line multiline comment" $
        expectTokens
          [ "   ",
            "{- hello world -}" `as` (SemanticTokenTypes_Comment, []),
            "   \n"
          ]
          NoOverlaps,
      testCase "multiple lines multiline comment" $
        expectTokens
          [ "   ",
            "{- hello" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "  to the  " `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "world -}" `as` (SemanticTokenTypes_Comment, []),
            "   \n"
          ]
          NoOverlaps,
      testCase "many comments" $
        expectTokens
          [ "-- this is a single-line comment" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "{-" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "and this is" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "a multi-line" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "comment -}" `as` (SemanticTokenTypes_Comment, []),
            "\n\n",
            "foo" `as` (SemanticTokenTypes_Variable, []),
            " = ",
            "True" `as` (SemanticTokenTypes_Keyword, []),
            "  ",
            "-- this is an inline comment" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "bar" `as` (SemanticTokenTypes_Variable, []),
            " ",
            "{- comments can -}" `as` (SemanticTokenTypes_Comment, []),
            " = ",
            "{- go anywhere! -}" `as` (SemanticTokenTypes_Comment, []),
            "False" `as` (SemanticTokenTypes_Keyword, []),
            "\n"
          ]
          NoOverlaps,
      testCase "multiline comment without end" $
        expectTokens
          [ "   ",
            "{- hello" `as` (SemanticTokenTypes_Comment, []),
            "\n",
            "  world  " `as` (SemanticTokenTypes_Comment, []),
            "\n"
          ]
          NoOverlaps
    ]
