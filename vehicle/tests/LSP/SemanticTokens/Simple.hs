module SemanticTokens.Simple where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (ExpectedTokens, OverlapMode (..), as, expectTokens)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

specification :: ExpectedTokens
specification =
  [ "foo" `as` (SemanticTokenTypes_Variable, []),
    " = ",
    "42" `as` (SemanticTokenTypes_Number, []),
    "\n",
    "bar" `as` (SemanticTokenTypes_Variable, []),
    " = ",
    "True" `as` (SemanticTokenTypes_Keyword, []),
    "\n"
  ]

test_tokensSimple :: TestTree
test_tokensSimple =
  testCase "produces tokens for a simple example" $
    expectTokens specification NoOverlaps
