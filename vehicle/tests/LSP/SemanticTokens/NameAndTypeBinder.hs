module SemanticTokens.NameAndTypeBinder where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (ExpectedTokens, OverlapMode (..), as, expectTokens)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

specification :: ExpectedTokens
specification =
  [ "foo" `as` (SemanticTokenTypes_Function, []),
    " (",
    "x" `as` (SemanticTokenTypes_Variable, []),
    " : ",
    "Real" `as` (SemanticTokenTypes_Type, []),
    ") = ",
    "x" `as` (SemanticTokenTypes_Variable, []),
    "\n"
  ]

test_tokensNameAndTypeBinder :: TestTree
test_tokensNameAndTypeBinder =
  testCase "name-and-type binder" $
    expectTokens specification AllowStrictOverlaps
