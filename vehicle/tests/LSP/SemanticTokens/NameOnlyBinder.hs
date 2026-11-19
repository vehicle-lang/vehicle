module SemanticTokens.NameOnlyBinder where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (ExpectedTokens, OverlapMode (..), as, expectTokens)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

specification :: ExpectedTokens
specification =
  [ "foo" `as` (SemanticTokenTypes_Function, []),
    " ",
    "x" `as` (SemanticTokenTypes_Variable, []),
    " = ",
    "x" `as` (SemanticTokenTypes_Variable, []),
    "\n"
  ]

test_tokensNameOnlyBinder :: TestTree
test_tokensNameOnlyBinder =
  testCase "name-only binder" $
    expectTokens specification AllowLaxOverlaps
