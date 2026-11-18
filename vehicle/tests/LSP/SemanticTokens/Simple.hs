module SemanticTokens.Simple where

import Language.LSP.Protocol.Types (SemanticTokenTypes (..))
import SemanticTokens.Common (expectTokens, tokenAt)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

specification :: String
specification = "foo = 42\n"

test_tokensSimple :: TestTree
test_tokensSimple =
  testCase "produces tokens for a simple example" $
    expectTokens
      specification
      [ tokenAt 1 1 3 SemanticTokenTypes_Variable,
        tokenAt 1 7 8 SemanticTokenTypes_Number
      ]
