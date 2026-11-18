module Session.Smoke where

import Language.LSP.Protocol.Capabilities (fullLatestClientCaps)
import Language.LSP.Test (runSession)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test_sessionSmoke :: TestTree
test_sessionSmoke =
  testCase "starts and initialises a session" $
    runSession "vehicle lsp" fullLatestClientCaps "." $
      pure ()
