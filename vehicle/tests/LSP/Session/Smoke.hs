module Session.Smoke where

import Session.Common (runSessionWithDiagnostics)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test_sessionSmoke :: TestTree
test_sessionSmoke =
  testCase "starts and initialises a session" $
    runSessionWithDiagnostics "vehicle lsp" "." (pure ())
