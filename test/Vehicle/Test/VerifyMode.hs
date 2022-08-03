
module Vehicle.Test.VerifyMode where

import Test.Tasty

import Vehicle.Test.VerifyMode.Golden as Golden

functionalityTests :: TestTree
functionalityTests = testGroup "Verify"
  [ Golden.goldenTests
  ]
