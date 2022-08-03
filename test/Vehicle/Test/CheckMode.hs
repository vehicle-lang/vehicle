
module Vehicle.Test.CheckMode
  ( functionalityTests
  ) where

import Test.Tasty

import Vehicle.Test.CheckMode.Golden as Golden (tests)

functionalityTests :: TestTree
functionalityTests = testGroup "Check"
  [ Golden.tests
  ]