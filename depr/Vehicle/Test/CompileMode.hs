module Vehicle.Test.CompileMode
  ( functionalityTests
  , performanceTests
  ) where

import Test.Tasty

import Vehicle.Test.CompileMode.Error qualified as Error (functionalityTests)
import Vehicle.Test.CompileMode.Golden qualified as Golden
import Vehicle.Test.CompileMode.Unit qualified as Unit (functionalityTests)
import Vehicle.Test.Utils

functionalityTests :: MonadTest m => m TestTree
functionalityTests = testGroup "Compile" <$> sequence
  [ Golden.functionalityTests
  , Unit.functionalityTests
  , Error.functionalityTests
  ]

performanceTests :: TestTree
performanceTests = testGroup "Compile"
  [ Golden.performanceTests
  ]
