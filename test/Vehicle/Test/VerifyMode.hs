
module Vehicle.Test.VerifyMode where

import Test.Tasty (TestTree, testGroup)

import Vehicle.Test.Utils (MonadTest)
import Vehicle.Test.VerifyMode.Golden as Golden

integrationTests :: MonadTest m => m TestTree
integrationTests = testGroup "Verify" <$> sequence
  [ Golden.goldenTests
  ]
