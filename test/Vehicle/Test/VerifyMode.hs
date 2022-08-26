
module Vehicle.Test.VerifyMode where

import Test.Tasty ( testGroup, TestTree )

import Vehicle.Test.VerifyMode.Golden as Golden
import Vehicle.Test.Utils (MonadTest)

integrationTests :: MonadTest m => m TestTree
integrationTests = testGroup "Verify" <$> sequence
  [ Golden.goldenTests
  ]
