module Vehicle.Test.Golden.TestSkip where

import Data.Tagged (Tagged (..))
import Test.Tasty.Options (OptionDescription, OptionSet)
import Test.Tasty.Providers (IsTest (..), Result, testPassed)
import Test.Tasty.Runners (Progress)

data TestSkip = TestSkip

instance IsTest TestSkip where
  run :: OptionSet -> TestSkip -> (Progress -> IO ()) -> IO Result
  run _optionSet TestSkip _progress = return $ testPassed "SKIPPED"

  testOptions :: Tagged TestSkip [OptionDescription]
  testOptions = Tagged []
