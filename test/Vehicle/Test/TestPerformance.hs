import Test.Tasty.Bench

import Control.Monad.Reader

import Vehicle.Test.CompileMode qualified as CompileMode
import Vehicle.Test.Utils

main :: IO ()
main = defaultMain performanceTests

performanceTests :: [Benchmark]
performanceTests =
  [ CompileMode.performanceTests
  ]