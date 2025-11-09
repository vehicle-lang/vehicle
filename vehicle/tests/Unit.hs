import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Test.Tasty
  ( defaultIngredients,
    defaultMainWithIngredients,
    testGroup,
  )
import Vehicle.Test.Unit.Common (vehicleLoggingIngredient)
import Vehicle.Test.Unit.Compile.CommandLine (commandLineParserTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMainWithIngredients
    (vehicleLoggingIngredient : defaultIngredients)
    $ testGroup
      "Tests"
      [ commandLineParserTests
      ]
