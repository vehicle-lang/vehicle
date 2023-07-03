import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Test.Tasty
  ( defaultIngredients,
    defaultMainWithIngredients,
    testGroup,
  )
import Vehicle.Test.Unit.Common (vehicleLoggingIngredient)
import Vehicle.Test.Unit.Compile.CommandLine (commandLineParserTests)
import Vehicle.Test.Unit.Compile.DeBruijn (deBruijnTests)
import Vehicle.Test.Unit.Compile.Normalisation (normalisationTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMainWithIngredients
    (vehicleLoggingIngredient : defaultIngredients)
    $ testGroup
      "Tests"
      [ deBruijnTests,
        normalisationTests,
        commandLineParserTests
      ]
