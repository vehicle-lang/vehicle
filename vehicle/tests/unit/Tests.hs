import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Test.Tasty
  ( TestTree,
    defaultIngredients,
    defaultMainWithIngredients,
    includingOptions,
    testGroup,
  )
import Vehicle.Test.Unit.Common (vehicleLoggingIngredient)
import Vehicle.Test.Unit.Compile.AlphaEquivalence (alphaEquivalenceTests)
import Vehicle.Test.Unit.Compile.CoDeBruijn (coDeBruijnTests)
import Vehicle.Test.Unit.Compile.CommandLine (commandLineParserTests)
import Vehicle.Test.Unit.Compile.DeBruijn (deBruijnTests)
import Vehicle.Test.Unit.Compile.IfElimination (ifEliminationTests)
import Vehicle.Test.Unit.Compile.LetInsertion (letInsertionTests)
import Vehicle.Test.Unit.Compile.Normalisation (normalisationTests)
import Vehicle.Test.Unit.Compile.PositionTree (positionTreeTests)
import Vehicle.Test.Unit.Compile.QuantifierLifting (quantiferLiftingTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMainWithIngredients
    (vehicleLoggingIngredient : defaultIngredients)
    $ testGroup
      "Tests"
      [ deBruijnTests,
        normalisationTests,
        ifEliminationTests,
        quantiferLiftingTests,
        alphaEquivalenceTests,
        coDeBruijnTests,
        positionTreeTests,
        letInsertionTests,
        commandLineParserTests
      ]
