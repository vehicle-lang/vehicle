module Vehicle.Test.CompileMode.Unit
  ( functionalityTests
  ) where

import Test.Tasty (TestTree, testGroup)

import Vehicle.Test.CompileMode.Unit.AlphaEquivalence (alphaEquivalenceTests)
import Vehicle.Test.CompileMode.Unit.CoDeBruijn (coDeBruijnTests)
import Vehicle.Test.CompileMode.Unit.CommandLine (commandLineParserTests)
import Vehicle.Test.CompileMode.Unit.IfElimination (ifEliminationTests)
import Vehicle.Test.CompileMode.Unit.LetInsertion (letInsertionTests)
import Vehicle.Test.CompileMode.Unit.PositionTree (positionTreeTests)
import Vehicle.Test.CompileMode.Unit.QuantifierLifting (quantiferLiftingTests)
import Vehicle.Test.Utils (MonadTest)

functionalityTests :: MonadTest m => m TestTree
functionalityTests = testGroup "UnitTests" <$> sequence
  [ alphaEquivalenceTests
  , coDeBruijnTests
  , positionTreeTests
  , letInsertionTests
  , ifEliminationTests
  , quantiferLiftingTests
  , pure commandLineParserTests
  ]
