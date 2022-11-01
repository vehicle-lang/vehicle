module Vehicle.Test.Unit.Compile.CoDeBruijn
  ( coDeBruijnTests ) where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.IntMap qualified as IntMap
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile (parseAndTypeCheckExpr)
import Vehicle.Compile.AlphaEquivalence ()
import Vehicle.Compile.CoDeBruijnify (fromCoDB, toCoDBExpr)
import Vehicle.Compile.Prelude (indent, layoutAsString, line, squotes, (<+>))
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Test.Unit.Common (unitTestCase)

--------------------------------------------------------------------------------
-- Alpha equivalence tests

coDeBruijnTests :: TestTree
coDeBruijnTests =
  testGroup "CoDeBruijnIndices" . fmap toFromCoDB $
    [ CoDeBruijnTestSpec "type"    "Nat"
    , CoDeBruijnTestSpec "typeFun" "Nat -> Nat"
    , CoDeBruijnTestSpec "lam"     "\\(x : Nat) -> x"
    , CoDeBruijnTestSpec "lam2"    "\\(f : Nat -> Nat) (x : Nat) -> f x"
    , CoDeBruijnTestSpec "pi"      "forallT (n : Nat) . Tensor Nat [n]"
    , CoDeBruijnTestSpec "neg"     "\\(x : Int) -> - x"
    ]

data CoDeBruijnTestSpec = CoDeBruijnTestSpec String Text

toFromCoDB :: CoDeBruijnTestSpec -> TestTree
toFromCoDB (CoDeBruijnTestSpec testName e1) =
  unitTestCase testName $ do
    e2 <- parseAndTypeCheckExpr e1
    let e3 = toCoDBExpr e2
    let e4 = fromCoDB e3

    let errorMessage = layoutAsString $
          "Expected:" <+> line <> indent 2 (
            squotes (prettyVerbose e2)  <> line <>
            squotes (prettyVerbose e4)) <> line <>
          "to be equal" <>
          "\n\n" <>
          "CoDB parse:" <+> line <> indent 2 (
            squotes (prettyVerbose e3))

    return $ assertBool errorMessage (e2 == e4)