module Vehicle.Test.Unit.Compile.AlphaEquivalence (alphaEquivalenceTests) where

import Control.Exception ()
import Data.Hashable (Hashable (hash))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile (parseAndTypeCheckExpr)
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Expr.AlphaEquivalence (AlphaEquivalence (alphaEq))
import Vehicle.Expr.CoDeBruijn.Conversion (toCoDBExpr)
import Vehicle.Prelude
  ( Pretty (pretty),
    indent,
    layoutAsString,
    line,
    squotes,
    (<+>),
  )
import Vehicle.Test.Unit.Common (unitTestCase)

--------------------------------------------------------------------------------
-- Alpha equivalence tests

data AlphaTestSpec = AlphaTestSpec String Bool Text Text

alphaEquivalenceTests :: TestTree
alphaEquivalenceTests =
  testGroup "AlphaEquivalence" . fmap equalUpToAlpha $
    [ AlphaTestSpec "natEq" True "1 : Nat" "1 : Nat",
      AlphaTestSpec "natNeq" False "1 : Nat" "2 : Nat",
      AlphaTestSpec "lambdaEq" True "\\(x : Nat) -> x" "\\(y : Nat) -> y",
      AlphaTestSpec "lambda2Neq" False "\\(x : Nat) -> \\(y : Nat) -> x" "\\(x : Nat) -> \\(y : Nat) -> y",
      AlphaTestSpec "appEq" True "(\\(x : Nat) -> x) 1" "(\\(y : Nat) -> y) 1"
    ]

equalUpToAlpha :: AlphaTestSpec -> TestTree
equalUpToAlpha (AlphaTestSpec testName shouldBeEqual t1 t2) =
  unitTestCase testName $ do
    e1 <- parseAndTypeCheckExpr t1
    e2 <- parseAndTypeCheckExpr t2

    let errorMessage =
          layoutAsString $
            "Expected:"
              <+> line
                <> indent
                  2
                  ( squotes (pretty t1)
                      <> line
                      <> squotes (pretty t2)
                  )
                <> line
                <> "to"
              <+> (if shouldBeEqual then "" else "not ")
                <> "be equal up to alpha equivalence"
                <> "\n\n"
                <> "CoDB parses:"
              <+> line
                <> indent
                  2
                  ( squotes (prettyVerbose (toCoDBExpr e1))
                      <> line
                      <> squotes (prettyVerbose (toCoDBExpr e2))
                  )
                <> "\n\n"
                <> "Hashes:"
                <> line
                <> indent
                  2
                  ( squotes (pretty $ hash e1)
                      <> line
                      <> squotes (pretty $ hash e2)
                  )

    let result = alphaEq e1 e2
    return $ assertBool errorMessage (result == shouldBeEqual)
