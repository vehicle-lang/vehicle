module Vehicle.Test.Unit.Compile.Normalisation
  ( normalisationTests,
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile.Normalise.NBE (whnf)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Expr.AlphaEquivalence ()
import Vehicle.Expr.DeBruijn (DBLevel)
import Vehicle.Test.Unit.Common (unitTestCase)

normalisationTests :: TestTree
normalisationTests =
  testGroup "Normalisation" $
    fmap
      normalisationTest
      [ NBETest
          { name = "Lambda",
            dbLevel = 0,
            input = Lam p (binding (NatType p)) (BoundVar p 0),
            expected = Lam p (binding (NatType p)) (BoundVar p 0)
          },
        NBETest
          { name = "AppLambdaClosedBody",
            dbLevel = 0,
            input = App p (Lam p (binding (NatType p)) (BoundVar p 0)) [ExplicitArg p $ NatLiteral p 1],
            expected = NatLiteral p 1
          },
        NBETest
          { name = "AppLambdaOpenBody",
            dbLevel = 1,
            input = App p (Lam p (binding (NatType p)) (BoundVar p 1)) [ExplicitArg p $ NatLiteral p 1],
            expected = BoundVar p 0
          },
        NBETest
          { name = "AppPlus",
            dbLevel = 1,
            input = App p (Builtin p $ Add AddNat) [ExplicitArg p (NatLiteral p 2), ExplicitArg p (NatLiteral p 1)],
            expected = NatLiteral p 3
          },
        NBETest
          { name = "ListMeta",
            dbLevel = 1,
            input = App p (Builtin p $ Constructor List) [ExplicitArg p (Meta p (MetaID 0))],
            expected = App p (Builtin p $ Constructor List) [ExplicitArg p (Meta p (MetaID 0))]
          }
      ]

--------------------------------------------------------------------------------
-- De Bruijn tests

data NBETest = NBETest
  { name :: String,
    dbLevel :: DBLevel,
    input :: CheckedExpr,
    expected :: CheckedExpr
  }

normalisationTest :: NBETest -> TestTree
normalisationTest NBETest {..} =
  unitTestCase ("normalise" <> name) $ do
    normInput <- whnf dbLevel mempty mempty input
    actual <- quote dbLevel normInput

    let errorMessage =
          layoutAsString $
            "Expected the result of normalising:"
              <> line
              <> indent 2 (prettyVerbose input)
              <> line
              <> "to be equal to:"
              <+> line
                <> indent 2 (prettyVerbose expected)
                <> line
                <> "but was:"
              <+> line
                <> indent 2 (prettyVerbose actual)

    return $ assertBool errorMessage (actual == expected)

p :: Provenance
p = mempty

binding :: CheckedType -> CheckedBinder
binding = Binder p (BinderForm (OnlyName "x") False) Explicit Relevant ()
