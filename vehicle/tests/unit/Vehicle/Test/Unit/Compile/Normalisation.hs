module Vehicle.Test.Unit.Compile.Normalisation
  ( normalisationTests,
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile.Normalise.NBE (runEmptyNormT, whnf)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard (StandardBinder, StandardBuiltinType (..), StandardExpr, StandardType)
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.DeBruijn (Lv)
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised
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
            input = App p (Builtin p $ CFunction $ Add AddNat) [ExplicitArg p (NatLiteral p 2), ExplicitArg p (NatLiteral p 1)],
            expected = NatLiteral p 3
          },
        NBETest
          { name = "ListMeta",
            dbLevel = 1,
            input = App p (Builtin p $ CType $ StandardBuiltinType List) [ExplicitArg p (Meta p (MetaID 0))],
            expected = App p (Builtin p $ CType $ StandardBuiltinType List) [ExplicitArg p (Meta p (MetaID 0))]
          }
      ]

--------------------------------------------------------------------------------
-- De Bruijn tests

data NBETest = NBETest
  { name :: String,
    dbLevel :: Lv,
    input :: StandardExpr,
    expected :: StandardExpr
  }

normalisationTest :: NBETest -> TestTree
normalisationTest NBETest {..} =
  unitTestCase ("normalise" <> name) $ do
    normInput <- runEmptyNormT @StandardBuiltinType $ whnf (mkNoOpEnv dbLevel) input
    actual <- quote mempty dbLevel normInput

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

binding :: StandardType -> StandardBinder
binding = Binder p (BinderDisplayForm (OnlyName "x") False) Explicit Relevant

mkNoOpEnv :: Lv -> Env builtin
mkNoOpEnv boundCtxSize = [(Nothing, VBoundVar i []) | i <- reverse [0 .. boundCtxSize - 1]]
