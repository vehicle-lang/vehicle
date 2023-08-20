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
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.BuiltinInterface
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
            input = App p (Lam p (binding (NatType p)) (BoundVar p 0)) [RelevantExplicitArg p $ NatLiteral p 1],
            expected = NatLiteral p 1
          },
        NBETest
          { name = "AppLambdaOpenBody",
            dbLevel = 1,
            input = App p (Lam p (binding (NatType p)) (BoundVar p 1)) [RelevantExplicitArg p $ NatLiteral p 1],
            expected = BoundVar p 0
          },
        NBETest
          { name = "AppPlus",
            dbLevel = 1,
            input = App p (Builtin p $ BuiltinFunction $ Add AddNat) [RelevantExplicitArg p (NatLiteral p 2), RelevantExplicitArg p (NatLiteral p 1)],
            expected = NatLiteral p 3
          },
        NBETest
          { name = "ListMeta",
            dbLevel = 1,
            input = App p (Builtin p $ BuiltinType List) [RelevantExplicitArg p (Meta p (MetaID 0))],
            expected = App p (Builtin p $ BuiltinType List) [RelevantExplicitArg p (Meta p (MetaID 0))]
          }
      ]

--------------------------------------------------------------------------------
-- De Bruijn tests

data NBETest = NBETest
  { name :: String,
    dbLevel :: Lv,
    input :: Expr Ix Builtin,
    expected :: Expr Ix Builtin
  }

normalisationTest :: NBETest -> TestTree
normalisationTest NBETest {..} =
  unitTestCase ("normalise" <> name) $ do
    normInput <- runEmptyNormT @Builtin $ whnf (mkNoOpEnv dbLevel) input
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

binding :: Type Ix Builtin -> Binder Ix Builtin
binding = Binder p (BinderDisplayForm (OnlyName "x") False) Explicit Relevant

mkNoOpEnv :: Lv -> Env builtin
mkNoOpEnv boundCtxSize = [(Nothing, VBoundVar i []) | i <- reverse [0 .. boundCtxSize - 1]]
