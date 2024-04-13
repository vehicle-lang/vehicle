module Vehicle.Test.Unit.Compile.Normalisation
  ( normalisationTests,
  )
where

import Data.Data (Proxy (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BuiltinInterface.Expr
import Vehicle.Data.NormalisedExpr
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
            input = App (Lam p (binding (NatType p)) (BoundVar p 0)) [Arg p Explicit Relevant $ NatLiteral p 1],
            expected = NatLiteral p 1
          },
        NBETest
          { name = "AppLambdaOpenBody",
            dbLevel = 1,
            input = App (Lam p (binding (NatType p)) (BoundVar p 1)) [Arg p Explicit Relevant $ NatLiteral p 1],
            expected = BoundVar p 0
          },
        NBETest
          { name = "AppPlus",
            dbLevel = 1,
            input = App (Builtin p $ BuiltinFunction $ Add AddNat) [Arg p Explicit Relevant (NatLiteral p 2), Arg p Explicit Relevant (NatLiteral p 1)],
            expected = NatLiteral p 3
          },
        NBETest
          { name = "ListMeta",
            dbLevel = 1,
            input = App (Builtin p $ BuiltinType List) [Arg p Explicit Relevant (Meta p (MetaID 0))],
            expected = App (Builtin p $ BuiltinType List) [Arg p Explicit Relevant (Meta p (MetaID 0))]
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
    normInput <-
      runFreshFreeContextT (Proxy @Builtin) $
        normaliseInEnv (mkNoOpEnv dbLevel) input
    let actual = quote mempty dbLevel normInput

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

mkNoOpEnv :: Lv -> WHNFBoundEnv builtin
mkNoOpEnv boundCtxSize = [mkDefaultEnvEntry "_" Bound | _ <- [0 .. boundCtxSize - 1]]
