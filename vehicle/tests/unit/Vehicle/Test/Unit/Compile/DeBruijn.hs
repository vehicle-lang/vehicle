module Vehicle.Test.Unit.Compile.DeBruijn (deBruijnTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile.Prelude
  ( BinderDisplayForm (BinderDisplayForm),
    BinderNamingForm (OnlyName),
    Expr (Lam),
    GenericBinder (Binder),
    Pretty (pretty),
    Provenance,
    Relevance (Relevant),
    Visibility (Explicit),
    indent,
    layoutAsString,
    line,
    (<+>),
    pattern BoundVar,
  )
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard (StandardExpr, TypeCheckedBinder, TypeCheckedExpr, TypeCheckedType)
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.DeBruijn (Lv, liftDBIndices, substDBInto)
import Vehicle.Test.Unit.Common (unitTestCase)

--------------------------------------------------------------------------------
-- De Bruijn tests

deBruijnTests :: TestTree
deBruijnTests =
  testGroup "DeBruijnIndices" $
    substitutionTests
      <> liftingTests

substitutionTests :: [TestTree]
substitutionTests =
  fmap
    substTest
    [ SubstitutionTest
        { name = "UnderLambdaClosed",
          value = NatLiteral p 2,
          expr = Lam p (binding (NatType p)) (BoundVar p 0),
          expected = Lam p (binding (NatType p)) (BoundVar p 0)
        },
      SubstitutionTest
        { name = "UnderLambdaOpenBody",
          value = NatLiteral p 2,
          expr = Lam p (binding (NatType p)) (BoundVar p 1),
          expected = Lam p (binding (NatType p)) (NatLiteral p 2)
        },
      SubstitutionTest
        { name = "UnderLambdaOpenType",
          value = NatLiteral p 2,
          expr = Lam p (binding (BoundVar p 0)) (BoundVar p 0),
          expected = Lam p (binding (NatLiteral p 2)) (BoundVar p 0)
        }
    ]

liftingTests :: [TestTree]
liftingTests =
  fmap
    liftTest
    [ LiftingTest
        { name = "UnderLambdaClosed",
          amount = 1,
          input = Lam p (binding (NatType p)) (BoundVar p 0),
          expected = Lam p (binding (NatType p)) (BoundVar p 0)
        },
      LiftingTest
        { name = "UnderLambdaOpenBody",
          amount = 1,
          input = Lam p (binding (NatType p)) (BoundVar p 1),
          expected = Lam p (binding (NatType p)) (BoundVar p 2)
        },
      LiftingTest
        { name = "UnderLambdaOpenType",
          amount = 1,
          input = Lam p (binding (BoundVar p 0)) (BoundVar p 0),
          expected = Lam p (binding (BoundVar p 1)) (BoundVar p 0)
        }
    ]

--------------------------------------------------------------------------------
-- MySubst

data SubstitutionTest = SubstitutionTest
  { name :: String,
    value :: StandardExpr,
    expr :: StandardExpr,
    expected :: StandardExpr
  }

substTest :: SubstitutionTest -> TestTree
substTest SubstitutionTest {..} =
  unitTestCase ("subst" <> name) $ do
    let actual = value `substDBInto` expr

    let errorMessage =
          layoutAsString $
            "Expected performing the subsitution:"
              <> line
              <> indent 2 (prettyVerbose value)
              <> line
              <> "into"
              <> line
              <> indent 2 (prettyVerbose expr)
              <> line
              <> "to be equal to:"
                <+> line
              <> indent 2 (prettyVerbose expected)
              <> line
              <> "but was:"
                <+> line
              <> indent 2 (prettyVerbose actual)

    return $ assertBool errorMessage (actual == expected)

--------------------------------------------------------------------------------
-- Lifting

data LiftingTest = LiftingTest
  { name :: String,
    amount :: Lv,
    input :: TypeCheckedExpr,
    expected :: TypeCheckedExpr
  }

liftTest :: LiftingTest -> TestTree
liftTest LiftingTest {..} =
  unitTestCase ("lift" <> name) $ do
    let actual = liftDBIndices amount input

    let errorMessage =
          layoutAsString $
            "Expected the result of lifting the free indices in:"
              <> line
              <> indent 2 (prettyVerbose input)
              <> line
              <> "by"
                <+> pretty amount
                <+> "to be equal to:"
                <+> line
              <> indent 2 (prettyVerbose expected)
              <> line
              <> "but was:"
                <+> line
              <> indent 2 (prettyVerbose actual)

    return $ assertBool errorMessage (actual == expected)

p :: Provenance
p = mempty

binding :: TypeCheckedType -> TypeCheckedBinder
binding = Binder p (BinderDisplayForm (OnlyName "x") False) Explicit Relevant
