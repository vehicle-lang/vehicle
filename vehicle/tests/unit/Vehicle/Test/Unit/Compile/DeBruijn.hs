module Vehicle.Test.Unit.Compile.DeBruijn
  ( deBruijnTests ) where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.IntMap qualified as IntMap
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile (parseAndTypeCheckExpr)
import Vehicle.Compile.AlphaEquivalence ()
import Vehicle.Compile.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Test.Unit.Common (unitTestCase)

--------------------------------------------------------------------------------
-- Alpha equivalence tests

deBruijnTests :: TestTree
deBruijnTests =
  testGroup "DeBruijnIndices" $
    substitutionTests
    <> liftingTests

substitutionTests :: [TestTree]
substitutionTests = fmap substTest
  [ SubstitutionTest
    { name     = "UnderLambdaClosed"
    , subst    = \case {0 -> Just $ NatLiteral p 2; _ -> Nothing}
    , input    = Lam p (binding (NatType p)) (BoundVar p 0)
    , expected = Lam p (binding (NatType p)) (BoundVar p 0)
    }

  , SubstitutionTest
    { name     = "UnderLambdaOpenBody"
    , subst    = \case {0 -> Just $ NatLiteral p 2; _ -> Nothing}
    , input    = Lam p (binding (NatType p)) (BoundVar p 1)
    , expected = Lam p (binding (NatType p)) (NatLiteral p 2)
    }

  , SubstitutionTest
    { name     = "UnderLambdaOpenType"
    , subst    = \case {0 -> Just $ NatLiteral p 2; _ -> Nothing}
    , input    = Lam p (binding (BoundVar p 0)) (BoundVar p 0)
    , expected = Lam p (binding (NatLiteral p 2)) (BoundVar p 0)
    }
  ]

liftingTests :: [TestTree]
liftingTests = fmap liftTest
  [ LiftingTest
    { name     = "UnderLambdaClosed"
    , amount   = 1
    , input    = Lam p (binding (NatType p)) (BoundVar p 0)
    , expected = Lam p (binding (NatType p)) (BoundVar p 0)
    }

  , LiftingTest
    { name     = "UnderLambdaOpenBody"
    , amount   = 1
    , input    = Lam p (binding (NatType p)) (BoundVar p 1)
    , expected = Lam p (binding (NatType p)) (BoundVar p 2)
    }

  , LiftingTest
    { name     = "UnderLambdaOpenType"
    , amount   = 1
    , input    = Lam p (binding (BoundVar p 0)) (BoundVar p 0)
    , expected = Lam p (binding (BoundVar p 1)) (BoundVar p 0)
    }
  ]

--------------------------------------------------------------------------------
-- MySubst

data SubstitutionTest = SubstitutionTest
  { name     :: String
  , subst    :: Substitution
  , input    :: CheckedExpr
  , expected :: CheckedExpr
  }

substTest :: SubstitutionTest -> TestTree
substTest SubstitutionTest{..} =
  unitTestCase ("subst" <> name) $ do

    let actual = substitute subst input

    let errorMessage = layoutAsString $
          "Expected performing the subsitution:" <> line <>
            indent 2 "(unviewable)" <> line <>
          "into" <> line <>
            indent 2 (prettyVerbose input) <> line <>
          "to be equal to:" <+> line <>
            indent 2 (prettyVerbose expected) <> line <>
          "but was:" <+> line <>
            indent 2 (prettyVerbose actual)

    return $ assertBool errorMessage (actual == expected)


--------------------------------------------------------------------------------
-- Lifting

data LiftingTest = LiftingTest
  { name     :: String
  , amount   :: Int
  , input    :: CheckedExpr
  , expected :: CheckedExpr
  }

liftTest :: LiftingTest -> TestTree
liftTest LiftingTest{..} =
  unitTestCase ("lift" <> name) $ do

    let actual = liftFreeDBIndices amount input

    let errorMessage = layoutAsString $
          "Expected the result of lifting the free indices in:" <> line <>
            indent 2 (prettyVerbose input) <> line <>
          "by" <+> pretty amount <+> "to be equal to:" <+> line <>
            indent 2 (prettyVerbose expected) <> line <>
          "but was:" <+> line <>
            indent 2 (prettyVerbose actual)

    return $ assertBool errorMessage (actual == expected)

p :: Provenance
p = mempty

binding :: CheckedType -> CheckedBinder
binding = ExplicitBinder p Nothing
