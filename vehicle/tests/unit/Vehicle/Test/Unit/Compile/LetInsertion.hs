module Vehicle.Test.Unit.Compile.LetInsertion where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile (parseAndTypeCheckExpr, typeCheckExpr)
import Vehicle.Expr.AlphaEquivalence (AlphaEquivalence (alphaEq))
import Vehicle.Compile.LetInsertion (insertLets)
import Vehicle.Compile.Prelude (CheckedCoDBExpr, Expr (App), Pretty (pretty),
                                indent, layoutAsString, line, squotes)
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Test.Unit.Common (normTypeClasses, unitTestCase)

--------------------------------------------------------------------------------
-- Let lifting tests

letInsertionTests :: TestTree
letInsertionTests =
  testGroup "LetInsertion" . fmap letInsertionTest $
    [ InsertionTestSpec "insertFun"
        standardFilter
        "(Nat -> Nat) -> (Nat -> Nat)"
        "let y = (Nat -> Nat) in y -> y"

    , InsertionTestSpec "insertNeg"
        standardFilter
        "\\(x : Int) -> (- x) + (- x)"
        "\\(x : Int) -> (let y = (- x) in (y + y))"

    -- Disabled due to bugs in type-checker
    -- , testCase "insertLam" $ letInsertionTest
    --     standardFilter
    --     "(\\(x : Nat) -> x) ((\\(y : Nat) -> y) 1)"
    --     "let id = (\\(z : Nat) -> z) in id (id 1)"

    -- Disabled due to bugs in type-checker
    -- , testCase "insertAdd" $ letInsertionTest
    --     standardFilter
    --     "\\(x : Int) (y : Int) -> (((- x) + (- y)) / ((- x) + (- y))) + (- y)"
    --     "\\(x : Int) -> (let b = (- x) in (\\(y : Int) -> (let a = (- y) in (let c = (a + b) in (c / c))) + y))"

    , InsertionTestSpec "insertLiftApp"
        appFilter
        "- - (1 : Int)"
        "let a = 1 in let b = (- (a : Int)) in let c = - b in c"
    ]

data InsertionTestSpec = InsertionTestSpec String SubexprFilter Text Text

type SubexprFilter = CheckedCoDBExpr -> Int -> Bool

standardFilter :: SubexprFilter
standardFilter e q = q > 1

appFilter :: SubexprFilter
appFilter (App{}, _)         _ = True
appFilter _                  _ = False

letInsertionTest :: InsertionTestSpec -> TestTree
letInsertionTest (InsertionTestSpec testName filter input expected) =
  unitTestCase testName $ do
    inputExpr    <- normTypeClasses =<< parseAndTypeCheckExpr input
    expectedExpr <- normTypeClasses =<< parseAndTypeCheckExpr expected
    result       <- insertLets filter True inputExpr

    -- Need to re-typecheck the result as let-insertion puts a Hole on
    -- each binder type.
    typedResult <- typeCheckExpr result

    let errorMessage = layoutAsString $
          "Expected the result of let lifting" <> line <>
            indent 2 (squotes (pretty input)) <> line <>
          "to be alpha equivalent to" <> line <>
            indent 2 (squotes (prettyFriendly expectedExpr)) <> line <>
          "however the result was" <> line <>
            indent 2 (squotes (prettyFriendly typedResult))

    return $ assertBool errorMessage $
      alphaEq expectedExpr typedResult
