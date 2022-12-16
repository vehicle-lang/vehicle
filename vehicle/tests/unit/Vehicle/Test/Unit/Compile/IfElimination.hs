module Vehicle.Test.Unit.Compile.IfElimination
  ( ifEliminationTests,
  )
where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Vehicle.Compile (parseAndTypeCheckExpr)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Queries.IfElimination
import Vehicle.Expr.AlphaEquivalence
import Vehicle.Test.Unit.Common (normTypeClasses, unitTestCase)

--------------------------------------------------------------------------------
-- If lifting tests

ifEliminationTests :: TestTree
ifEliminationTests =
  testGroup "LiftAndElimIf" . fmap liftAndEliminateIfsTest $
    [ IfTestSpec
        "liftIfAnn"
        "(if False then True else False) : Bool"
        "if False then (True : Bool) else (False : Bool)",
      IfTestSpec
        "liftListIf2"
        "([if True then 1 else 2, if False then 3 else 4]) ! 0 >= (0 : Nat)"
        "if True \
        \then (if False then ([1, 3] ! 0 >= (0 : Nat)) else ([1, 4] ! 0 >= (0 : Nat))) \
        \else (if False then ([2, 3] ! 0 >= (0 : Nat)) else ([2, 4] ! 0 >= (0 : Nat)))",
      IfTestSpec
        "liftIfAdd2"
        "(if True then 1 else 2) + (if False then 3 else 4) >= 0"
        "if True \
        \then (if False then (1 + 3 >= 0) else (1 + 4 >= 0)) \
        \else (if False then (2 + 3 >= 0) else (2 + 4 >= 0))",
      IfTestSpec
        "elimIfNot"
        "not (if True then False else (True : Bool))"
        "not ((True and False) or (False and (True : Bool)))",
      IfTestSpec
        "elimIfIf"
        "if (if True then False else True) then False else True"
        "if ((True and False) or (False and True)) then False else True"
    ]

--------------------------------------------------------------------------------
-- Test implementations

data IfTestSpec = IfTestSpec String Text Text

liftAndEliminateIfsTest :: IfTestSpec -> TestTree
liftAndEliminateIfsTest (IfTestSpec testName input expected) =
  unitTestCase testName $ do
    inputExpr <- normTypeClasses =<< parseAndTypeCheckExpr input
    expectedExpr <- normTypeClasses =<< parseAndTypeCheckExpr expected
    result <- eliminateIfs inputExpr

    let errorMessage =
          layoutAsString $
            "Expected the result of if elimination on"
              <> line
              <> indent 2 (squotes (pretty input))
              <> line
              <> "to be alpha equivalent to"
              <> line
              <> indent 2 (squotes (prettyVerbose expectedExpr))
              <> line
              <> "however the result was"
              <> line
              <> indent 2 (squotes (prettyVerbose result))

    return $
      assertBool errorMessage $
        alphaEq expectedExpr result
