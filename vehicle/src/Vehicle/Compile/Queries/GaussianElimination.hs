module Vehicle.Compile.Queries.GaussianElimination
  ( GaussianVariableSolution,
    gaussianElimination,
    reconstructGaussianVariableValue,
    solutionEquality,
  )
where

import Control.Monad (foldM, unless)
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Verify.Core

-- | Performs Gaussian elimination. Returns a list of solved variables, the remaining
-- unused assertions and the indices of the solved assertions.
gaussianElimination ::
  (MonadCompile m) =>
  BoundCtx MixedVariable ->
  [GaussianAssertion] ->
  m ([(MixedVariable, GaussianAssertion)], [GaussianAssertion], IntSet)
gaussianElimination variablesToEliminate exprs =
  logCompilerPass MidDetail currentPhase $ do
    logDebug MaxDetail $ prettyExprs exprs
    let numberedExprs = zip [0 ..] exprs
    (solvedVars, reducedRows, usedRows) <- foldM reduceRow (mempty, numberedExprs, mempty) (reverse variablesToEliminate)

    let unusedExprs = coerce (fmap snd reducedRows)

    unless (null unusedExprs) $
      logDebug MidDetail $
        line <> pretty ("Unused:" :: String) <> line <> indent 2 (prettyExprs unusedExprs)

    return (solvedVars, unusedExprs, usedRows)

--------------------------------------------------------------------------------
-- Interface

type GaussianAssertion = SparseLinearExpr MixedVariable

type Solution = (MixedVariable, SparseLinearExpr MixedVariable)

type RowID = Int

--------------------------------------------------------------------------------
-- Algorithm

-- | Tries to reduce the provided row in the matrix.
-- If unable to reduce it, then it returns the matrix unchanged.
reduceRow ::
  (MonadCompile m) =>
  ([Solution], [(RowID, GaussianAssertion)], IntSet) ->
  MixedVariable ->
  m ([Solution], [(RowID, GaussianAssertion)], IntSet)
reduceRow (solvedVars, rows, usedRows) var = do
  let result@(newSolvedVars', newRows', _) = fromMaybe (solvedVars, rows, usedRows) $ do
        ((pivotRowID, pivotRow), remainingRows) <- findPivot var rows
        -- Eliminate the row from the remaining rows
        let newRows = fmap (second (eliminateVar var pivotRow)) remainingRows
        let newSolvedVars = (var, pivotRow) : solvedVars
        let newUsedRows = IntSet.insert pivotRowID usedRows
        return (newSolvedVars, newRows, newUsedRows)

  logDebug MaxDetail $
    line
      <> "After solving for"
        <+> quotePretty var
      <> ":"
      <> line
      <> indent
        2
        ( "Solutions:"
            <+> pretty newSolvedVars'
            <> line
            <> "Equations:"
              <+> prettyExprs (fmap snd newRows')
        )
  return result

-- | Given a variable, tries to find a row with a suitable pivot element to
-- eliminate it.
findPivot ::
  MixedVariable ->
  [(RowID, GaussianAssertion)] ->
  Maybe ((RowID, GaussianAssertion), [(RowID, GaussianAssertion)])
findPivot _ [] = Nothing
findPivot var ((rowID, row) : rows)
  | lookupCoefficient row var /= 0 = Just ((rowID, row), rows)
  | otherwise = second ((rowID, row) :) <$> findPivot var rows

--------------------------------------------------------------------------------
-- Solutions

-- | Tries to reconstruct the value of the variable that is
-- consistent with the current assignment of variables. Returns either a
-- variable that is missing in the assignment of the reconstructed value.
reconstructGaussianVariableValue ::
  VariableAssignment MixedVariable ->
  GaussianVariableSolution ->
  Either MixedVariable VariableValue
reconstructGaussianVariableValue assignment solution =
  evaluateExpr (solutionEquality solution) assignment

--------------------------------------------------------------------------------
-- Utilities

currentPhase :: Doc ()
currentPhase = "Gaussian elimination of user variables"

prettyExprs :: [GaussianAssertion] -> Doc a
prettyExprs exprs = prettyAssertions (fmap (Assertion Equal) exprs)
