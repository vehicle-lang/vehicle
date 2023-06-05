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
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Verify.Core

gaussianElimination ::
  (MonadCompile m, LinearExpression linexp) =>
  [Variable] ->
  [linexp] ->
  Int ->
  m ([(LinearVar, GaussianVariableSolution)], [linexp])
gaussianElimination varNames exprs numberOfRowsToReduce =
  logCompilerPass MidDetail currentPhase $ do
    logDebug MaxDetail $ prettyExprs varNames exprs
    let maxIterations = min (length exprs) numberOfRowsToReduce
    let iterations :: [Int] = [0 .. maxIterations - 1]
    (solvedVars, reducedRows) <- foldM (reduceRow varNames) (mempty, exprs) iterations

    let unusedExprs = coerce reducedRows
    let solvedExprs = fmap (second (GaussianVariableSolution . toSparse)) solvedVars

    unless (null unusedExprs) $
      logDebug MidDetail $
        line <> pretty ("Unused:" :: String) <> line <> prettyExprs varNames unusedExprs

    return (solvedExprs, unusedExprs)

--------------------------------------------------------------------------------
-- Interface

type Solution linexp = (LinearVar, linexp)

--------------------------------------------------------------------------------
-- Algorithm

-- | Tries to reduce the provided row in the matrix.
-- If unable to reduce it, then it returns the matrix unchanged.
reduceRow ::
  (MonadCompile m, LinearExpression linexp) =>
  [Variable] ->
  ([Solution linexp], [linexp]) ->
  LinearVar ->
  m ([Solution linexp], [linexp])
reduceRow varNames (solvedVars, rows) var = do
  let result = fromMaybe (solvedVars, rows) $ do
        (pivotRow, remainingRows) <- findPivot var rows
        -- Eliminate the row from the remaining rows
        let newRows = eliminateVar var pivotRow <$> remainingRows
        -- Add the newly solved row (TODO remove this?)
        let normSolvedVars = second (eliminateVar var pivotRow) <$> solvedVars
        let newSolvedVars = (var, pivotRow) : normSolvedVars
        return (newSolvedVars, newRows)

  logDebug MaxDetail $
    line
      <> "After iteration"
        <+> pretty (var + 1)
      <> ":"
      <> line
      <> indent
        2
        ( "Solutions:"
            <> prettySolutions varNames (fst result)
            <> line
            <> "Equations:"
            <> prettyExprs varNames (snd result)
        )
  return result

findPivot :: (LinearExpression linexp) => LinearVar -> [linexp] -> Maybe (linexp, [linexp])
findPivot _ [] = Nothing
findPivot var (x : xs)
  | lookupAt x var /= 0 = Just (x, xs)
  | otherwise = second (x :) <$> findPivot var xs

--------------------------------------------------------------------------------
-- Solutions

-- | Tries to reconstruct the value of the variable that is
-- consistent with the current assignment of variables.
reconstructGaussianVariableValue ::
  VariableAssignment ->
  GaussianVariableSolution ->
  Double
reconstructGaussianVariableValue assignment solution =
  evaluateExpr (solutionEquality solution) assignment

currentPhase :: Doc ()
currentPhase = "Gaussian elimination of user variables"

--------------------------------------------------------------------------------
-- Utilities

prettyExprs :: (LinearExpression linexp) => [Variable] -> [linexp] -> Doc a
prettyExprs varNames exprs = prettyAssertions varNames (fmap (Assertion Equal) exprs)

prettySolutions :: (LinearExpression linexp) => [Variable] -> [Solution linexp] -> Doc a
prettySolutions varNames solutions = prettyExprs varNames (fmap snd solutions)
