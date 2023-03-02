module Vehicle.Compile.Queries.GaussianElimination
  ( GaussianVariableSolution,
    gaussianElimination,
    reconstructGaussianVariableValue,
    solutionEquality,
  )
where

import Control.Monad (foldM, unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable

gaussianElimination ::
  (MonadCompile m, LinearExpression linexp) =>
  [Variable] ->
  [linexp] ->
  Int ->
  m ([(LinearVar, GaussianVariableSolution)], [linexp])
gaussianElimination varNames exprs numberOfRowsToReduce =
  logCompilerPass MinDetail currentPhase $ do
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

type Solution row = (LinearVar, row)

--------------------------------------------------------------------------------
-- Algorithm

-- | Tries to reduce the provided row in the matrix.
-- If unable to reduce it, then it returns the matrix unchanged.
reduceRow ::
  (MonadCompile m, LinearExpression row) =>
  [Variable] ->
  ([Solution row], [row]) ->
  LinearVar ->
  m ([Solution row], [row])
reduceRow varNames (solvedVars, rows) var = do
  let result = fromMaybe (solvedVars, rows) $ do
        (row, remainingRows) <- removeFirstNonZeroRow var rows
        -- Eliminate the row from the remaining rows
        let newRows = eliminateVarFromRow var row <$> remainingRows
        -- Add the newly solved row (TODO remove this?)
        let normSolvedVars = second (eliminateVarFromRow var row) <$> solvedVars
        let newSolvedVars = (var, row) : normSolvedVars
        return (newSolvedVars, newRows)

  logDebug MaxDetail $
    line <> "After iteration"
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

eliminateVarFromRow :: LinearExpression row => LinearVar -> row -> row -> row
eliminateVarFromRow var normRow row = do
  let coefficient = lookupAt row var / lookupAt normRow var
  addExprs 1 row (-coefficient) normRow

removeFirstNonZeroRow :: LinearExpression row => LinearVar -> [row] -> Maybe (row, [row])
removeFirstNonZeroRow _ [] = Nothing
removeFirstNonZeroRow var (x : xs)
  | lookupAt x var /= 0 = Just (x, xs)
  | otherwise = second (x :) <$> removeFirstNonZeroRow var xs

--------------------------------------------------------------------------------
-- Solutions

-- | A solution for a variable is an equation where the coefficient for that
-- variable is 1.
newtype GaussianVariableSolution = GaussianVariableSolution
  { solutionEquality :: SparseLinearExpr
  }
  deriving (Generic)

instance ToJSON GaussianVariableSolution

instance FromJSON GaussianVariableSolution

-- | Tries to reconstruct the value of the variable that is
-- consistent with the current assignment of variables.
reconstructGaussianVariableValue ::
  VariableAssignment ->
  GaussianVariableSolution ->
  Maybe Double
reconstructGaussianVariableValue assignment solution =
  Just $ evaluateExpr (solutionEquality solution) assignment

currentPhase :: Doc ()
currentPhase = "Gaussian elimination of user variables"

--------------------------------------------------------------------------------
-- Utilities

prettyExprs :: LinearExpression row => [Variable] -> [row] -> Doc a
prettyExprs varNames exprs = prettyAssertions varNames (fmap (Assertion Equal) exprs)

prettySolutions :: LinearExpression row => [Variable] -> [Solution row] -> Doc a
prettySolutions varNames solutions = prettyExprs varNames (fmap snd solutions)
