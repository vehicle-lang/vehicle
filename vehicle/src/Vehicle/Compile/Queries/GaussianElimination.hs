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
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as V
import GHC.Generics (Generic)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable

type Row = V.Vector Coefficient

type Solution = (LinearVar, Row)

gaussianElimination ::
  MonadCompile m =>
  [Variable] ->
  [LinearExpr] ->
  Int ->
  m ([(LinearVar, GaussianVariableSolution)], [LinearExpr])
gaussianElimination varNames exprs numberOfRowsToReduce =
  logCompilerPass MinDetail currentPhase $ do
    logDebug MaxDetail $ prettyExprs varNames exprs
    let maxIterations = min (length exprs) numberOfRowsToReduce
    let iterations :: [Int] = [0 .. maxIterations - 1]
    let rows = fmap unLinearExpr exprs
    (solvedVars, reducedRows) <- foldM (reduceRow varNames) (mempty, rows) iterations

    let unusedExprs = fmap LinearExpr reducedRows
    let solvedExprs = fmap (second (GaussianVariableSolution . LinearExpr)) solvedVars

    unless (null unusedExprs) $
      logDebug MidDetail $
        line <> pretty ("Unused:" :: String) <> line <> prettyExprs varNames unusedExprs

    return (solvedExprs, unusedExprs)

prettyExprs :: [Variable] -> [LinearExpr] -> Doc a
prettyExprs varNames exprs = prettyAssertions varNames (fmap (Assertion Equal) exprs)

prettyRows :: [Variable] -> [Row] -> Doc a
prettyRows varNames rows = prettyExprs varNames (fmap LinearExpr rows)

prettySolutions :: [Variable] -> [Solution] -> Doc a
prettySolutions varNames solutions = prettyRows varNames (fmap snd solutions)

-- | Tries to reduce the provided row in the matrix.
-- If unable to reduce it, then it returns the matrix unchanged.
reduceRow ::
  MonadCompile m =>
  [Variable] ->
  ([Solution], [Row]) ->
  LinearVar ->
  m ([Solution], [Row])
reduceRow varNames (solvedVars, rows) var = do
  let result = fromMaybe (solvedVars, rows) $ do
        (row, remainingRows) <- removeFirstNonZeroRow var rows
        let coefficient = row V.! var
        let normRow = V.map (/ coefficient) row
        -- Eliminate the row from the remaining rows
        let newRows = eliminateVarFromRow var normRow <$> remainingRows
        -- Add the newly solved row
        let normSolvedVars = second (eliminateVarFromRow var normRow) <$> solvedVars
        let newSolvedVars = (var, normRow) : normSolvedVars
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
              <> prettyRows varNames (snd result)
          )
  return result

eliminateVarFromRow :: LinearVar -> Row -> Row -> Row
eliminateVarFromRow var normRow row =
  let coefficient = row V.! var
   in V.zipWith (\a b -> a - coefficient * b) row normRow

removeFirstNonZeroRow :: LinearVar -> [Row] -> Maybe (Row, [Row])
removeFirstNonZeroRow _ [] = Nothing
removeFirstNonZeroRow var (x : xs)
  | x V.! var /= 0 = Just (x, xs)
  | otherwise = second (x :) <$> removeFirstNonZeroRow var xs

-- | A FM solution for a variable is two lists of constraints. The variable value
-- must be greater than the set of assertions, and less than the first is that
newtype GaussianVariableSolution = GaussianVariableSolution LinearExpr
  deriving (Generic)

instance ToJSON GaussianVariableSolution

instance FromJSON GaussianVariableSolution

solutionEquality :: GaussianVariableSolution -> LinearExpr
solutionEquality (GaussianVariableSolution eq) = eq

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
