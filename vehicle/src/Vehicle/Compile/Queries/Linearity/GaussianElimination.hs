
module Vehicle.Compile.Queries.Linearity.GaussianElimination
  ( gaussianElimination
  ) where

import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as V

import Control.Monad (foldM, unless)
import Data.Bifunctor
import Vehicle.Compile.Error
import Vehicle.Compile.Queries.Linearity.Core
import Vehicle.Compile.Prelude

type Row = V.Vector Coefficient
type Solution = (LinearVar, Row)

gaussianElimination :: MonadCompile m
                    => VariableNames
                    -> [LinearExpr]
                    -> Int
                    -> m ([(LinearVar, LinearExpr)], [LinearExpr])
gaussianElimination varNames exprs numberOfRowsToReduce =
  logCompilerPass MinDetail currentPhase $ do
    logDebug MaxDetail $ prettyExprs varNames exprs
    let maxIterations = min (length exprs) numberOfRowsToReduce
    let iterations :: [Int] = [0 .. maxIterations - 1]
    let rows = fmap unLinearExpr exprs
    (solvedVars, reducedRows) <- foldM (reduceRow varNames) (mempty, rows) iterations

    let unusedExprs = fmap LinearExpr reducedRows
    let solvedExprs = fmap (second LinearExpr) solvedVars

    unless (null unusedExprs) $
      logDebug MidDetail $
        line <> pretty ("Unused:" :: String) <> line <> prettyExprs varNames unusedExprs

    return (solvedExprs, unusedExprs)

prettyExprs :: VariableNames -> [LinearExpr] -> Doc a
prettyExprs varNames exprs = prettyAssertions varNames (fmap (Assertion Equal) exprs)

prettyRows :: VariableNames -> [Row] -> Doc a
prettyRows varNames rows = prettyExprs varNames (fmap LinearExpr rows)

prettySolutions :: VariableNames -> [Solution] -> Doc a
prettySolutions varNames solutions = prettyRows varNames (fmap snd solutions)

-- | Tries to reduce the provided row in the matrix.
-- If unable to reduce it, then it returns the matrix unchanged.
reduceRow :: MonadCompile m
          => VariableNames
          -> ([Solution], [Row])
          -> LinearVar
          -> m ([Solution], [Row])
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
    line <> "After iteration" <+> pretty (var + 1) <> ":" <> line <>
    indent 2 (
      "Solutions:" <>
      prettySolutions varNames (fst result) <> line <>
      "Equations:" <>
      prettyRows varNames (snd result)
    )
  return result

eliminateVarFromRow :: LinearVar -> Row -> Row -> Row
eliminateVarFromRow var normRow row =
  let coefficient = row V.! var in
  V.zipWith (\a b -> a - coefficient*b) row normRow

removeFirstNonZeroRow :: LinearVar -> [Row] -> Maybe (Row, [Row])
removeFirstNonZeroRow _   [] = Nothing
removeFirstNonZeroRow var (x : xs)
  | x V.! var /= 0 = Just (x, xs)
  | otherwise      = second (x :) <$> removeFirstNonZeroRow var xs

currentPhase :: Doc ()
currentPhase = "Gaussian elimination of user variables"
