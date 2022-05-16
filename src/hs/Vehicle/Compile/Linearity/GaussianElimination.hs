
module Vehicle.Compile.Linearity.GaussianElimination
  ( gaussianElimination
  ) where

import Data.Vector.Unboxed qualified as V
import Data.Maybe (fromMaybe)
import Data.List (findIndex)

import Vehicle.Compile.Linearity.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Control.Monad (foldM, unless)

type Row = V.Vector Coefficient

gaussianElimination :: MonadCompile m
                    => VariableNames
                    -> [LinearExpr]
                    -> Int
                    -> m ([LinearExpr], [LinearExpr])
gaussianElimination varNames exprs numberOfRowsToReduce =
  logCompilerPass currentPhase $ do
    logDebug MaxDetail $ prettyExprs varNames exprs

    let rowNumbers :: [Int] = [0 .. numberOfRowsToReduce-1]
    let rows = fmap unLinearExpr exprs
    (numberOfRowsReduced, reducedRows) <- foldM (reduceRow varNames) (0, rows) rowNumbers
    let (solvedExprs, unusedExprs) = splitAt numberOfRowsReduced (fmap LinearExpr reducedRows)

    unless (null unusedExprs) $
      logDebug MidDetail $
        line <> pretty ("Unused:" :: String) <> line <> prettyExprs varNames unusedExprs

    return (solvedExprs, unusedExprs)

prettyExprs :: VariableNames -> [LinearExpr] -> Doc a
prettyExprs varNames exprs = prettyAssertions varNames (fmap (Assertion Equals) exprs)

prettyRows :: VariableNames -> [Row] -> Doc a
prettyRows varNames rows = prettyExprs varNames (fmap LinearExpr rows)

-- | Tries to reduce the provided row in the matrix.
-- If unable to reduce it, then it returns the matrix unchanged.
reduceRow :: MonadCompile m => VariableNames -> (Int, [Row]) -> LinearVar -> m (Int, [Row])
reduceRow varNames (currentRow, rows) var = do
  let result = fromMaybe (currentRow, rows) $ do
        firstNonZeroRow <- findFirstNonZeroRowIndex rows var currentRow
        let matrix2 = swap rows var firstNonZeroRow
        let row = matrix2 !! currentRow
        let coefficient = row V.! var
        let normRow = V.map (/ coefficient) row
        let prevRows = eliminateVarFromRow var normRow <$> take currentRow matrix2
        let nextRows = eliminateVarFromRow var normRow <$> drop (currentRow+1) matrix2
        return (currentRow + 1, prevRows <> [normRow] <> nextRows)

  logDebug MaxDetail $
    line <> "After iteration" <+> pretty (var + 1) <> ":" <> line <>
    prettyRows varNames (snd result)
  return result

eliminateVarFromRow :: LinearVar -> Row -> Row -> Row
eliminateVarFromRow var normRow row =
  let coefficient = row V.! var in
  V.zipWith (\a b -> a - coefficient*b) row normRow

findFirstNonZeroRowIndex :: [Row] -> LinearVar -> Int -> Maybe Int
findFirstNonZeroRowIndex rows var startingIndex =
  (startingIndex +) <$> findIndex (\x -> x V.! var /= 0) (drop startingIndex rows)

swap :: [a] -> Int -> Int -> [a]
swap xs a b
  | a > b  = swap xs b a
  | a == b = xs
  | otherwise  = let
    (p1, p2) = (take a xs, drop (a+1) xs)
    (p3, p4) = (take (b-a-1) p2, drop (b-a) p2)
  in p1 <> [xs !! b] <> p3 <> [xs !! a] ++ p4

currentPhase :: Doc ()
currentPhase = "Gaussian elimination of user variable equalities"