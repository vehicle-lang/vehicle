module Vehicle.Compile.Linearity
  ( module X
  , solveForUserVariables
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (partition)
import Data.Vector.Unboxed qualified as Vector

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Linearity.Core as X
import Vehicle.Compile.Linearity.GaussianElimination

solveForUserVariables :: MonadCompile m => Int -> CLSTProblem -> m CLSTProblem
solveForUserVariables numberOfUserVars (CLSTProblem varNames assertions) =
  logCompilerPass currentPass $ do
    -- First remove those assertions that don't have any user variables in them.
    let (withUserVars, withoutUserVars) =
          partition (hasUserVariables numberOfUserVars) assertions

    -- Then split out the equalities from the inequalities.
    let (equalitiesWithUserVars, inequalitiesWithUserVars) =
          partition isEquality withUserVars

    -- Try to reduce the equalities.
    (solvedEqualityExprs, unusedEqualityExprs) <-
      gaussianElimination varNames (map assertionExpr equalitiesWithUserVars) numberOfUserVars

    if length solvedEqualityExprs /= numberOfUserVars
      then compilerDeveloperError "Underconstrained set of user variables not yet supported"
      else do
        let unusedEqualities = fmap (Assertion Equals) unusedEqualityExprs
        let userVarMap = createUserVarMap solvedEqualityExprs
        let reducedInequalities =
              flip fmap inequalitiesWithUserVars $ \assertion ->
                foldr (uncurry substitute) assertion (Map.assocs userVarMap)

        let finalAssertions = withoutUserVars <> unusedEqualities <> reducedInequalities
        return (CLSTProblem varNames finalAssertions)

hasUserVariables :: Int -> Assertion -> Bool
hasUserVariables numberOfUserVariables (Assertion _ (LinearExpr e)) =
  let userCoefficients = Vector.take numberOfUserVariables e in
  Vector.any (/= 0) userCoefficients

createUserVarMap :: [LinearExpr] -> Map LinearVar LinearExpr
createUserVarMap assertions = Map.fromAscList $ flip fmap assertions $
  \l@(LinearExpr e) -> case Vector.findIndex (== 1) e of
    Nothing -> developerError "Malformed user variable solution"
    Just v  -> (v, l)

substitute :: LinearVar -> LinearExpr -> Assertion -> Assertion
substitute var (LinearExpr e1) (Assertion r2 (LinearExpr e2)) =
  let coeff = e2 Vector.! var in
  let e2'  = Vector.zipWith (\a b -> b - coeff * a) e1 e2 in
  Assertion r2 (LinearExpr e2')

currentPass :: Doc ()
currentPass = "elimination of user variables"