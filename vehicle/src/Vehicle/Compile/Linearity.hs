module Vehicle.Compile.Linearity
  ( module X
  , solveForUserVariables
  , reconstructUserVars
  ) where

import Control.Monad (foldM)
import Data.Bifunctor
import Data.List (partition)
import Data.Set qualified as Set (difference, fromList)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

import Vehicle.Compile.Error
import Vehicle.Compile.Linearity.Core as X
import Vehicle.Compile.Linearity.FourierMotzkinElimination (fourierMotzkinElimination,
                                                            reconstructFMUserVar)
import Vehicle.Compile.Linearity.GaussianElimination (gaussianElimination)
import Vehicle.Compile.Prelude hiding (substitute)

solveForUserVariables :: MonadCompile m
                      => Int
                      -> CLSTProblem
                      -> m (CLSTProblem, UserVarReconstructionInfo)
solveForUserVariables numberOfUserVars (CLSTProblem varNames assertions) =
  logCompilerPass MinDetail currentPass $ do
    let allUserVars = Set.fromList [0..numberOfUserVars-1]

    -- First remove those assertions that don't have any user variables in them.
    let (withUserVars, withoutUserVars) =
          partition (hasUserVariables numberOfUserVars) assertions

    -- Then split out the equalities from the inequalities.
    let (equalitiesWithUserVars, inequalitiesWithUserVars) =
          partition isEquality withUserVars

    -- Try to solve for user variables using Gaussian elimination.
    (solvedEqualityExprs, unusedEqualityExprs) <-
      gaussianElimination varNames (map assertionExpr equalitiesWithUserVars) numberOfUserVars
    let unusedEqualities = fmap (Assertion Equal) unusedEqualityExprs
    let gaussianElimSolutions = fmap (second RecEquality) solvedEqualityExprs

    -- Eliminate the solved user variables in the inequalities
    let reducedInequalities =
          flip fmap inequalitiesWithUserVars $ \assertion ->
            foldl (uncurry . substitute) assertion solvedEqualityExprs

    -- Calculate the set of unsolved user variables
    let varsSolvedByGaussianElim = Set.fromList (fmap fst solvedEqualityExprs)
    let varsUnsolvedByGaussianElim = Set.difference allUserVars varsSolvedByGaussianElim

    -- Eliminate the remaining unsolved user vars using Fourier-Motzkin elimination
    (fmElimSolutions, fmElimOutputInequalities) <-
      fourierMotzkinElimination varNames varsUnsolvedByGaussianElim reducedInequalities

    -- Calculate the way to reconstruct the user variables
    let varSolutions = fmElimSolutions <> gaussianElimSolutions

    -- Calculate the final set of (user-variable free) assertions
    let finalAssertions = withoutUserVars <> unusedEqualities <> fmElimOutputInequalities

    -- Return the problem
    return (CLSTProblem varNames finalAssertions, varSolutions)

hasUserVariables :: Int -> Assertion -> Bool
hasUserVariables numberOfUserVariables (Assertion _ (LinearExpr e)) =
  let userCoefficients = Vector.take numberOfUserVariables e in
  Vector.any (/= 0) userCoefficients

substitute :: Assertion -> LinearVar -> LinearExpr -> Assertion
substitute (Assertion r2 (LinearExpr e2)) var (LinearExpr e1) =
  let coeff = e2 Vector.! var in
  let e2'  = Vector.zipWith (\a b -> b - coeff * a) e1 e2 in
  Assertion r2 (LinearExpr e2')

reconstructUserVars :: UserVarReconstructionInfo
                    -> Vector Double
                    -> Maybe (Vector Double)
reconstructUserVars info ioVarValues = do
  let numberOfUserVars = length info
  let startingUserVarValues = Vector.replicate numberOfUserVars 0
  let constantValue = Vector.singleton 1.0
  let startingValues = startingUserVarValues <> ioVarValues <> constantValue
  let reconstructedVars = foldM reconstructVar startingValues info
  Vector.take numberOfUserVars <$> reconstructedVars
  where
    reconstructVar :: Vector Double
                -> (LinearVar, VarReconstruction)
                -> Maybe (Vector Double)
    reconstructVar current (var, varInfo) = do
      value <- case varInfo of
        RecEquality e -> Just $ evaluateExpr e current
        RecInequalities less greater -> reconstructFMUserVar current less greater
      return $ Vector.update current [(var, value)]

currentPass :: Doc ()
currentPass = "elimination of user variables"
