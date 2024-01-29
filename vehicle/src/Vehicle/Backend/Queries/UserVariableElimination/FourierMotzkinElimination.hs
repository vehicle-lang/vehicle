module Vehicle.Backend.Queries.UserVariableElimination.FourierMotzkinElimination
  ( FourierMotzkinVariableSolution,
    fourierMotzkinElimination,
    reconstructFourierMotzkinVariableValue,
  )
where

import Control.Monad (foldM)
import Data.Map (Map)
import Vehicle.Backend.Queries.UserVariableElimination.Core (FourierMotzkinVariableSolution (..), RationalInequality (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.LinearExpr
import Vehicle.Syntax.Builtin (Strictness (..))
import Vehicle.Verify.Variable

-- | TODO If performance proves unnacceptably poor look into
-- Imbert's acceleration theorems:
--
-- Jean-Louis Imbert,
-- About Redundant Inequalities Generated by Fourier's Algorithm
-- Artificial Intelligence IV: Methodology, Systems, Applications, 1990
fourierMotzkinElimination ::
  (MonadCompile m) =>
  UserRationalVariable ->
  [RationalInequality] ->
  m (FourierMotzkinVariableSolution, [RationalInequality])
fourierMotzkinElimination var inequalities = do
  let (less, greater, unusedInequalities) = partition (UserRationalVar var) inequalities
  let solution = FMSolution less greater

  let newInequalities = fmap combineInequalities [(x, y) | x <- less, y <- greater]

  logDebug MaxDetail $
    line
      <> "After FM solving for"
        <+> pretty var
      <> ":"
      <> line
      <> indent
        2
        ( "LHS inequalities:"
            <> pretty less
            <> line
            <> "RHS inequalities:"
            <> pretty greater
            <> line
            <> "New inequalities:"
            <> pretty newInequalities
        )

  return (solution, newInequalities <> unusedInequalities)

combineInequalities :: (RationalInequality, RationalInequality) -> RationalInequality
combineInequalities (RationalInequality rel1 expr1, RationalInequality rel2 expr2) =
  let rel = case (rel1, rel2) of
        (Strict, _) -> Strict
        (_, Strict) -> Strict
        (_, _) -> NonStrict
   in RationalInequality
        { rationalIneqExpr = addExprs 1 1 expr1 expr2,
          strictness = rel
        }

-- | Partitions the inequalities into three sets:
--  1. Those where the rest of the expression is less than the variable
--  2. Those where the rest of the expression is greater than the variable
--  3. Those which don't mention the variable at all.
partition ::
  RationalVariable ->
  [RationalInequality] ->
  ([RationalInequality], [RationalInequality], [RationalInequality])
partition var = foldr categorise ([], [], [])
  where
    categorise ::
      RationalInequality ->
      ([RationalInequality], [RationalInequality], [RationalInequality]) ->
      ([RationalInequality], [RationalInequality], [RationalInequality])
    categorise a@(RationalInequality rel expr) (less, greater, unused) = do
      let coeff = lookupCoefficient expr var
      if coeff < 0
        then do
          let coeff' = -coeff
          let expr' = scaleExpr (1 / coeff') expr
          let a' = RationalInequality rel expr'
          (a' : less, greater, unused)
        else
          if coeff > 0
            then do
              let expr' = scaleExpr (1 / coeff) expr
              let a' = RationalInequality rel expr'
              (less, a' : greater, unused)
            else (less, greater, a : unused)

--------------------------------------------------------------------------------
-- Solutions

-- | Tries to reconstruct the value of the variable that is
-- consistent with the current assignment of variables. Returns either a
-- required variable that is missing from the assignment or the reconstructed
-- value.
reconstructFourierMotzkinVariableValue ::
  Map RationalVariable Rational ->
  FourierMotzkinVariableSolution ->
  Either RationalVariable Rational
reconstructFourierMotzkinVariableValue assignment solution = do
  let inf = 1 / 0
  let negInf = -1 / 0
  let initialMax = (negInf, NonStrict)
  let initialMin = (inf, NonStrict)
  (lowerBound, minRel) <- foldM evaluateMaxValue initialMax (lowerBounds solution)
  (upperBound, maxRel) <- foldM evaluateMinValue initialMin (upperBounds solution)

  if lowerBound < upperBound || minRel == NonStrict && maxRel == NonStrict
    then return $ addConstants 0.5 0.5 lowerBound upperBound
    else -- Only 99% sure about this. Can't find a good reference to the reconstruction phase of the
    -- algorithm. Closest to referencing this impossibility is:
    -- https://people.math.carleton.ca/~kcheung/math/notes/MATH5801/02/2_1_fourier_motzkin.html
      developerError "Fourier-Motzkin reconstruction failed. This isn't supposed to be possible..."
  where
    evaluateMinValue ::
      (Rational, Strictness) ->
      RationalInequality ->
      Either RationalVariable (Rational, Strictness)
    evaluateMinValue current@(currentMin, _) (RationalInequality rel expr) = do
      value <- evaluateExpr expr assignment
      return $
        if (value < currentMin) || (value == currentMin && rel == Strict)
          then (value, rel)
          else current

    evaluateMaxValue ::
      (Rational, Strictness) ->
      RationalInequality ->
      Either RationalVariable (Rational, Strictness)
    evaluateMaxValue current@(currentMax, _) (RationalInequality rel expr) = do
      value <- evaluateExpr expr assignment
      return $
        if (value > currentMax) || (value == currentMax && rel == Strict)
          then (value, rel)
          else current