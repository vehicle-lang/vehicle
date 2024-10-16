module Vehicle.Compile.FourierMotzkinElimination
  ( fourierMotzkinElimination,
    reconstructFourierMotzkinVariableValue,
  )
where

import Control.Monad (foldM)
import Data.Map (Map)
import GHC.Real (infinity)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Builtin.Core (Strictness (..))
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (Tensor (..))

-- | TODO If performance proves unnacceptably poor look into
-- Imbert's acceleration theorems:
--
-- Jean-Louis Imbert,
-- About Redundant Inequalities Generated by Fourier's Algorithm
-- Artificial Intelligence IV: Methodology, Systems, Applications, 1990
fourierMotzkinElimination ::
  (MonadCompile m) =>
  Variable ->
  [Inequality] ->
  m (Bounds, [Inequality])
fourierMotzkinElimination var inequalities = do
  let (solution@(Bounds lower upper), unusedInequalities) = partition var inequalities
  let newInequalities = fmap combineInequalities [(x, y) | x <- lower, y <- upper]
  return (solution, newInequalities <> unusedInequalities)

combineInequalities ::
  (LowerBound, UpperBound) ->
  Inequality
combineInequalities (Inequality rel1 lowerBound, Inequality rel2 upperBound) =
  Inequality
    { inequalityExpr = addExprs 1 (-1) lowerBound upperBound,
      strictness = case (rel1, rel2) of
        (Strict, _) -> Strict
        (_, Strict) -> Strict
        (_, _) -> NonStrict
    }

-- | Partitions the inequalities into three sets:
--  1. Those where the rest of the expression is less than the variable
--  2. Those where the rest of the expression is greater than the variable
--  3. Those which don't mention the variable at all.
partition :: Variable -> [Inequality] -> (Bounds, [Inequality])
partition var = foldr categorise (Bounds [] [], [])
  where
    categorise ::
      Inequality ->
      (Bounds, [Inequality]) ->
      (Bounds, [Inequality])
    categorise ineq@(Inequality rel expr) (bounds@Bounds {..}, unused) = do
      let (coeff, valueExpr) = rearrangeExprToSolveFor var expr
      let bound = Bound rel valueExpr
      if coeff < 0
        then (Bounds {lowerBounds = bound : lowerBounds, ..}, unused)
        else
          if coeff > 0
            then (Bounds {upperBounds = bound : upperBounds, ..}, unused)
            else (bounds, ineq : unused)

--------------------------------------------------------------------------------
-- Solutions

-- | Tries to reconstruct the value of the variable that is
-- consistent with the current assignment of variables. Returns either a
-- required variable that is missing from the assignment or the reconstructed
-- value.
reconstructFourierMotzkinVariableValue ::
  Map ElementVariable Constant ->
  Bounds ->
  Either ElementVariable Constant
reconstructFourierMotzkinVariableValue assignment solution = do
  let initialMax = (-infinity, NonStrict)
  let initialMin = (infinity, NonStrict)
  (lowerBound, minRel) <- foldM evaluateMaxValue initialMax (lowerBounds solution)
  (upperBound, maxRel) <- foldM evaluateMinValue initialMin (upperBounds solution)

  let validBound =
        lowerBound < upperBound
          -- Turns out `-infinity < infinity` returns `False`
          || lowerBound == -infinity && upperBound == infinity
          || (lowerBound == upperBound && minRel == NonStrict && maxRel == NonStrict)

  if validBound
    then return $ Tensor mempty [pickValue lowerBound upperBound]
    else do
      -- Only 99% sure about this. Can't find a good reference to the reconstruction phase of the
      -- algorithm. Closest to referencing this impossibility is:
      -- https://people.math.carleton.ca/~kcheung/math/notes/MATH5801/02/2_1_fourier_motzkin.html
      developerError "Fourier-Motzkin reconstruction failed. This isn't supposed to be possible..."
  where
    evaluateMinValue ::
      (Rational, Strictness) ->
      LowerBound ->
      Either ElementVariable (Rational, Strictness)
    evaluateMinValue current@(currentMin, _) (Bound rel expr) = do
      value <- extractValue <$> evaluateExpr expr assignment
      return $
        if (value < currentMin) || (value == currentMin && rel == Strict)
          then (value, rel)
          else current

    evaluateMaxValue ::
      (Rational, Strictness) ->
      UpperBound ->
      Either ElementVariable (Rational, Strictness)
    evaluateMaxValue current@(currentMax, _) (Bound rel expr) = do
      value <- extractValue <$> evaluateExpr expr assignment
      return $
        if (value > currentMax) || (value == currentMax && rel == Strict)
          then (value, rel)
          else current

    extractValue :: Constant -> Rational
    extractValue (Tensor [] [v]) = v
    extractValue _ = developerError "FM-elimination doesn't yet work over tensors"

    pickValue :: Rational -> Rational -> Rational
    pickValue lowerBound upperBound
      | lowerBound == -infinity && upperBound == infinity = 0
      | lowerBound == -infinity = upperBound - 1.0
      | upperBound == infinity = lowerBound + 1.0
      | otherwise = 0.5 * (lowerBound + upperBound)
