{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.TypeSystem.InstanceDefaults where

import Vehicle.Backend.LossFunction.TypeSystem.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core (parseInstanceGoal)
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
import Vehicle.Compile.Type.Core
import Vehicle.Expr.Normalised (Spine, Value (..), isNMeta)
import Vehicle.Syntax.Builtin hiding (BuiltinType)

instance HasInstanceDefaults LossBuiltin where
  getCandidatesFromConstraint = getCandidates
  compareCandidates = compareCandidate

--------------------------------------------------------------------------------
-- Default solutions to type-class constraints

compareCandidate :: Candidate LossBuiltin -> Candidate LossBuiltin -> Maybe Ordering
compareCandidate _c1 _c2 = Just EQ

getCandidates ::
  forall m.
  (MonadCompile m) =>
  ConstraintContext LossBuiltin ->
  InstanceConstraint LossBuiltin ->
  m [Candidate LossBuiltin]
getCandidates ctx (Resolve origin _ _ expr) = do
  InstanceGoal {..} <- parseInstanceGoal expr
  let defaults = case (goalHead, goalSpine) of
        (LossTC HasBoolLiteral {}, [tArg]) -> Just [tArg]
        (LossTC HasNot, [tArg, _tRes]) -> Just [tArg]
        (LossTC HasAnd, [tArg1, tArg2, _tRes]) -> Just [tArg1, tArg2]
        (LossTC HasOr, [tArg1, tArg2, _tRes]) -> Just [tArg1, tArg2]
        (LossTC HasImplies, [tArg1, tArg2, _tRes]) -> Just [tArg1, tArg2]
        _ -> Nothing

  return $ case defaults of
    Nothing -> []
    Just defaultArgs ->
      getCandidatesFromArgs (ctx, origin) goalHead defaultArgs

getCandidatesFromArgs ::
  InstanceConstraintInfo LossBuiltin ->
  LossBuiltin ->
  Spine LossBuiltin ->
  [Candidate LossBuiltin]
getCandidatesFromArgs info tc ts = map mkCandidate (filter (isNMeta . argExpr) ts)
  where
    mkCandidate t =
      Candidate
        { candidateTypeClass = tc,
          candidateMetaExpr = argExpr t,
          candidateInfo = info,
          candidateSolution = VBuiltin (BuiltinType Bool) []
        }
