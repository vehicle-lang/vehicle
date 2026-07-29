module Vehicle.Compile.Type.Constraint.Core
  ( runConstraintSolver,
    malformedConstraintError,
    extractHeadFromInstanceCandidate,
    findInstanceGoalHead,
    createInstanceUnification,
    mkCandidate,
    makeInstanceDatabase,
    instantiateInstanceConstraintSolution,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Map (fromListWith)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.DSL
import Vehicle.Data.Variable.Bound.Context.Generic

malformedConstraintError ::
  (PrintableBuiltin builtin, MonadCompile m) =>
  WithContext (InstanceConstraint builtin) ->
  m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed auxiliary constraint:" <+> prettyVerbose c

-- | Create a new unification constraint as a subgoal of an existing instance constraint.
createInstanceUnification ::
  (MonadTypeChecker builtin m) =>
  (ConstraintContext builtin, InstanceConstraintOrigin builtin) ->
  ThunkWithMetas builtin ->
  ThunkWithMetas builtin ->
  m (WithContext (UnificationConstraint builtin))
createInstanceUnification (ctx, origin) e1 e2 = do
  let unifyOrigin = CheckingInstanceType origin
  WithContext (Unify unifyOrigin e1 e2) <$> copyContext ctx Nothing

extractHeadFromInstanceCandidate ::
  (PrintableBuiltin builtin) =>
  InstanceCandidate builtin ->
  (InstanceHead builtin, InstanceCandidate builtin)
extractHeadFromInstanceCandidate candidate@InstanceCandidate {..} = do
  case findInstanceGoalHead candidateExpr of
    Right b -> (b, candidate)
    Left subexpr -> do
      let candidateDoc = prettyVerbose subexpr
      let problemDoc = prettyVerbose subexpr
      developerError $
        "Invalid builtin instance candidate:"
          <+> candidateDoc
          <> line
          <> "Problematic subexpr:"
            <+> problemDoc

mkCandidate ::
  (DSLExpr builtin, DSLExpr builtin, Maybe InstancePriority) ->
  InstanceCandidate builtin
mkCandidate (expr, solution, priority) = do
  let p = mempty
  let expr' = fromDSL p expr
  let solution' = fromDSL p solution
  InstanceCandidate expr' solution' priority

makeInstanceDatabase ::
  (Ord builtin, PrintableBuiltin builtin) =>
  [InstanceCandidate builtin] ->
  InstanceDatabase builtin
makeInstanceDatabase allInstances = do
  let tcAndCandidates = fmap (second (: []) . extractHeadFromInstanceCandidate) allInstances
  let instances = fromListWith (<>) (reverse tcAndCandidates)
  InstanceDatabase instances

instantiateInstanceConstraintSolution ::
  forall builtin m.
  (MonadTypeChecker builtin m, NormalisableBuiltin builtin) =>
  WithContext (InstanceConstraint builtin) ->
  Expr builtin ->
  m ()
instantiateInstanceConstraintSolution (WithContext (Resolve origin meta _ _ _) ctx) solution = do
  metaInfo <- getMetaInfo meta
  let boundCtx = boundContextOf ctx
  case metaSolution metaInfo of
    Nothing -> solveMeta meta solution boundCtx
    Just existingSolution -> do
      logDebug MaxDetail ("solved" <+> pretty meta <+> "as" <+> prettyVerbose solution)
      logDebug MaxDetail (indent 2 ("however" <+> pretty meta <+> "=" <+> prettyVerbose (unnormalised existingSolution) <+> "already so unifying"))
      let abstractedSolution = abstractOverCtx (metaCtx metaInfo) solution
      let normSolution = Unforced (boundContextToEnv boundCtx) abstractedSolution
      newConstraint <- createInstanceUnification (ctx, origin) normSolution (normalised existingSolution)
      addUnificationConstraints [newConstraint]
