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
import Data.Map (fromListWith, mapMaybeWithKey)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Variable
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Value
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
  Value builtin ->
  Value builtin ->
  m (WithContext (UnificationConstraint builtin))
createInstanceUnification (ctx, origin) e1 e2 = do
  let unifyOrigin = CheckingInstanceType origin
  WithContext (Unify unifyOrigin e1 e2) <$> copyContext ctx Nothing

extractHeadFromInstanceCandidate ::
  (PrintableBuiltin builtin) =>
  InstanceCandidate builtin ->
  (builtin, InstanceCandidate builtin)
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

findInstanceGoalHead :: Expr builtin -> Either (Expr builtin) builtin
findInstanceGoalHead = \case
  Pi _ binder body
    | not (isExplicit binder) -> findInstanceGoalHead body
  App (Builtin _ b) _ -> Right b
  Builtin _ b -> Right b
  expr -> Left expr

mkCandidate :: (DSLExpr builtin, DSLExpr builtin, Bool) -> InstanceCandidate builtin
mkCandidate (expr, solution, defaultInstance) = do
  let p = mempty
  let expr' = fromDSL p expr
  let solution' = fromDSL p solution
  InstanceCandidate expr' solution' defaultInstance

makeInstanceDatabase :: (PrintableBuiltin builtin, Ord builtin) => [InstanceCandidate builtin] -> InstanceDatabase builtin
makeInstanceDatabase allInstances = do
  let tcAndCandidates = fmap (second (: []) . extractHeadFromInstanceCandidate) allInstances
  let instances = fromListWith (<>) tcAndCandidates
  let defaults = mapMaybeWithKey findDefault instances
  InstanceDatabase instances defaults
  where
    findDefault :: (Pretty builtin) => builtin -> [InstanceCandidate builtin] -> Maybe (InstanceCandidate builtin)
    findDefault b instances = do
      let defaultInstances = filter defaultInstance instances
      case defaultInstances of
        [] -> Nothing
        [inst] -> Just inst
        _ -> developerError $ "Multiple default instances found for" <+> quotePretty b

instantiateInstanceConstraintSolution ::
  forall builtin m.
  (MonadTypeChecker builtin m, NormalisableBuiltin builtin) =>
  WithContext (InstanceConstraint builtin) ->
  Expr builtin ->
  m ()
instantiateInstanceConstraintSolution (WithContext (Resolve origin meta _ _) ctx) solution = do
  metaInfo <- getMetaInfo meta
  let boundCtx = boundContextOf ctx
  case metaSolution metaInfo of
    Nothing -> solveMeta meta solution boundCtx
    Just existingSolution -> do
      logDebug MaxDetail ("solved" <+> pretty meta <+> "as" <+> prettyVerbose solution)
      logDebug MaxDetail (indent 2 ("however" <+> pretty meta <+> "=" <+> prettyVerbose (unnormalised existingSolution) <+> "already so unifying"))
      let abstractedSolution = abstractOverCtx (metaCtx metaInfo) solution
      normSolution <- eval (toNamedBoundCtx boundCtx) (boundContextToEnv boundCtx) abstractedSolution
      newConstraint <- createInstanceUnification (ctx, origin) normSolution (normalised existingSolution)
      addUnificationConstraints [newConstraint]
