module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( resolveInstance,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Prettyprinter (list)
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message (MeaningfulError (..))
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin, prettyExternal, prettyFriendly)
import Vehicle.Compile.Type (runUnificationSolver)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Data.DeBruijn (dbLevelToIndex, substDBInto)
import Vehicle.Data.NormalisedExpr

--------------------------------------------------------------------------------
-- Public interface

resolveInstance ::
  forall builtin m.
  (Hashable builtin, MonadInstance builtin m) =>
  InstanceCandidateDatabase builtin ->
  WithContext (InstanceConstraint builtin) ->
  m ()
resolveInstance allCandidates (WithContext constraint ctx) = do
  normConstraint@(Resolve origin meta relevance expr) <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly (WithContext normConstraint ctx)

  goal <- parseInstanceGoal expr
  let candidates = fromMaybe [] $ Map.lookup (goalHead goal) allCandidates
  solveInstanceGoal candidates ctx origin meta relevance goal

--------------------------------------------------------------------------------
-- Algorithm

type MonadInstance builtin m = TCM builtin m

-- The algorithm for this is taken from
-- https://agda.readthedocs.io/en/v2.6.2.2/language/instance-arguments.html#instance-resolution

solveInstanceGoal ::
  forall builtin m.
  (MonadInstance builtin m) =>
  [InstanceCandidate builtin] ->
  ConstraintContext builtin ->
  InstanceConstraintOrigin builtin ->
  MetaID ->
  Relevance ->
  InstanceGoal builtin ->
  m ()
solveInstanceGoal rawBuiltinCandidates ctx origin meta relevance goal = do
  let boundCtx = boundContext ctx

  -- The builtin candidates have access to the entire bound context
  let builtinCandidates = fmap (`WithContext` boundCtx) rawBuiltinCandidates
  -- Find the candidates in the bound context.
  candidatesInBoundCtx <- findCandidatesInBoundCtx goal boundCtx
  let allCandidates = builtinCandidates <> candidatesInBoundCtx

  logDebug MaxDetail $
    line
      <> "Builtin candidates:"
      <> line
      <> indent 2 (list (fmap prettyCandidate builtinCandidates))
      <> line
      <> "Context candidates:"
      <> line
      <> indent 2 (list (fmap prettyCandidate candidatesInBoundCtx))
      <> line

  -- Try all candidates
  successfulCandidates <- catMaybes <$> traverse (checkCandidate (ctx, origin) meta goal) allCandidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [(candidate, typeCheckerState)] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyCandidate candidate)
      adoptHypotheticalState typeCheckerState

    -- If there are no valid candidates then we fail.
    [] -> do
      substOrigin <- substMetas origin
      throwError $ TypingError $ FailedInstanceConstraint ctx substOrigin goal allCandidates

    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      logDebug MaxDetail "Multiple possible candidates found so deferring."
      let constraint = WithContext (InstanceConstraint (Resolve origin meta relevance (goalExpr goal))) ctx
      -- TODO can we be more precise with the set of blocking metas?
      blockedConstraint <- blockConstraintOn constraint <$> getUnsolvedMetas (Proxy @builtin)
      addConstraints [blockedConstraint]

-- | Locates any more candidates that are in the bound context of the constraint
findCandidatesInBoundCtx ::
  forall builtin m.
  (MonadInstance builtin m) =>
  InstanceGoal builtin ->
  BoundCtx builtin ->
  m [WithContext (InstanceCandidate builtin)]
findCandidatesInBoundCtx goal ctx = go ctx
  where
    go :: (MonadCompile m) => BoundCtx builtin -> m [WithContext (InstanceCandidate builtin)]
    go = \case
      [] -> return []
      (binder : localCtx) -> do
        candidates <- go localCtx
        let binderType = typeOf binder
        case findInstanceGoalHead binderType of
          Right binderHead | binderHead == goalHead goal -> do
            let candidate =
                  InstanceCandidate
                    { candidateExpr = binderType,
                      candidateSolution = BoundVar mempty (dbLevelToIndex (Lv $ length ctx) (Lv $ length localCtx))
                    }
            return $ WithContext candidate localCtx : candidates
          _ -> return candidates

-- | Checks whether a candidate is a possibility for the instance goal.
-- Returns `Nothing` if it is definitely not a valid candidate and
-- `Just` if it might be a valid candidate.
checkCandidate ::
  forall builtin m.
  (MonadInstance builtin m) =>
  InstanceConstraintInfo builtin ->
  MetaID ->
  InstanceGoal builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Maybe (WithContext (InstanceCandidate builtin), TypeCheckerState builtin))
checkCandidate info@(constraintCtx, constraintOrigin) meta goal@InstanceGoal {..} candidate = do
  let candidateDoc = squotes (prettyCandidate candidate)
  logCompilerPass MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerTHypothetically $ do
      -- Allow the candidate to access all the arguments in the goal telescope.
      let goalCtxExtension = goalTelescope
      let extendedGoalCtx = goalCtxExtension ++ boundContext constraintCtx
      let extendedGoalInfo = (setConstraintBoundCtx constraintCtx extendedGoalCtx, constraintOrigin)

      logCompilerSection MaxDetail "hypothetically accepting candidate" $ do
        -- Instantiate the candidate telescope with metas and subst into body.
        (substCandidateExpr, substCandidateSolution, recInstanceConstraints) <-
          instantiateCandidateTelescope goalCtxExtension info candidate

        -- Unify the goal and candidate bodies
        unificationConstraint <-
          createInstanceUnification extendedGoalInfo (goalExpr goal) substCandidateExpr

        -- Replace the provenance of the final solution with the provenance of where the
        -- constraint was generated. This is needed to get the information to propagate
        -- properly for the polarity and linearity types, otherwise the provenance ends
        -- up empty as the candidates are constructed independently.
        let finalCandidateSolution = replaceProvenance (provenanceOf constraintCtx) substCandidateSolution

        -- Add the solution of the type-class as well (if we had first class records
        -- then we wouldn't need to do this manually).
        solveMeta meta finalCandidateSolution extendedGoalCtx

        addConstraints (unificationConstraint : recInstanceConstraints)

      runUnificationSolver (Proxy @builtin) mempty

    case result of
      Left err -> do
        logDebug MaxDetail $ line <> "Rejecting" <+> candidateDoc <+> "as a possibility"
        logDebug MaxDetail $ indent 2 (pretty (details err)) <> line
        return Nothing
      Right (_, state) -> do
        logDebug MaxDetail $ "Keeping" <+> candidateDoc <+> "as a possibility" <> line
        return $ Just (candidate, state)

-- | Generate meta variables for each binder in the telescope of the candidate
-- and then substitute them into the candidate expression.
instantiateCandidateTelescope ::
  forall builtin m.
  (MonadInstance builtin m) =>
  BoundCtx builtin ->
  InstanceConstraintInfo builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (WHNFValue builtin, Expr Ix builtin, [WithContext (Constraint builtin)])
instantiateCandidateTelescope goalCtxExtension (constraintCtx, constraintOrigin) candidate = do
  let WithContext InstanceCandidate {..} candidateCtx = candidate
  logCompilerSection MaxDetail "instantiating candidate telescope" $ do
    let initialCtx = goalCtxExtension ++ candidateCtx
    (candidateBody, candidateSol, newInstanceConstraints, finalCtx) <-
      go (candidateExpr, candidateSolution, [], initialCtx)
    normCandidateBody <- normaliseInEnv (boundContextToEnv finalCtx) candidateBody
    return (normCandidateBody, candidateSol, newInstanceConstraints)
  where
    go ::
      (MonadInstance builtin m) =>
      (Type Ix builtin, Expr Ix builtin, [WithContext (Constraint builtin)], BoundCtx builtin) ->
      m (Type Ix builtin, Expr Ix builtin, [WithContext (Constraint builtin)], BoundCtx builtin)
    go = \case
      (Pi _ exprBinder exprBody, Lam _ _solutionBinder solutionBody, constraints, boundCtx) -> do
        let binderType = typeOf exprBinder
        (newArg, newConstraints) <- case visibilityOf exprBinder of
          Explicit {} ->
            compilerDeveloperError "Should not have an explicit argument in instance goal telescope"
          Implicit {} -> do
            let p = provenanceOf constraintCtx
            expr <- freshMetaExpr p binderType boundCtx
            return (unnormalised expr, [])
          Instance {} -> do
            let newInfo = (setConstraintBoundCtx constraintCtx boundCtx, constraintOrigin)
            -- WARNING massive hack should be traversing the normalised type here.
            normBinderType <- normaliseInEnv (boundContextToEnv boundCtx) binderType
            (expr, constraint) <- createSubInstance newInfo (relevanceOf exprBinder) normBinderType
            return (expr, [constraint])
        let exprBodyResult = newArg `substDBInto` exprBody
        let solutionBodyResult = newArg `substDBInto` solutionBody
        go (exprBodyResult, solutionBodyResult, newConstraints <> constraints, boundCtx)
      body -> return body

-- TODO move this to Print
prettyCandidate :: (PrintableBuiltin builtin) => WithContext (InstanceCandidate builtin) -> Doc a
prettyCandidate (WithContext candidate ctx) =
  prettyExternal (WithContext (candidateExpr candidate) (toNamedBoundCtx ctx))

goalExpr :: InstanceGoal builtin -> WHNFValue builtin
goalExpr InstanceGoal {..} = VBuiltin goalHead goalSpine

replaceProvenance :: Provenance -> Expr Ix builtin -> Expr Ix builtin
replaceProvenance p = go
  where
    go :: Expr Ix builtin -> Expr Ix builtin
    go = \case
      Meta _p m -> Meta p m
      App _ fun args -> App p (go fun) (fmap (fmap go) args)
      Universe _ u -> Universe p u
      Hole _ h -> Hole p h
      Builtin _ b -> Builtin p b
      FreeVar _ v -> FreeVar p v
      BoundVar _ v -> BoundVar p v
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi _ binder res -> Pi p (fmap go binder) (go res)
      Let _ e1 binder e2 -> Let p (go e1) (fmap go binder) (go e2)
      Lam _ binder e -> Lam p (fmap go binder) (go e)
