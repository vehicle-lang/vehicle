module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( solveInstanceConstraint,
    acceptCandidate,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Prettyprinter (list)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin, prettyExternal, prettyFriendly)
import Vehicle.Compile.Print.Error (MeaningfulError (..))
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.UnificationSolver (runUnificationSolver)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class (addInstanceConstraints)
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn (dbLevelToIndex)

--------------------------------------------------------------------------------
-- Public interface

solveInstanceConstraint ::
  forall builtin m.
  (Hashable builtin, MonadInstance builtin m) =>
  InstanceDatabase builtin ->
  WithContext (InstanceConstraint builtin) ->
  m ()
solveInstanceConstraint database constraint = do
  normConstraint <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly normConstraint

  let goal = parseInstanceGoal normConstraint
  let candidates = lookupInstances database goal
  solveInstanceGoal normConstraint candidates goal

--------------------------------------------------------------------------------
-- Algorithm

type MonadInstance builtin m =
  MonadTypeChecker
    builtin
    m

-- The algorithm for this is taken from
-- https://agda.readthedocs.io/en/v2.6.2.2/language/instance-arguments.html#instance-resolution

solveInstanceGoal ::
  forall builtin m.
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  [InstanceCandidate builtin] ->
  InstanceGoal builtin ->
  m ()
solveInstanceGoal constraint rawBuiltinCandidates goal = do
  let boundCtx = boundContext $ contextOf constraint
  candidatesInBoundCtx <- findCandidatesInBoundCtx goal boundCtx
  -- The previously declared candidates have access to the entire bound context
  let builtinCandidates = fmap (`WithContext` boundCtx) rawBuiltinCandidates
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
  successfulCandidates <- catMaybes <$> traverse (checkCandidate constraint goal) allCandidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [(candidate, typeCheckerState)] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyCandidate candidate)
      adoptHypotheticalState typeCheckerState

    -- If there are no valid candidates then we fail.
    [] -> do
      finalConstraint <- substMetas constraint
      throwError $ TypingError $ FailedInstanceConstraint finalConstraint allCandidates

    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      logDebug MaxDetail "Multiple possible candidates found so deferring."
      -- TODO can we be more precise with the set of blocking metas?
      blockedConstraint <- blockConstraintOn constraint <$> getUnsolvedMetas (Proxy @builtin)
      addInstanceConstraints [blockedConstraint]

-- | Locates any more candidates that are in the bound context of the constraint
findCandidatesInBoundCtx ::
  forall builtin m.
  (MonadInstance builtin m) =>
  InstanceGoal builtin ->
  BoundCtx (Type builtin) ->
  m [WithContext (InstanceCandidate builtin)]
findCandidatesInBoundCtx goal ctx = go ctx
  where
    go :: (MonadCompile m) => BoundCtx (Type builtin) -> m [WithContext (InstanceCandidate builtin)]
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
                      candidateSolution = BoundVar mempty (dbLevelToIndex (Lv $ length ctx) (Lv $ length localCtx)),
                      defaultInstance = False
                    }
            return $ WithContext candidate localCtx : candidates
          _ -> return candidates

-- | Checks whether a candidate is a possibility for the instance goal.
-- Returns `Nothing` if it is definitely not a valid candidate and
-- `Just` if it might be a valid candidate.
checkCandidate ::
  forall builtin m.
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  InstanceGoal builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Maybe (WithContext (InstanceCandidate builtin), TypeCheckerState builtin))
checkCandidate constraint goal candidate = do
  let candidateDoc = squotes (prettyCandidate candidate)
  logCompilerPass MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerTHypothetically $ do
      logCompilerSection MaxDetail "hypothetically accepting candidate" $
        acceptCandidate constraint goal candidate
      runUnificationSolver (Proxy @builtin) mempty

    case result of
      Left err -> do
        logDebug MaxDetail $ line <> "Rejecting" <+> candidateDoc <+> "as a possibility"
        logDebug MaxDetail $ indent 2 (pretty (details err)) <> line
        return Nothing
      Right (_, state) -> do
        logDebug MaxDetail $ "Keeping" <+> candidateDoc <+> "as a possibility" <> line
        return $ Just (candidate, state)

acceptCandidate ::
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  InstanceGoal builtin ->
  WithContext (InstanceCandidate builtin) ->
  m ()
acceptCandidate (WithContext Resolve {..} constraintCtx) goal candidate = do
  -- Allow the candidate to access all the arguments in the goal telescope.
  let goalCtxExtension = goalTelescope goal
  let extendedGoalCtx = goalCtxExtension ++ boundContext constraintCtx
  let extendedGoalInfo = (setConstraintBoundCtx constraintCtx extendedGoalCtx, instanceOrigin)

  -- Instantiate the candidate telescope with metas and subst into body.
  (substCandidateExpr, substCandidateSolution, recInstanceConstraints) <-
    instantiateCandidateTelescope goalCtxExtension (constraintCtx, instanceOrigin) candidate
  addInstanceConstraints recInstanceConstraints

  -- Unify the goal and candidate bodies
  unificationConstraint <- createInstanceUnification extendedGoalInfo (goalExpr goal) substCandidateExpr
  addUnificationConstraints [unificationConstraint]

  -- Replace the provenance of the final solution with the provenance of where the
  -- constraint was generated. This is needed to get the information to propagate
  -- properly for the polarity and linearity types, otherwise the provenance ends
  -- up empty as the candidates are constructed independently.
  let finalCandidateSolution = replaceProvenance (provenanceOf constraintCtx) substCandidateSolution

  -- Add the solution of the type-class as well (if we had first class records
  -- then we wouldn't need to do this manually).
  solveMeta instanceSolutionMeta finalCandidateSolution extendedGoalCtx

-- | Generate meta variables for each binder in the telescope of the candidate
-- and then substitute them into the candidate expression.
instantiateCandidateTelescope ::
  forall builtin m.
  (MonadInstance builtin m) =>
  BoundCtx (Type builtin) ->
  InstanceConstraintInfo builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Value builtin, Expr builtin, [WithContext (InstanceConstraint builtin)])
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
      (Type builtin, Expr builtin, [WithContext (InstanceConstraint builtin)], BoundCtx (Type builtin)) ->
      m (Type builtin, Expr builtin, [WithContext (InstanceConstraint builtin)], BoundCtx (Type builtin))
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

goalExpr :: InstanceGoal builtin -> Value builtin
goalExpr InstanceGoal {..} = VBuiltin goalHead goalSpine

replaceProvenance :: Provenance -> Expr builtin -> Expr builtin
replaceProvenance p = go
  where
    go :: Expr builtin -> Expr builtin
    go = \case
      Meta _p m -> Meta p m
      App fun args -> App (go fun) (fmap (fmap go) args)
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
