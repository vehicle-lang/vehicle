module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( runInstanceSolver,
    acceptCandidate,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Either (partitionEithers)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Print.Error (formatCompileError)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.UnificationSolver (runUnificationSolver)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin)
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Bound.Level (dbLevelToIndex)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))

--------------------------------------------------------------------------------
-- Public interface

-- | Attempts to solve as many instance constraints as possible.
runInstanceSolver ::
  (MonadInstance builtin m, TypableBuiltin builtin) =>
  Proxy builtin ->
  InstanceSearchDepth ->
  m ()
runInstanceSolver proxy depth = do
  logCompilerSection2 MaxDetail "instance solver run" $
    runConstraintSolver
      getActiveInstanceConstraints
      setInstanceConstraints
      (solveInstanceConstraint depth)
      True
      proxy

--------------------------------------------------------------------------------
-- Algorithm

type MonadInstance builtin m =
  ( MonadTypeChecker builtin m,
    TypableBuiltin builtin
  )

-- The algorithm for this is taken from
-- https://agda.readthedocs.io/en/v2.6.2.2/language/instance-arguments.html#instance-resolution

solveInstanceConstraint ::
  forall builtin m.
  (MonadInstance builtin m) =>
  InstanceSearchDepth ->
  WithContext (InstanceConstraint builtin) ->
  m ()
solveInstanceConstraint depth constraint = do
  normConstraint <- substMetaVariables constraint
  logDebug MaxDetail $ "Forced:" <+> prettyExternal normConstraint

  let goal = instanceGoal $ objectIn normConstraint
  candidates <- getInstanceCandidates goal
  solveInstanceGoal normConstraint candidates depth goal

solveInstanceGoal ::
  forall builtin m.
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  [InstanceCandidate builtin] ->
  InstanceSearchDepth ->
  InstanceGoal builtin ->
  m ()
solveInstanceGoal constraint rawBuiltinCandidates depth goal = do
  let boundCtx = boundContext $ contextOf constraint
  candidatesInBoundCtx <- findCandidatesInBoundCtx goal boundCtx
  -- The previously declared candidates have access to the entire bound context
  let builtinCandidates = fmap (`WithContext` boundCtx) rawBuiltinCandidates
  let allCandidates = builtinCandidates <> candidatesInBoundCtx

  logDebug MaxDetail $
    line
      <> "Builtin candidates:"
      <> line
      <> indent 2 (prettyMultiLineList (fmap prettyCandidate builtinCandidates))
      <> line
      <> "Context candidates:"
      <> line
      <> indent 2 (prettyMultiLineList (fmap prettyCandidate candidatesInBoundCtx))
      <> line
      <> "Depth:" <+> pretty depth
      <> line

  -- Try all candidates
  (unsuccessfulCandidates, successfulCandidates) <-
    partitionEithers <$> traverse (checkCandidate constraint goal depth) allCandidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [(candidate, typeCheckerState)] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyCandidate candidate)
      adoptHypotheticalState typeCheckerState

    -- If there are no valid candidates then we fail.
    [] -> do
      freeCtx <- getFreeCtx (Proxy @builtin)
      finalConstraint <- substMetaVariables constraint
      throwError $ TypingError $ FailedInstanceConstraint $ FailedInstanceConstraintError freeCtx finalConstraint unsuccessfulCandidates

    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      logDebug MaxDetail "Multiple possible candidates found so deferring."
      -- TODO can we be more precise with the set of blocking metas?
      -- Probably not as the set of blocking metas will depend on the depth at which we're searching

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
  InstanceSearchDepth ->
  WithContext (InstanceCandidate builtin) ->
  m (Either (WithContext (InstanceCandidate builtin), UnAnnDoc) (WithContext (InstanceCandidate builtin), TypeCheckerState builtin))
checkCandidate constraint goal depth candidate = do
  let candidateDoc = squotes (prettyCandidate candidate)
  logCompilerSection2 MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerTHypothetically $ do
      logCompilerSection MaxDetail "hypothetically accepting candidate" $
        acceptCandidate constraint goal candidate

      -- Run the solvers to check for conflicts
      let proxy = Proxy @builtin
      runUnificationSolver proxy False
      if depth == 0
        then return mempty
        else runInstanceSolver proxy (depth - 1)
    case result of
      Left err -> do
        let vehicleError = formatCompileError err
        logDebug MaxDetail $ line <> "Rejecting" <+> candidateDoc <+> "as a possibility"
        logDebug MaxDetail $ indent 2 (pretty vehicleError) <> line
        return $ Left (candidate, problem vehicleError)
      Right (_, state) -> do
        logDebug MaxDetail $ "Keeping" <+> candidateDoc <+> "as a possibility" <> line
        return $ Right (candidate, state)

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
  let newConstraintCtx = setConstraintBoundCtx constraintCtx extendedGoalCtx
  let extendedGoalInfo = (newConstraintCtx, instanceOrigin)

  -- Instantiate the candidate telescope with metas and subst into body.
  (substCandidateExpr, substCandidateSolution) <-
    instantiateCandidateTelescope goalCtxExtension (constraintCtx, instanceOrigin) candidate

  -- Unify the goal and candidate bodies
  goalConstraint <- createInstanceUnification extendedGoalInfo (goalExpr goal) substCandidateExpr

  instantiateInstanceConstraintSolution (WithContext Resolve {..} newConstraintCtx) substCandidateSolution

  -- Add the constriants
  addUnificationConstraints [goalConstraint]

-- | Generate meta variables for each binder in the telescope of the candidate
-- and then substitute them into the candidate expression.
instantiateCandidateTelescope ::
  forall builtin m.
  (MonadInstance builtin m) =>
  BoundCtx (Type builtin) ->
  InstanceConstraintInfo builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Value builtin, Expr builtin)
instantiateCandidateTelescope goalCtxExtension (constraintCtx, constraintOrigin) candidate = do
  let WithContext InstanceCandidate {..} candidateCtx = candidate
  logCompilerSection MaxDetail "instantiating candidate telescope" $ do
    let initialCtx = goalCtxExtension ++ candidateCtx
    let createInstance relevance typ = do
          let newInfo = (setConstraintBoundCtx constraintCtx initialCtx, constraintOrigin)
          -- WARNING massive hack should be traversing the normalised type here.
          normBinderType <- eval (toNamedBoundCtx initialCtx) (boundContextToEnv initialCtx) typ
          (expr, constraint) <- createDerivedInstanceConstraint newInfo relevance normBinderType
          addInstanceConstraints [constraint]
          return expr

    (candidateBody, candidateSol, _args) <- instantiateTelescope InstanceTelescope createInstance initialCtx (candidateExpr, candidateSolution)
    normCandidateBody <- eval (toNamedBoundCtx initialCtx) (boundContextToEnv initialCtx) candidateBody
    return (normCandidateBody, candidateSol)

-- TODO move this to Print
prettyCandidate :: (PrintableBuiltin builtin) => WithContext (InstanceCandidate builtin) -> Doc a
prettyCandidate (WithContext candidate ctx) =
  prettyExternal (WithContext (candidateExpr candidate) (toNamedBoundCtx ctx))
