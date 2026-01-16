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
  candidateState <- getCurrentCandidateState normConstraint
  solveInstanceGoal normConstraint candidateState depth goal

solveInstanceGoal ::
  forall builtin m.
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  InstanceCandidateState builtin ->
  InstanceSearchDepth ->
  InstanceGoal builtin ->
  m ()
solveInstanceGoal constraint (candidates, failedCandidates) depth goal = do
  logDebug MaxDetail $
    line
      <> "Candidates:"
      <> line
      <> indent 2 (prettyMultiLineList (fmap prettyExternal candidates))
      <> line

  -- Try all candidates
  (unsuccessfulCandidates, successfulCandidates) <-
    partitionEithers <$> traverse (checkCandidate constraint goal depth) candidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [SuccessfulInstanceCandidate {..}] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyExternal successfulCandidate)
      adoptHypotheticalState successfulState

    -- If there are no valid candidates then we fail.
    [] -> do
      freeCtx <- getFreeCtx (Proxy @builtin)
      finalConstraint <- substMetaVariables constraint
      throwError $
        TypingError $
          FailedInstanceConstraint $
            FailedInstanceConstraintError
              { _freeCtx = freeCtx,
                failedConstraint = finalConstraint,
                exploredCandidates = failedCandidates <> unsuccessfulCandidates
              }

    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      logDebug MaxDetail $
        "Multiple possible candidates:"
          <> lineIndent (vsep $ fmap (prettyExternal . successfulCandidate) successfulCandidates)

      -- Find most general candiate
      case findLeastGeneralCandidate successfulCandidates of
        Just SuccessfulInstanceCandidate {..} -> do
          logDebug MaxDetail $ "Accepting least general candidate:" <+> squotes (prettyExternal successfulCandidate)
          adoptHypotheticalState successfulState
        Nothing -> do
          -- Create the updated constraint
          let newPossibleCandidates = fmap successfulCandidate successfulCandidates
          let newFailedCandidates = failedCandidates <> unsuccessfulCandidates
          let newConstraint = flip mapObject constraint $ \c ->
                c
                  { instanceCandidateState = Just (newPossibleCandidates, newFailedCandidates)
                  }
          -- TODO can we be more precise with the set of blocking metas?
          -- Probably not as the set of blocking metas will depend on the depth at which we're searching
          blockedConstraint <- blockConstraintOn newConstraint <$> getUnsolvedMetas (Proxy @builtin)

          addInstanceConstraints [blockedConstraint]

getCurrentCandidateState ::
  forall builtin m.
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  m (InstanceCandidateState builtin)
getCurrentCandidateState constraint =
  case instanceCandidateState $ objectIn constraint of
    Nothing -> getInitialCandidateState constraint
    Just state -> return state

getInitialCandidateState ::
  forall builtin m.
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  m (InstanceCandidateState builtin)
getInitialCandidateState constraint = do
  let goal = instanceGoal $ objectIn constraint
  let boundCtx = boundContext $ contextOf constraint

  -- Candidates in free context
  rawBuiltinCandidates <- getInstanceCandidatesFromFreeCtx goal
  let builtinCandidates = fmap (`WithContext` boundCtx) rawBuiltinCandidates

  -- Candidates in bound context
  let candidatesInBoundCtx = getCandidatesInBoundCtx goal boundCtx

  logDebug MaxDetail $
    line
      <> "Builtin candidates:"
      <> line
      <> indent 2 (prettyMultiLineList (fmap prettyExternal builtinCandidates))
      <> line
      <> "Context candidates:"
      <> line
      <> indent 2 (prettyMultiLineList (fmap prettyExternal candidatesInBoundCtx))
      <> line

  let possibleCandidates = builtinCandidates <> candidatesInBoundCtx
  let failedCandidates = mempty
  return (possibleCandidates, failedCandidates)

-- | Locates any more candidates that are in the bound context of the constraint
getCandidatesInBoundCtx ::
  forall builtin.
  (Eq builtin) =>
  InstanceGoal builtin ->
  BoundCtx (Type builtin) ->
  [WithContext (InstanceCandidate builtin)]
getCandidatesInBoundCtx goal ctx = go ctx
  where
    go :: BoundCtx (Type builtin) -> [WithContext (InstanceCandidate builtin)]
    go = \case
      [] -> []
      (binder : localCtx) -> do
        let candidates = go localCtx
        let binderType = typeOf binder
        case findInstanceGoalHead binderType of
          Right binderHead | binderHead == goalHead goal -> do
            let candidate =
                  InstanceCandidate
                    { candidateExpr = binderType,
                      candidateSolution = BoundVar mempty (dbLevelToIndex (Lv $ length ctx) (Lv $ length localCtx)),
                      candidatePriority = Nothing
                    }
            WithContext candidate localCtx : candidates
          _ -> candidates

data SuccessfulInstanceCandidate builtin = SuccessfulInstanceCandidate
  { successfulCandidate :: WithContext (InstanceCandidate builtin),
    successfulState :: TypeCheckerState builtin,
    successfulSolution :: Value builtin
  }

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
  m (Either (FailedInstanceCandidate builtin) (SuccessfulInstanceCandidate builtin))
checkCandidate constraint goal depth candidate = do
  let candidateDoc = squotes (prettyExternal candidate)
  logCompilerSection2 MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerTHypothetically $ do
      instantiatedSolution <-
        logCompilerSection MaxDetail "hypothetically accepting candidate" $
          acceptCandidate constraint goal candidate

      -- Run the solvers to check for conflicts
      let proxy = Proxy @builtin
      runUnificationSolver proxy False
      if depth == 0
        then return mempty
        else runInstanceSolver proxy (depth - 1)
      return instantiatedSolution

    case result of
      Left err -> do
        let vehicleError = formatCompileError err
        logDebug MaxDetail $ line <> "Rejecting" <+> candidateDoc <+> "as a possibility"
        logDebug MaxDetail $ indent 2 (pretty vehicleError) <> line
        return $ Left (candidate, problem vehicleError)
      Right (instantiatedSolution, state) -> do
        logDebug MaxDetail $ "Keeping" <+> candidateDoc <+> "as a possibility" <> line
        return $ Right $ SuccessfulInstanceCandidate candidate state instantiatedSolution

acceptCandidate ::
  (MonadInstance builtin m) =>
  WithContext (InstanceConstraint builtin) ->
  InstanceGoal builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Value builtin)
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

  return substCandidateExpr

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

-- | Sees if one of the candidates is provably less general than all the
-- others, e.g.
--
--   HasAdd (Tensor Rat t)
--
-- is less than general than
--
--   {{TensorLike r}} -> HasAdd r
findLeastGeneralCandidate ::
  (Eq builtin) =>
  [SuccessfulInstanceCandidate builtin] ->
  Maybe (SuccessfulInstanceCandidate builtin)
findLeastGeneralCandidate = \case
  -- TODO this could be generalised to find a minimum in the whole graph
  -- but this is sufficient now.
  [c1, c2] -> case c1 `lessGeneralThan` c2 of
    Nothing -> Nothing
    -- This is a hack to stop type-classes with zero arguments
    -- from being declared equal (e.g. `IsTensorType` in decidability types)...
    Just EQ -> Nothing
    Just LT -> Just c1
    Just GT -> Just c2
  _ -> Nothing

lessGeneralThan ::
  forall builtin.
  (Eq builtin) =>
  SuccessfulInstanceCandidate builtin ->
  SuccessfulInstanceCandidate builtin ->
  Maybe Ordering
lessGeneralThan candidate1 candidate2 =
  go (successfulSolution candidate1) (successfulSolution candidate2)
  where
    go :: Value builtin -> Value builtin -> Maybe Ordering
    go v1 v2 = case (v1, v2) of
      (VMeta {}, VMeta {}) -> Just EQ
      (VMeta {}, _) -> Just GT
      (_, VMeta {}) -> Just LT
      (VBuiltin b1 args1, VBuiltin b2 args2)
        | b1 /= b2 -> Nothing
        | otherwise -> goArgs args1 args2
      (VFreeVar i1 args1, VFreeVar i2 args2)
        | i1 /= i2 -> Nothing
        | otherwise -> goArgs args1 args2
      -- TODO extend with remaining cases?
      _ -> Nothing

    goArgs :: Spine builtin -> Spine builtin -> Maybe Ordering
    goArgs args1 args2
      | length args1 /= length args2 = Nothing
      | otherwise = do
          let maybeResults = zipWith (\x y -> go (argExpr x) (argExpr y)) args1 args2
          let OrderingCounts {..} = countOrderings maybeResults
          if numberOfNothings > 0 || (numberOfLTs > 0 && numberOfGTs > 0)
            then Nothing
            else
              if numberOfEQs == length maybeResults
                then Just EQ
                else
                  if numberOfLTs > 0
                    then Just LT
                    else Just GT

data OrderingCounts = OrderingCounts
  { numberOfNothings :: Int,
    numberOfEQs :: Int,
    numberOfLTs :: Int,
    numberOfGTs :: Int
  }

countOrderings :: [Maybe Ordering] -> OrderingCounts
countOrderings = \case
  [] -> OrderingCounts 0 0 0 0
  o : os -> do
    let counts = countOrderings os
    case o of
      Nothing -> counts {numberOfNothings = numberOfNothings counts + 1}
      Just EQ -> counts {numberOfEQs = numberOfEQs counts + 1}
      Just LT -> counts {numberOfLTs = numberOfLTs counts + 1}
      Just GT -> counts {numberOfGTs = numberOfGTs counts + 1}
