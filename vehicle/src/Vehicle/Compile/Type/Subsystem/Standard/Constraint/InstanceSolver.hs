module Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceSolver
  ( solveInstanceConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Prettyprinter (list)
import Vehicle.Compile.Error (CompileError (..), MonadCompile)
import Vehicle.Compile.Error.Message (MeaningfulError (..))
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly)
import Vehicle.Compile.Type (runUnificationSolver)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceBuiltins
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassSolver (solveTypeClassConstraint)
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DeBruijn (Lv (..), dbLevelToIndex, substDBInto)
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Public interface

solveInstanceConstraint :: (MonadInstance m) => WithContext StandardTypeClassConstraint -> m ()
solveInstanceConstraint (WithContext constraint ctx) = do
  normConstraint@(Has _ b _) <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly (WithContext normConstraint ctx)

  tc <- getTypeClass b
  let nConstraint = WithContext normConstraint ctx
  solve tc nConstraint

--------------------------------------------------------------------------------
-- Algorithm

solve :: (MonadInstance m) => TypeClass -> WithContext StandardTypeClassConstraint -> m ()
solve tc = case HashMap.lookup tc builtinInstances of
  Just builtinCandidates -> solveInstanceGoal builtinCandidates
  _ -> solveTypeClassConstraint

-- The algorithm for this is taken from
-- https://agda.readthedocs.io/en/v2.6.2.2/language/instance-arguments.html#instance-resolution

solveInstanceGoal ::
  (MonadInstance m) =>
  [Provenance -> InstanceCandidate] ->
  WithContext StandardTypeClassConstraint ->
  m ()
solveInstanceGoal rawBuiltinCandidates (WithContext tcConstraint@(Has meta b spine) ctx) = do
  tc <- getTypeClass b
  let p = provenanceOf ctx
  let boundCtx = boundContext ctx

  -- Goal telescopes aren't yet implemented
  let goalTelescope = reverse []
  let goal = InstanceGoal goalTelescope tc spine

  -- Extend the current context by the bound variables in the telescope of the goal.
  let newCtx = extendConstraintBoundCtx ctx goalTelescope

  -- The builtin candidates have access to the entire bound context
  let builtinCandidates = fmap (\candidate -> WithContext (candidate p) boundCtx) rawBuiltinCandidates
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
  successfulCandidates <- catMaybes <$> traverse (checkCandidate newCtx meta goal) allCandidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [(candidate, typeCheckerState)] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyCandidate candidate)
      adoptHypotheticalState typeCheckerState

    -- If there are no valid candidates then we fail.
    [] -> do
      substCtx <- substMetas ctx
      throwError $ FailedInstanceConstraint substCtx goal allCandidates

    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      logDebug MaxDetail "Multiple possible candidates found so deferring."
      let constraint = WithContext (TypeClassConstraint tcConstraint) ctx
      -- TODO can we be more precise with the set of blocking metas?
      blockedConstraint <- blockConstraintOn constraint <$> getUnsolvedMetas (Proxy @StandardBuiltinType)
      addConstraints [blockedConstraint]

-- | Locates any more candidates that are in the bound context of the constraint
findCandidatesInBoundCtx ::
  (MonadCompile m) =>
  InstanceGoal ->
  StandardTypingBoundCtx ->
  m [WithContext InstanceCandidate]
findCandidatesInBoundCtx goal ctx = go ctx
  where
    go :: (MonadCompile m) => StandardTypingBoundCtx -> m [WithContext InstanceCandidate]
    go = \case
      [] -> return []
      ((_, t) : localCtx) -> do
        candidates <- go localCtx
        case findTypeClassOfCandidate t of
          Right tc | tc == goalHead goal -> do
            let candidate =
                  InstanceCandidate
                    { candidateExpr = t,
                      candidateSolution = BoundVar mempty (dbLevelToIndex (Lv $ length ctx) (Lv $ length localCtx))
                    }
            return $ WithContext candidate localCtx : candidates
          _ -> return candidates

-- | Checks whether a candidate is a possibility for the instance goal.
-- Returns `Nothing` if it is definitely not a valid candidate and
-- `Just` if it might be a valid candidate.
checkCandidate ::
  (MonadInstance m) =>
  StandardConstraintContext ->
  MetaID ->
  InstanceGoal ->
  WithContext InstanceCandidate ->
  m (Maybe (WithContext InstanceCandidate, TypeCheckerState StandardBuiltinType))
checkCandidate ctx meta goal candidate = do
  let candidateDoc = squotes (prettyCandidate candidate)
  logCompilerPass MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerHypothetically $ do
      -- Instantiate the candidate telescope with metas and subst into body.
      (substCandidateExpr, substCandidateSolution) <-
        instantiateCandidateTelescope ctx candidate

      logCompilerSection MaxDetail "hypothetically accepting candidate" $ do
        -- Unify the goal and candidate bodies
        let bodiesEqual = Unify (goalExpr goal) substCandidateExpr
        newCtx <- copyContext ctx
        let unificationConstraint = WithContext bodiesEqual newCtx
        addUnificationConstraints [unificationConstraint]

        -- Add the solution of the type-class as well (if we had first class records
        -- then we wouldn't need to do this manually).
        solveMeta meta substCandidateSolution (boundContext newCtx)

      runUnificationSolver (Proxy @StandardBuiltinType) mempty

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
  (MonadInstance m) =>
  StandardConstraintContext ->
  WithContext InstanceCandidate ->
  m (StandardNormExpr, StandardExpr)
instantiateCandidateTelescope ctx (WithContext InstanceCandidate {..} candidateCtx) =
  logCompilerSection MaxDetail "instantiating candidate telescope" $ do
    let p = provenanceOf ctx
    let origin = getConstraintOrigin ctx
    (candidateBody, candidateSol) <- go p candidateCtx origin (candidateExpr, candidateSolution)
    let currentEnv = typingBoundContextToEnv candidateCtx
    normCandidateBody <- eval currentEnv candidateBody
    return (normCandidateBody, candidateSol)
  where
    go ::
      (MonadInstance m) =>
      Provenance ->
      StandardTypingBoundCtx ->
      (StandardExpr, [StandardArg]) ->
      (StandardType, StandardExpr) ->
      m
        (StandardType, StandardExpr)
    go p boundCtx origin = \case
      (Pi _ exprBinder exprBody, Lam _ _solutionBinder solutionBody) -> do
        newArg <- argExpr <$> instantiateArgForNonExplicitBinder boundCtx p origin exprBinder
        let exprBodyResult = unnormalised newArg `substDBInto` exprBody
        let solutionBodyResult = unnormalised newArg `substDBInto` solutionBody
        go p boundCtx origin (exprBodyResult, solutionBodyResult)
      body -> return body

prettyCandidate :: WithContext InstanceCandidate -> Doc a
prettyCandidate (WithContext candidate ctx) =
  prettyExternal (WithContext (candidateExpr candidate) (boundContextOf ctx))

getConstraintOrigin :: StandardConstraintContext -> (StandardExpr, [StandardArg])
getConstraintOrigin ctx = case origin ctx of
  CheckingTypeClass fun args _ _ -> (fun, args)
  _ -> developerError "The origin of an instance constraint should be an instance argument"
