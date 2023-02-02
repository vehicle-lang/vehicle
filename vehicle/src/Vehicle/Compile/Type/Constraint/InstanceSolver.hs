module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( runInstanceSolver,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes)
import Prettyprinter (list)
import Vehicle.Compile.Error (CompileError (..), MonadCompile)
import Vehicle.Compile.Error.Message (MeaningfulError (..))
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.InstanceBuiltins
import Vehicle.Compile.Type.Constraint.TypeClassSolver (solveTypeClassConstraint)
import Vehicle.Compile.Type.Constraint.UnificationSolver (runUnificationSolver)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Expr.DeBruijn (DBLevel (..), dbLevelToIndex, substDBInto)
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Public interface

-- | Attempts to solve as many type-class constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runInstanceSolver :: TCM m => MetaSet -> m ()
runInstanceSolver metasSolved =
  logCompilerPass MaxDetail ("instance solver run" <> line) $
    runConstraintSolver
      getActiveTypeClassConstraints
      setTypeClassConstraints
      solveInstanceConstraint
      metasSolved

solveInstanceConstraint :: TCM m => WithContext TypeClassConstraint -> m ()
solveInstanceConstraint (WithContext constraint ctx) = do
  normConstraint@(Has _ tc _) <- substMetas constraint
  let nConstraint = WithContext normConstraint ctx
  solve tc nConstraint

solve :: TypeClass -> forall m. TCM m => WithContext TypeClassConstraint -> m ()
solve tc = case HashMap.lookup tc declaredCandidates of
  Just candidates -> solveInstanceGoal candidates
  _ -> solveTypeClassConstraint

--------------------------------------------------------------------------------
-- Algorithm

-- The algorithm for this is taken from
-- https://agda.readthedocs.io/en/v2.6.2.2/language/instance-arguments.html#instance-resolution

solveInstanceGoal :: TCM m => [InstanceCandidate] -> WithContext TypeClassConstraint -> m ()
solveInstanceGoal builtinCandidates (WithContext tcConstraint@(Has meta tc spine) ctx) = do
  -- Goal telescopes aren't yet implemented
  let goalTelescope = reverse []
  let goal = InstanceGoal goalTelescope tc spine

  -- Extend the current context by the bound variables in the telescope of the goal.
  let newCtx = extendConstraintBoundCtx (copyContext ctx) goalTelescope

  -- Compute the final list of candidates
  candidatesInBoundCtx <- findCandidatesInBoundCtx goal (boundContext newCtx)
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
    [] -> throwError $ FailedInstanceConstraint ctx goal
    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      let constraint = WithContext (TypeClassConstraint tcConstraint) ctx
      -- TODO can we be more precise with the set of blocking metas?
      blockedConstraint <- blockConstraintOn constraint <$> getUnsolvedMetas
      addConstraints [blockedConstraint]

-- | Locates any more candidates that are in the bound context of the constraint
findCandidatesInBoundCtx :: MonadCompile m => InstanceGoal -> TypingBoundCtx -> m [InstanceCandidate]
findCandidatesInBoundCtx goal ctx = go ctx
  where
    go :: MonadCompile m => TypingBoundCtx -> m [InstanceCandidate]
    go = \case
      [] -> return []
      ((_, t, _) : localCtx) -> do
        candidates <- go localCtx
        case findTypeClassOfCandidate t of
          Right tc | tc == goalHead goal -> do
            let candidate =
                  InstanceCandidate
                    { candidateContext = localCtx,
                      candidateExpr = t,
                      candidateSolution = BoundVar mempty (dbLevelToIndex (DBLevel $ length ctx) (DBLevel $ length localCtx))
                    }
            return $ candidate : candidates
          _ -> return candidates

-- | Checks whether a candidate is a possibility for the instance goal.
-- Returns `Nothing` if it is definitely not a valid candidate and
-- `Just` if it might be a valid candidate.
checkCandidate ::
  TCM m =>
  ConstraintContext ->
  MetaID ->
  InstanceGoal ->
  InstanceCandidate ->
  m (Maybe (InstanceCandidate, TypeCheckerState))
checkCandidate ctx meta goal candidate = do
  let candidateDoc = squotes (prettyCandidate candidate)
  logCompilerPass MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerHypothetically $ do
      -- Instantiate the candidate telescope with metas and subst into body.
      (substCandidateExpr, substCandidateSolution) <- instantiateCandidateTelescope ctx candidate

      logCompilerSection MaxDetail "hypothetically accepting candidate" $ do
        -- Unify the goal and candidate bodies
        let bodiesEqual = Unify (goalExpr goal) substCandidateExpr
        let unificationConstraint = WithContext bodiesEqual ctx
        addUnificationConstraints [unificationConstraint]

        -- Add the solution of the type-class as well (if we had first class records
        -- then we wouldn't need to do this manually).
        solveMeta meta substCandidateSolution (boundContext ctx)

      runUnificationSolver mempty

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
instantiateCandidateTelescope :: TCM m => ConstraintContext -> InstanceCandidate -> m (BasicNormExpr, CheckedExpr)
instantiateCandidateTelescope ctx InstanceCandidate {..} =
  logCompilerSection MaxDetail "instantiating candidate telescope" $ do
    let p = provenanceOf ctx
    let origin = getConstraintOrigin ctx
    (candidateBody, candidateSol) <- go p candidateContext origin (candidateExpr, candidateSolution)

    declCtx <- getDeclSubstitution
    metaCtx <- getMetaSubstitution
    let currentEnv = mkNoOpEnv (DBLevel $ length candidateContext)
    normCandidateBody <- runReaderT (eval currentEnv candidateBody) (declCtx, metaCtx)
    return (normCandidateBody, candidateSol)
  where
    go :: TCM m => Provenance -> TypingBoundCtx -> (CheckedExpr, [CheckedArg]) -> (CheckedType, CheckedExpr) -> m (CheckedType, CheckedExpr)
    go p boundCtx origin = \case
      (Pi _ exprBinder exprBody, Pi _ _solutionBinder solutionBody) -> do
        newArg <- argExpr <$> instantiateArgForNonExplicitBinder boundCtx p origin exprBinder
        let exprBodyResult = unnormalised newArg `substDBInto` exprBody
        let solutionBodyResult = unnormalised newArg `substDBInto` solutionBody
        go p boundCtx origin (exprBodyResult, solutionBodyResult)
      body -> return body

prettyCandidate :: InstanceCandidate -> Doc a
prettyCandidate candidate = prettyExternal (WithContext (candidateExpr candidate) (boundContextOf (candidateContext candidate)))

getConstraintOrigin :: ConstraintContext -> (CheckedExpr, [CheckedArg])
getConstraintOrigin ctx = case origin ctx of
  CheckingTypeClass fun args -> (fun, args)
  _ -> developerError "The origin of an instance constraint should be an instance argument"
