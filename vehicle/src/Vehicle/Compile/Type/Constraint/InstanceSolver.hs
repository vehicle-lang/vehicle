module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( resolveInstance,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Prettyprinter (list)
import Vehicle.Compile.Error (CompileError (..), MonadCompile)
import Vehicle.Compile.Error.Message (MeaningfulError (..))
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin, prettyExternal, prettyFriendly)
import Vehicle.Compile.Type (runUnificationSolver)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.DeBruijn (Ix, Lv (..), dbLevelToIndex, substDBInto)
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Public interface

resolveInstance ::
  forall builtin m.
  (Hashable builtin, MonadInstance builtin m) =>
  InstanceCandidateDatabase builtin ->
  WithContext (InstanceConstraint builtin) ->
  m ()
resolveInstance allCandidates (WithContext constraint ctx) = do
  normConstraint@(Has meta relevance expr) <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly (WithContext normConstraint ctx)

  goal <- parseInstanceGoal expr
  let candidates = fromMaybe [] $ Map.lookup (goalHead goal) allCandidates
  solveInstanceGoal candidates ctx meta relevance goal

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
  MetaID ->
  Relevance ->
  InstanceGoal builtin ->
  m ()
solveInstanceGoal rawBuiltinCandidates ctx meta relevance goal@InstanceGoal {..} = do
  let boundCtx = boundContext ctx

  -- Extend the current context by the bound variables in the telescope of the goal.
  let newCtx = extendConstraintBoundCtx ctx goalTelescope

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
  successfulCandidates <- catMaybes <$> traverse (checkCandidate newCtx meta goal) allCandidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [(candidate, typeCheckerState)] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyCandidate candidate)
      adoptHypotheticalState typeCheckerState

    -- If there are no valid candidates then we fail.
    [] -> do
      substCtx <- substMetas ctx
      throwError $ TypingError $ FailedInstanceConstraint substCtx goal allCandidates

    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> do
      logDebug MaxDetail "Multiple possible candidates found so deferring."
      let constraint = WithContext (InstanceConstraint (Has meta relevance (goalExpr goal))) ctx
      -- TODO can we be more precise with the set of blocking metas?
      blockedConstraint <- blockConstraintOn constraint <$> getUnsolvedMetas (Proxy @builtin)
      addConstraints [blockedConstraint]

-- | Locates any more candidates that are in the bound context of the constraint
findCandidatesInBoundCtx ::
  forall builtin m.
  (MonadInstance builtin m) =>
  InstanceGoal builtin ->
  TypingBoundCtx builtin ->
  m [WithContext (InstanceCandidate builtin)]
findCandidatesInBoundCtx goal ctx = go ctx
  where
    go :: (MonadCompile m) => TypingBoundCtx builtin -> m [WithContext (InstanceCandidate builtin)]
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
  ConstraintContext builtin ->
  MetaID ->
  InstanceGoal builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Maybe (WithContext (InstanceCandidate builtin), TypeCheckerState builtin))
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
  (MonadInstance builtin m) =>
  ConstraintContext builtin ->
  WithContext (InstanceCandidate builtin) ->
  m (Value builtin, Expr Ix builtin)
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
      (MonadInstance builtin m) =>
      Provenance ->
      TypingBoundCtx builtin ->
      (Expr Ix builtin, [Arg Ix builtin]) ->
      (Type Ix builtin, Expr Ix builtin) ->
      m (Type Ix builtin, Expr Ix builtin)
    go p boundCtx origin = \case
      (Pi _ exprBinder exprBody, Lam _ _solutionBinder solutionBody) -> do
        newArg <- argExpr <$> instantiateArgForNonExplicitBinder boundCtx p origin exprBinder
        let exprBodyResult = unnormalised newArg `substDBInto` exprBody
        let solutionBodyResult = unnormalised newArg `substDBInto` solutionBody
        go p boundCtx origin (exprBodyResult, solutionBodyResult)
      body -> return body

-- TODO move this to Print
prettyCandidate :: (PrintableBuiltin builtin) => WithContext (InstanceCandidate builtin) -> Doc a
prettyCandidate (WithContext candidate ctx) =
  prettyExternal (WithContext (candidateExpr candidate) (boundContextOf ctx))

getConstraintOrigin :: ConstraintContext builtin -> (Expr Ix builtin, [Arg Ix builtin])
getConstraintOrigin ctx = case origin ctx of
  CheckingTypeClass fun args _ -> (fun, args)
  _ -> developerError "The origin of an instance constraint should be an instance argument"

goalExpr :: InstanceGoal builtin -> Value builtin
goalExpr InstanceGoal {..} = VBuiltin goalHead goalSpine
