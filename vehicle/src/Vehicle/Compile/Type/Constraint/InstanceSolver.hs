module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( runInstanceSolver,
  )
where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Maybe (catMaybes)
import Vehicle.Compile.Error (CompileError (..))
import Vehicle.Compile.Error.Message (MeaningfulError (..))
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint (ConstraintContext, InstanceCandidate (..), InstanceGoal (..), TypeClassConstraint (..), UnificationConstraint (..), boundContext, contextDBLevel, copyContext, extendConstraintBoundCtx)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.TypeClassSolver (solveTypeClassConstraint)
import Vehicle.Compile.Type.Constraint.UnificationSolver (runUnificationSolver)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx, mkTypingBoundCtxEntry)
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
  normConstraint@(Has m tc spine) <- substMetas constraint
  let nConstraint = WithContext normConstraint ctx
  logDebug MaxDetail $ prettyVerbose nConstraint
  solve tc m nConstraint spine

type Solver =
  forall m.
  TCM m =>
  MetaID ->
  WithContext TypeClassConstraint ->
  Spine ->
  m ()

castInstanceFn :: InstanceSolver -> Solver
castInstanceFn f m c = f (contextOf c) m

solve :: TypeClass -> Solver
solve = \case
  HasMap -> castInstanceFn solveHasMapInstance
  _ -> \_ (WithContext constraint ctx) _ -> solveTypeClassConstraint ctx constraint

--------------------------------------------------------------------------------
-- Algorithm

-- The algorithm for this is taken from
-- https://agda.readthedocs.io/en/v2.6.2.2/language/instance-arguments.html#instance-resolution

solveInstanceGoal :: TCM m => ConstraintContext -> MetaID -> InstanceGoal -> [InstanceCandidate] -> m ()
solveInstanceGoal ctx meta goal candidates = do
  successfulCandidates <- catMaybes <$> traverse (checkCandidate ctx meta goal) candidates

  case successfulCandidates of
    -- If there is a single valid candidate then we adopt the resulting state
    [(candidate, typeCheckerState)] -> do
      logDebug MaxDetail $ "Accepting only remaining candidate:" <+> squotes (prettyVerbose $ candidateExpr candidate)
      adoptHypotheticalState typeCheckerState

    -- If there are no valid candidates then we fail.
    [] -> throwError $ FailedInstanceConstraint ctx goal
    -- Otherwise there are still multiple valid candidates so we're forced to block.
    _ -> return ()

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
checkCandidate ctx meta InstanceGoal {..} candidate@InstanceCandidate {..} = do
  let candidateDoc = squotes (prettyVerbose candidateExpr)
  logCompilerPass MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
    result <- runTypeCheckerHypothetically $ do
      -- Extend the current context by the bound variables in the telescope of the goal.
      let newCtx = extendConstraintBoundCtx (copyContext ctx) goalTelescope

      -- Instantiate the candidate telescope with metas and subst into body.
      substCandidateExpr <- instantiateCandidateTelescopeInCandidateBody ctx candidate

      logCompilerSection MaxDetail "hypothetically accepting candidate" $ do
        -- Unify the goal and candidate bodies
        let bodiesEqual = Unify goalExpr substCandidateExpr
        addUnificationConstraints [WithContext bodiesEqual newCtx]

        -- Add the solution of the type-class as well (if we had first class records
        -- then we wouldn't need to do this manually).
        solveTypeClassMeta ctx meta (candidateSolution (provenanceOf ctx))

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
instantiateCandidateTelescopeInCandidateBody :: TCM m => ConstraintContext -> InstanceCandidate -> m NormExpr
instantiateCandidateTelescopeInCandidateBody ctx InstanceCandidate {..} =
  logCompilerSection MaxDetail "instantiating candidate telescope" $ do
    let p = provenanceOf ctx
    let currentBoundCtx = boundContext ctx
    let currentEnv = mkNoOpEnv (contextDBLevel ctx)
    (extendedEnv, _) <- foldM (extendByBinder p) (currentEnv, currentBoundCtx) candidateTelescope

    declCtx <- getDeclSubstitution
    metaCtx <- getMetaSubstitution
    runReaderT (eval extendedEnv candidateExpr) (declCtx, metaCtx)
  where
    extendByBinder :: TCM m => Provenance -> (Env, TypingBoundCtx) -> CheckedBinder -> m (Env, TypingBoundCtx)
    extendByBinder p (env, boundCtx) binder = do
      meta <- freshExprMeta p (binderType binder) (boundContext ctx)
      let extendedEnv = normalised meta : env
      let extendedCtx = mkTypingBoundCtxEntry binder : boundCtx
      return (extendedEnv, extendedCtx)

--------------------------------------------------------------------------------
-- Instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

solveHasMapInstance :: TCM m => ConstraintContext -> MetaID -> Spine -> m ()
solveHasMapInstance ctx meta spine = do
  let goal = InstanceGoal (reverse []) (VConstructor mempty (TypeClass HasMap) spine)
  solveInstanceGoal ctx meta goal hasMapCandidates

hasMapCandidates :: [InstanceCandidate]
hasMapCandidates =
  [ InstanceCandidate
      { candidateTelescope = reverse [],
        candidateExpr =
          BuiltinTypeClass
            p
            HasMap
            [ ExplicitArg p (Builtin p (Constructor List))
            ],
        candidateSolution = \p' -> VBuiltin p' (Map MapList) []
      },
    InstanceCandidate
      { candidateTelescope = reverse [Binder p (BinderDisplayForm (OnlyName "n") True) (Implicit False) Relevant () (NatType p)],
        candidateExpr =
          BuiltinTypeClass
            p
            HasMap
            [ ExplicitArg
                p
                ( Lam
                    p
                    (Binder p (BinderDisplayForm (OnlyName "A") True) Explicit Relevant () (TypeUniverse p 0))
                    (VectorType p (BoundVar p 0) (BoundVar p 1))
                )
            ],
        candidateSolution = \p' -> VBuiltin p' (Map MapVector) []
      }
  ]
  where
    p = mempty
