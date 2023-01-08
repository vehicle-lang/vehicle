module Vehicle.Compile.Type.Constraint.InstanceSolver
  ( runInstanceSolver,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (catMaybes)
import Vehicle.Compile.Error (CompileError (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint (ConstraintContext, InstanceCandidate (..), InstanceGoal (..), TypeClassConstraint (..), UnificationConstraint (..), copyContext)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.TypeClassSolver (solveTypeClassConstraint)
import Vehicle.Compile.Type.Constraint.UnificationSolver (runUnificationSolver)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Monad
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
      logDebug MaxDetail $ "accepting" <+> squotes (prettyVerbose $ candidateExpr candidate)
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

  result <- runTypeCheckerHypothetically $ do
    logCompilerPass MaxDetail ("trying candidate instance" <+> candidateDoc) $ do
      -- At the moment assumes the telescopes are empty.

      -- TODO extend the current context by the goal telescope.
      let newCtx = copyContext ctx

      -- TODO generate meta variables for each binder in goal telescope.
      -- and substitute through the meta-variables into the goal expr.

      -- Unify the goal and candidate bodies
      let bodiesEqual = Unify goalExpr candidateExpr
      addUnificationConstraints [WithContext bodiesEqual newCtx]

      -- Add the solution to
      solveTypeClassMeta ctx meta (candidateSolution (provenanceOf ctx))

      runUnificationSolver mempty
      logDebug MaxDetail $ candidateDoc <+> "is a possibility"

  case result of
    Left _err -> do
      logDebug MaxDetail $ candidateDoc <+> "was rejected"
      return Nothing
    Right (_, state) ->
      return $ Just (candidate, state)

--------------------------------------------------------------------------------
-- Instances

-- Manually declared here as we have no way of declaring them in the language
-- itself.

solveHasMapInstance :: TCM m => ConstraintContext -> MetaID -> Spine -> m ()
solveHasMapInstance ctx meta spine = do
  let goal = InstanceGoal [] (VConstructor mempty (TypeClass HasMap) spine)
  solveInstanceGoal ctx meta goal hasMapCandidates

hasMapCandidates :: [InstanceCandidate]
hasMapCandidates =
  [ InstanceCandidate
      { candidateTelescope = [],
        candidateExpr = VConstructor mempty (TypeClass HasMap) [ExplicitArg mempty (VConstructor mempty List [])],
        candidateSolution = \p -> VBuiltin p (Map MapList) []
      }
  ]
