{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Type.Constraint.UnificationSolver
  ( runUnificationSolver,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Prettyprinter (sep)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote (Quote (..), unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Builtin (TypableBuiltin (..))
import Vehicle.Compile.Type.Constraint.Core (runConstraintSolver)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Force (forceHead)
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (null, singleton)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn

--------------------------------------------------------------------------------
-- Unification solver

-- See https://github.com/AndrasKovacs/elaboration-zoo/
-- for an excellent tutorial on the algorithm.

-- | Attempts to solve as many unification constraints as possible. Takes in
-- the set of meta-variables solved since unification was last run and outputs
-- the set of meta-variables solved during this run.
runUnificationSolver :: (MonadTypeChecker builtin m) => Proxy builtin -> m ()
runUnificationSolver proxy =
  logCompilerPass MaxDetail "unification solver run" $
    runConstraintSolver
      proxy
      getActiveUnificationConstraints
      setUnificationConstraints
      solveUnificationConstraint

--------------------------------------------------------------------------------
-- Unification algorithm

type MonadUnify builtin m = MonadTypeChecker builtin m

solveUnificationConstraint ::
  forall builtin m.
  (MonadUnify builtin m) =>
  WithContext (UnificationConstraint builtin) ->
  m ()
solveUnificationConstraint constraint = do
  result <- solve constraint
  case result of
    Success -> return ()
    Blocked blockedConstraints ->
      addUnificationConstraints blockedConstraints
    HardFailure failedConstraints -> do
      finalFailedConstraints <- traverse substMetas failedConstraints
      throwError $ TypingError $ FailedUnificationConstraints $ FailedUnificationConstraintsError finalFailedConstraints

solve ::
  forall builtin m.
  (MonadUnify builtin m) =>
  WithContext (UnificationConstraint builtin) ->
  m (UnificationResult builtin)
solve (WithContext (Unify origin e1 e2) ctx) = do
  -- Force the heads of both expressions
  let namedCtx = namedBoundCtxOf ctx
  (ne1, e1BlockingMetas) <- forceHead namedCtx e1
  (ne2, e2BlockingMetas) <- forceHead namedCtx e2

  -- Construct the new constraint information
  let blockingMetas = e1BlockingMetas <> e2BlockingMetas
  let updatedConstraint = WithContext (Unify origin ne1 ne2) ctx
  let constraintInfo = (updatedConstraint, blockingMetas)

  -- Perform the unification
  unification constraintInfo (ne1, ne2)

data UnificationResult builtin
  = Success
  | -- | Always an error
    HardFailure (NonEmpty (WithContext (UnificationConstraint builtin)))
  | -- | Only an error when further reduction will never occur.
    Blocked [WithContext (UnificationConstraint builtin)]

instance Semigroup (UnificationResult builtin) where
  HardFailure r1 <> HardFailure r2 = HardFailure (r1 <> r2)
  r1@HardFailure {} <> _ = r1
  _ <> r2@HardFailure {} = r2
  Blocked m1 <> Blocked m2 = Blocked (m1 <> m2)
  r1@Blocked {} <> _ = r1
  _ <> r2@Blocked {} = r2
  Success <> Success = Success

instance Monoid (UnificationResult builtin) where
  mempty = Success

type ConstraintInfo builtin = (WithContext (UnificationConstraint builtin), MetaSet)

ctxOf :: ConstraintInfo builtin -> ConstraintContext builtin
ctxOf (WithContext _ ctx, _) = ctx

-- | Create a new unification constraint, copying the context as appropriate.
subUnify ::
  (MonadTypeChecker builtin m) =>
  ConstraintInfo builtin ->
  (Value builtin, Value builtin) ->
  m (UnificationResult builtin)
subUnify (WithContext (Unify origin _ _) ctx, _) (e1, e2) =
  solve . WithContext (Unify origin e1 e2) =<< copyContext ctx

block ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  Maybe MetaSet ->
  m (UnificationResult builtin)
block (WithContext constraint ctx, originalBlockingMetas) maybeRefinedBlockingMetas = do
  let blockingMetas = fromMaybe originalBlockingMetas maybeRefinedBlockingMetas
  if MetaSet.null blockingMetas
    then return $ HardFailure [WithContext constraint ctx]
    else do
      newConstraint <- WithContext constraint <$> copyContext ctx
      let blockedConstraint = blockConstraintOn newConstraint blockingMetas
      return $ Blocked [blockedConstraint]

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x, y)

unification ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (Value builtin, Value builtin) ->
  m (UnificationResult builtin)
unification info@(constraint, _) = \case
  -----------------------
  -- Rigid-rigid cases --
  -----------------------
  VUniverse l1 :~: VUniverse l2
    | l1 == l2 -> solveTrivially
  VBoundVar v1 spine1 :~: VBoundVar v2 spine2
    | v1 == v2 -> solveSpine info spine1 spine2
  VFreeVar v1 spine1 :~: VFreeVar v2 spine2
    | v1 == v2 -> solveSpine info spine1 spine2
  VBuiltin b1 spine1 :~: VBuiltin b2 spine2
    | b1 == b2 -> solveSpine info spine1 spine2
    | isConstructor b1 && isConstructor b2 -> return $ HardFailure [constraint]
  VPi binder1 closure1 :~: VPi binder2 closure2
    | visibilityMatches binder1 binder2 -> solveClosure info (binder1, closure1) (binder2, closure2)
  VLam binder1 closure1 :~: VLam binder2 closure2 ->
    solveClosure info (binder1, closure1) (binder2, closure2)
  ---------------------
  -- Flex-flex cases --
  ---------------------
  VMeta meta1 spine1 :~: VMeta meta2 spine2
    | meta1 == meta2 -> solveSpine info spine1 spine2
    -- The longer spine normally means its in a deeper scope. This minor
    -- optimisation tries to solve the deeper meta first.
    | length spine1 < length spine2 -> solveFlexFlex info (meta2, spine2) (meta1, spine1)
    | otherwise -> solveFlexFlex info (meta1, spine1) (meta2, spine2)
  ----------------------
  -- Flex-rigid cases --
  ----------------------
  VMeta meta spine :~: e -> solveFlexRigid info (meta, spine) e
  e :~: VMeta meta spine -> solveFlexRigid info (meta, spine) e
  ------------------
  -- Blocked case --
  ------------------
  _ -> block info Nothing

solveTrivially :: (MonadUnify builtin m) => m (UnificationResult builtin)
solveTrivially = do
  logDebug MaxDetail "solved-trivially"
  return Success

solveArg ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (VArg builtin, VArg builtin) ->
  m (UnificationResult builtin)
solveArg info@(constraint, _) (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = return $ HardFailure [constraint]
  -- Don't unify instances, they should be uniquely determined by the type.
  | isInstance arg1 = return Success
  | otherwise = subUnify info (argExpr arg1, argExpr arg2)

solveSpine ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  Spine builtin ->
  Spine builtin ->
  m (UnificationResult builtin)
solveSpine info@(constraint, _) args1 args2
  | length args1 /= length args2 = return $ HardFailure [constraint]
  | otherwise = mconcat <$> traverse (solveArg info) (zip args1 args2)

solveClosure ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (VBinder builtin, Closure builtin) ->
  (VBinder builtin, Closure builtin) ->
  m (UnificationResult builtin)
solveClosure info@(constraint, _) (binder1, Closure env1 body1) (binder2, Closure env2 body2) = do
  -- Unify binder constraints
  binderConstraint <- subUnify info (typeOf binder1, typeOf binder2)

  -- Evaluate the normalised bodies of the lambdas
  let lv = contextDBLevel (contextOf constraint)
  nbody1 <- normaliseInEnv (extendEnvWithBound lv binder1 env1) body1
  nbody2 <- normaliseInEnv (extendEnvWithBound lv binder2 env2) body2

  -- Update the context.
  let updatedInfo = updateInfoUnderBinder info (binder1, binder2)

  -- Unify the two bodies
  bodyConstraint <- subUnify updatedInfo (nbody1, nbody2)

  -- Return the result
  return $ binderConstraint <> bodyConstraint

solveFlexFlex ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (MetaID, Spine builtin) ->
  (MetaID, Spine builtin) ->
  m (UnificationResult builtin)
solveFlexFlex info (meta1, spine1) (meta2, spine2) = do
  -- It may be that only one of the two spines is invertible
  maybeRenaming <- invert (contextDBLevel (ctxOf info)) (meta1, spine1)
  case maybeRenaming of
    Nothing -> solveFlexRigid info (meta2, spine2) (VMeta meta1 spine1)
    Just renaming -> solveFlexRigidWithRenaming (ctxOf info) (meta1, spine1) renaming (VMeta meta2 spine2)

solveFlexRigid ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (MetaID, Spine builtin) ->
  Value builtin ->
  m (UnificationResult builtin)
solveFlexRigid info (metaID, spine) solution = do
  -- Check that 'spine' is a pattern and try to calculate a substitution
  -- that renames the variables in `solution` to ones available to `meta`
  maybeRenaming <- invert (contextDBLevel (ctxOf info)) (metaID, spine)
  case maybeRenaming of
    Just renaming -> solveFlexRigidWithRenaming (ctxOf info) (metaID, spine) renaming solution
    -- This constraint is stuck because it is not pattern; shelve
    -- it for now and hope that another constraint allows us to
    -- progress.
    Nothing -> block info (Just (MetaSet.singleton metaID))

solveFlexRigidWithRenaming ::
  forall builtin m.
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  (MetaID, Spine builtin) ->
  Renaming ->
  Value builtin ->
  m (UnificationResult builtin)
solveFlexRigidWithRenaming ctx meta@(metaID, _) renaming solution = do
  prunedSolution <-
    if useDependentMetas (Proxy @builtin)
      then pruneMetaDependencies ctx meta solution
      else return solution

  let unnormSolution = quote mempty (contextDBLevel ctx) prunedSolution
  let substSolution = substDBAll 0 (\v -> unIx v `IntMap.lookup` renaming) unnormSolution
  solveMeta metaID substSolution (boundContext ctx)
  return Success

pruneMetaDependencies ::
  forall builtin m.
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  (MetaID, Spine builtin) ->
  Value builtin ->
  m (Value builtin)
pruneMetaDependencies ctx (solvingMetaID, solvingMetaSpine) attemptedSolution = do
  go attemptedSolution
  where
    go ::
      (MonadUnify builtin m) =>
      Value builtin ->
      m (Value builtin)
    go expr = case expr of
      VMeta m spine
        | m == solvingMetaID ->
            -- If `i` is inside the term we're trying to unify it with then error.
            -- Unsure if this should be a user or a developer error.
            compilerDeveloperError $
              "Meta variable"
                <+> pretty m
                <+> "found in own solution"
                <+> squotes (prettyVerbose attemptedSolution)
        | otherwise -> do
            metaSubst <- getMetaSubstitution (Proxy @builtin)
            case MetaMap.lookup m metaSubst of
              Just solution -> go =<< normaliseApp (normalised solution) spine
              Nothing -> do
                let (deps, _) = getNormMetaDependencies solvingMetaSpine
                let (jDeps, _) = getNormMetaDependencies spine
                let sharedDependencies = deps `intersect` jDeps
                if sharedDependencies /= jDeps
                  then createMetaWithRestrictedDependencies ctx m sharedDependencies
                  else return $ VMeta m spine
      VUniverse {} -> return expr
      VBuiltin b spine -> VBuiltin b <$> traverse (traverse go) spine
      VBoundVar v spine -> VBoundVar v <$> traverse (traverse go) spine
      VFreeVar v spine -> VFreeVar v <$> traverse (traverse go) spine
      -- Definitely going to have come back and fix this one later.
      -- Can't inspect the metas in the environment, as not every variable
      -- in the environment will be used? But maybe we can?
      VPi {} -> return expr -- VPi <$> traverse go binder <*> go result
      VLam {} -> return expr

createMetaWithRestrictedDependencies ::
  forall builtin m.
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  MetaID ->
  [Lv] ->
  m (Value builtin)
createMetaWithRestrictedDependencies ctx meta newDependencies = do
  p <- getMetaProvenance (Proxy @builtin) meta
  metaType <- getMetaType meta

  let constraintLevel = contextDBLevel ctx
  let dbIndices = fmap (dbLevelToIndex constraintLevel) newDependencies
  let boundCtx = boundContextOf ctx
  let newDeps = fmap (\v -> prettyFriendly (WithContext (BoundVar p v :: Expr builtin) (toNamedBoundCtx boundCtx))) dbIndices

  logCompilerSection MaxDetail ("restricting dependencies of" <+> pretty meta <+> "to" <+> sep newDeps) $ do
    let levelSet = IntSet.fromList $ fmap unLv newDependencies
    let makeElem (i, v) = if i `IntSet.member` levelSet then Just v else Nothing
    let ctxWithLevels = zip (reverse [0 .. length boundCtx - 1 :: Int]) boundCtx
    let restrictedContext = mapMaybe makeElem ctxWithLevels
    newMetaExpr <- freshMetaExpr p metaType restrictedContext

    let substitution = IntMap.fromAscList (zip [0 ..] (reverse dbIndices))
    let substMetaExpr = substDBAll 0 (\v -> unIx v `IntMap.lookup` substitution) (unnormalised newMetaExpr)
    solveMeta meta substMetaExpr (boundContext ctx)

    return $ normalised newMetaExpr

updateInfoUnderBinder ::
  ConstraintInfo builtin ->
  (VBinder builtin, VBinder builtin) ->
  ConstraintInfo builtin
updateInfoUnderBinder (WithContext constraint ctx, blockingMeta) (binder1, _binder2) = do
  -- Update the context.
  -- NOTE: that we have to unnormalise here indicates something is wrong.
  let unnormBinder = fmap (unnormalise (contextDBLevel ctx)) binder1
  let newCtx = updateConstraintBoundCtx ctx (unnormBinder :)
  (WithContext constraint newCtx, blockingMeta)

--------------------------------------------------------------------------------
-- Argument patterns

type Renaming = IntMap Ix

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
invert :: forall builtin m. (MonadUnify builtin m) => Lv -> (MetaID, Spine builtin) -> m (Maybe Renaming)
invert ctxSize (metaID, spine) = do
  metaCtxSize <- length <$> getMetaCtx (Proxy @builtin) metaID
  return $
    if metaCtxSize < length spine
      then Nothing
      else go (metaCtxSize - 1) IntMap.empty spine
  where
    go :: Int -> IntMap Ix -> Spine builtin -> Maybe Renaming
    go i revMap = \case
      [] -> Just revMap
      (ExplicitArg _ _ (VBoundVar j []) : restArgs) -> do
        -- TODO: we could eta-reduce arguments too, if possible
        let jIndex = dbLevelToIndex ctxSize j
        if IntMap.member (unIx jIndex) revMap
          then -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning;
          -- but then we should make sure the solution is well-typed
            Nothing
          else go (i - 1) (IntMap.insert (unIx jIndex) (Ix i) revMap) restArgs
      -- Not a pattern so return nothing.
      _ -> Nothing
