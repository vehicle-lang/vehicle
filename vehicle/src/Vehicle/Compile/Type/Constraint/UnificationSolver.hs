{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Type.Constraint.UnificationSolver
  ( solveUnificationConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Prettyprinter (sep)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Monad (MonadNorm (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Force (forceHead)
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (null, singleton)
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Unification solver

-- See https://github.com/AndrasKovacs/elaboration-zoo/
-- for an excellent tutorial on the algorithm.

--------------------------------------------------------------------------------
-- Unification algorithm

type MonadUnify builtin m = TCM builtin m

solveUnificationConstraint ::
  (MonadUnify builtin m) =>
  WithContext (UnificationConstraint builtin) ->
  m ()
solveUnificationConstraint (WithContext (Unify e1 e2) ctx) = do
  (ne1', e1BlockingMetas) <- forceHead ctx e1
  (ne2', e2BlockingMetas) <- forceHead ctx e2

  -- In theory this substitution shouldn't be needed, but in practice it is as if
  -- not all the meta-variables are substituted through then the scope of some
  -- meta-variables may be larger than the current scope of the constraint.
  -- These dependencies only disappear on substitution. Need to work out how to
  -- avoid doing this.
  nu@(Unify ne1 ne2) <- substMetas (Unify ne1' ne2')

  result <- unification ctx (e1BlockingMetas <> e2BlockingMetas) (ne1, ne2)
  case result of
    Success newConstraints -> do
      addUnificationConstraints newConstraints
    SoftFailure blockingMetas
      | not (MetaSet.null blockingMetas) -> do
          let normConstraint = WithContext nu ctx
          let blockedConstraint = blockConstraintOn normConstraint blockingMetas
          addUnificationConstraints [blockedConstraint]
    _ -> throwError $ TypingError $ FailedUnificationConstraints [WithContext nu ctx]

data UnificationResult builtin
  = Success [WithContext (UnificationConstraint builtin)]
  | -- | Always an error
    HardFailure
  | -- | Only an error when further reduction will never occur.
    SoftFailure MetaSet

instance Semigroup (UnificationResult builtin) where
  HardFailure <> _ = HardFailure
  _ <> HardFailure = HardFailure
  SoftFailure m1 <> SoftFailure m2 = SoftFailure (m1 <> m2)
  r1@SoftFailure {} <> _ = r1
  _ <> r2@SoftFailure {} = r2
  Success cs1 <> Success cs2 = Success (cs1 <> cs2)

instance Monoid (UnificationResult builtin) where
  mempty = Success mempty

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x, y)

unification ::
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  MetaSet ->
  (Value builtin, Value builtin) ->
  m (UnificationResult builtin)
unification ctx reductionBlockingMetas = \case
  -----------------------
  -- Rigid-rigid cases --
  -----------------------
  VUniverse l1 :~: VUniverse l2
    | l1 == l2 -> solveTrivially
  VBoundVar v1 spine1 :~: VBoundVar v2 spine2
    | v1 == v2 -> solveSpine ctx spine1 spine2
  VFreeVar v1 spine1 :~: VFreeVar v2 spine2
    | v1 == v2 -> solveSpine ctx spine1 spine2
  VBuiltin b1 spine1 :~: VBuiltin b2 spine2
    | b1 == b2 -> solveSpine ctx spine1 spine2
  VPi binder1 body1 :~: VPi binder2 body2
    | visibilityMatches binder1 binder2 -> solvePi ctx (binder1, body1) (binder2, body2)
  VLam binder1 env1 body1 :~: VLam binder2 env2 body2 ->
    solveLam (binder1, env1, body1) (binder2, env2, body2)
  ---------------------
  -- Flex-flex cases --
  ---------------------
  VMeta meta1 spine1 :~: VMeta meta2 spine2
    | meta1 == meta2 -> solveSpine ctx spine1 spine2
    -- The longer spine normally means its in a deeper scope. This minor
    -- optimisation tries to solve the deeper meta first.
    | length spine1 < length spine2 -> solveFlexFlex ctx (meta2, spine2) (meta1, spine1)
    | otherwise -> solveFlexFlex ctx (meta1, spine1) (meta2, spine2)
  ----------------------
  -- Flex-rigid cases --
  ----------------------
  VMeta meta spine :~: e -> solveFlexRigid ctx (meta, spine) e
  e :~: VMeta meta spine -> solveFlexRigid ctx (meta, spine) e
  -----------
  -- Other --
  -----------
  _ -> return $ SoftFailure reductionBlockingMetas

solveTrivially :: (MonadUnify builtin m) => m (UnificationResult builtin)
solveTrivially = do
  logDebug MaxDetail "solved-trivially"
  return $ Success mempty

solveArg ::
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  (VArg builtin, VArg builtin) ->
  Maybe (m (UnificationResult builtin))
solveArg ctx (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = Just $ return HardFailure
  | isInstance arg1 = Nothing
  | otherwise = Just $ do
      argEq <- unify ctx (argExpr arg1, argExpr arg2)
      return $ Success [argEq]

solveSpine ::
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  Spine builtin ->
  Spine builtin ->
  m (UnificationResult builtin)
solveSpine ctx args1 args2
  | length args1 /= length args2 = return HardFailure
  | otherwise = do
      constraints <- sequence $ mapMaybe (solveArg ctx) (zip args1 args2)
      return $ mconcat constraints

solveLam ::
  (MonadUnify builtin m) =>
  (VBinder builtin, Env builtin, Expr Ix builtin) ->
  (VBinder builtin, Env builtin, Expr Ix builtin) ->
  m (UnificationResult builtin)
solveLam _l1 _l2 = compilerDeveloperError "unification of type-level lambdas not yet supported"

solvePi ::
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  (VBinder builtin, Value builtin) ->
  (VBinder builtin, Value builtin) ->
  m (UnificationResult builtin)
solvePi ctx (binder1, body1) (binder2, body2) = do
  -- !!TODO!! Block until binders are solved
  -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
  -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
  binderConstraint <- unify ctx (typeOf binder1, typeOf binder2)
  bodyConstraint <- unify ctx (body1, body2)
  return $ Success [binderConstraint, bodyConstraint]

solveFlexFlex :: (MonadUnify builtin m) => ConstraintContext builtin -> (MetaID, Spine builtin) -> (MetaID, Spine builtin) -> m (UnificationResult builtin)
solveFlexFlex ctx (meta1, spine1) (meta2, spine2) = do
  -- It may be that only one of the two spines is invertible
  maybeRenaming <- invert (contextDBLevel ctx) (meta1, spine1)
  case maybeRenaming of
    Nothing -> solveFlexRigid ctx (meta2, spine2) (VMeta meta1 spine1)
    Just renaming -> solveFlexRigidWithRenaming ctx (meta1, spine1) renaming (VMeta meta2 spine2)

solveFlexRigid :: (MonadUnify builtin m) => ConstraintContext builtin -> (MetaID, Spine builtin) -> Value builtin -> m (UnificationResult builtin)
solveFlexRigid ctx (metaID, spine) solution = do
  -- Check that 'spine' is a pattern and try to calculate a substitution
  -- that renames the variables in `solution` to ones available to `meta`
  maybeRenaming <- invert (contextDBLevel ctx) (metaID, spine)
  case maybeRenaming of
    Just renaming -> solveFlexRigidWithRenaming ctx (metaID, spine) renaming solution
    -- This constraint is stuck because it is not pattern; shelve
    -- it for now and hope that another constraint allows us to
    -- progress.
    Nothing -> return $ SoftFailure $ MetaSet.singleton metaID

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

  unnormSolution <- quote mempty (contextDBLevel ctx) prunedSolution
  let substSolution = substDBAll 0 (\v -> unIx v `IntMap.lookup` renaming) unnormSolution
  solveMeta metaID substSolution (boundContext ctx)
  return $ Success mempty

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
            metaSubst <- getMetaSubstitution
            case MetaMap.lookup m metaSubst of
              Just solution -> go =<< evalApp (normalised solution) spine
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
      VPi binder result -> VPi <$> traverse go binder <*> go result
      -- Definitely going to have come back and fix this one later.
      -- Can't inspect the metas in the environment, as not every variable
      -- in the environment will be used?
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
  let newDeps = fmap (\v -> prettyFriendly (WithContext (BoundVar p v :: StandardExpr) boundCtx)) dbIndices

  logCompilerSection MaxDetail ("restricting dependencies of" <+> pretty meta <+> "to" <+> sep newDeps) $ do
    let levelSet = IntSet.fromList $ fmap unLv newDependencies
    let makeElem (i, v) = if i `IntSet.member` levelSet then Just v else Nothing
    let typingBoundCtx = boundContext ctx
    let ctxWithLevels = zip (reverse [0 .. length typingBoundCtx - 1 :: Int]) typingBoundCtx
    let restrictedContext = mapMaybe makeElem ctxWithLevels
    newMetaExpr <- freshMetaExpr p metaType restrictedContext

    let substitution = IntMap.fromAscList (zip [0 ..] (reverse dbIndices))
    let substMetaExpr = substDBAll 0 (\v -> unIx v `IntMap.lookup` substitution) (unnormalised newMetaExpr)
    solveMeta meta substMetaExpr (boundContext ctx)

    return $ normalised newMetaExpr

unify ::
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  (Value builtin, Value builtin) ->
  m (WithContext (UnificationConstraint builtin))
unify ctx (e1, e2) = WithContext (Unify e1 e2) <$> copyContext ctx

--------------------------------------------------------------------------------
-- Argument patterns

type Renaming = IntMap Ix

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
invert :: forall builtin m. (MonadUnify builtin m) => Lv -> (MetaID, Spine builtin) -> m (Maybe Renaming)
invert ctxSize (metaID, spine) = do
  metaCtxSize <- length <$> getMetaCtx @builtin metaID
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
