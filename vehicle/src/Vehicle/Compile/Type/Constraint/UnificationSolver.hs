{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Type.Constraint.UnificationSolver
  ( solveUnificationConstraint,
  )
where

import Control.Monad (when)
import Control.Monad.Writer (execWriterT)
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Foldable (for_, traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup, member, singleton, toList)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (null, unions)
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem (TypableBuiltin (..))
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Unification solver

-- See https://github.com/AndrasKovacs/elaboration-zoo/
-- for an excellent tutorial on the algorithm.

--------------------------------------------------------------------------------
-- Unification algorithm

type MonadUnify builtin m = TCM builtin m

solveUnificationConstraint :: MonadUnify builtin m => WithContext (UnificationConstraint builtin) -> m ()
solveUnificationConstraint (WithContext (Unify e1 e2) ctx) = do
  (ne1', e1BlockingMetas) <- forceHead e1
  (ne2', e2BlockingMetas) <- forceHead e2

  -- In theory this substitution shouldn't be needed, but in practice it is as if
  -- not all the meta-variables are substituted through then the scope of some
  -- meta-variables may be larger than the current scope of the constraint.
  -- These dependencies only disappear on substitution. Need to work out how to
  -- avoid doing this.
  nu@(Unify ne1 ne2) <- substMetas (Unify ne1' ne2')

  result <- unification ctx (ne1, ne2)
  case result of
    Success newConstraints -> do
      addUnificationConstraints newConstraints
    SoftFailure -> do
      let blockingMetas = MetaSet.unions [e1BlockingMetas, e2BlockingMetas]
      if MetaSet.null blockingMetas
        then handleTypingError (FailedUnification [WithContext nu ctx])
        else do
          let normConstraint = WithContext nu ctx
          let blockedConstraint = blockConstraintOn normConstraint blockingMetas
          addUnificationConstraints [blockedConstraint]
    HardFailure ->
      handleTypingError (FailedUnification [WithContext nu ctx])

data UnificationResult builtin
  = Success [WithContext (UnificationConstraint builtin)]
  | -- | Always an error
    HardFailure
  | -- | Only an error when further reduction will never occur.
    SoftFailure

instance Semigroup (UnificationResult builtin) where
  HardFailure <> _ = HardFailure
  _ <> HardFailure = HardFailure
  SoftFailure <> _ = SoftFailure
  _ <> SoftFailure = SoftFailure
  Success cs1 <> Success cs2 = Success (cs1 <> cs2)

instance Monoid (UnificationResult builtin) where
  mempty = Success mempty

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x, y)

unification ::
  MonadUnify builtin m =>
  ConstraintContext builtin ->
  (NormExpr builtin, NormExpr builtin) ->
  m (UnificationResult builtin)
unification ctx = \case
  -----------------------
  -- Rigid-rigid cases --
  -----------------------

  VUniverse l1 :~: VUniverse l2
    | l1 == l2 -> solveTrivially
  VLiteral l1 :~: VLiteral l2
    | l1 == l2 -> solveTrivially
  VBoundVar v1 spine1 :~: VBoundVar v2 spine2
    | v1 == v2 -> solveSpine ctx spine1 spine2
  VFreeVar v1 spine1 :~: VFreeVar v2 spine2
    | v1 == v2 -> solveSpine ctx spine1 spine2
  VBuiltin b1 spine1 :~: VBuiltin b2 spine2
    | b1 == b2 -> solveSpine ctx spine1 spine2
  VLVec xs1 spine1 :~: VLVec xs2 spine2
    | length xs1 == length xs2 -> solveVec ctx (xs1, spine1) (xs2, spine2)
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
  -- Catch-all
  _ -> return SoftFailure

solveTrivially :: MonadUnify builtin m => m (UnificationResult builtin)
solveTrivially = do
  logDebug MaxDetail "solved-trivially"
  return $ Success mempty

solveArg :: ConstraintContext builtin -> (NormArg builtin, NormArg builtin) -> Maybe (UnificationResult builtin)
solveArg ctx (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = Just HardFailure
  | isInstance arg1 = Nothing
  | otherwise = Just $ Success [unify ctx (argExpr arg1) (argExpr arg2)]

solveSpine ::
  MonadUnify builtin m =>
  ConstraintContext builtin ->
  [NormArg builtin] ->
  [NormArg builtin] ->
  m (UnificationResult builtin)
solveSpine ctx args1 args2
  | length args1 /= length args2 = return HardFailure
  | otherwise = return $ mconcat $ mapMaybe (solveArg ctx) (zip args1 args2)

solveLam ::
  MonadUnify builtin m =>
  (NormBinder builtin, Env builtin, DBExpr builtin) ->
  (NormBinder builtin, Env builtin, DBExpr builtin) ->
  m (UnificationResult builtin)
solveLam _l1 _l2 = compilerDeveloperError "unification of type-level lambdas not yet supported"

solveVec ::
  MonadUnify builtin m =>
  ConstraintContext builtin ->
  ([NormExpr builtin], Spine builtin) ->
  ([NormExpr builtin], Spine builtin) ->
  m (UnificationResult builtin)
solveVec ctx (xs1, spine1) (xs2, spine2) = do
  let elemProgress = Success $ zipWith (unify ctx) xs1 xs2
  argsProgress <- solveSpine ctx spine1 spine2
  return $ elemProgress <> argsProgress

solvePi ::
  MonadUnify builtin m =>
  ConstraintContext builtin ->
  (NormBinder builtin, NormExpr builtin) ->
  (NormBinder builtin, NormExpr builtin) ->
  m (UnificationResult builtin)
solvePi ctx (binder1, body1) (binder2, body2) = do
  -- !!TODO!! Block until binders are solved
  -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
  -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
  let binderConstraint = unify ctx (typeOf binder1) (typeOf binder2)
  let bodyConstraint = unify ctx body1 body2
  return $ Success [binderConstraint, bodyConstraint]

solveFlexFlex :: MonadUnify builtin m => ConstraintContext builtin -> (MetaID, Spine builtin) -> (MetaID, Spine builtin) -> m (UnificationResult builtin)
solveFlexFlex ctx (meta1, spine1) (meta2, spine2) = do
  -- It may be that only one of the two spines is invertible
  let maybeRenaming = invert (contextDBLevel ctx) spine1
  case maybeRenaming of
    Nothing -> solveFlexRigid ctx (meta2, spine2) (VMeta meta1 spine1)
    Just renaming -> solveFlexRigidWithRenaming ctx (meta1, spine1) renaming (VMeta meta2 spine2)

solveFlexRigid :: MonadUnify builtin m => ConstraintContext builtin -> (MetaID, Spine builtin) -> NormExpr builtin -> m (UnificationResult builtin)
solveFlexRigid ctx (meta, spine) = do
  let constraintLevel = DBLevel $ length (boundContext ctx)
  -- Check that 'args' is a pattern and try to calculate a substitution
  -- that renames the variables in 'e2' to ones available to meta `i`
  case invert constraintLevel spine of
    -- This constraint is stuck because it is not pattern; shelve
    -- it for now and hope that another constraint allows us to
    -- progress.
    Nothing -> \_ -> return SoftFailure
    Just renaming -> solveFlexRigidWithRenaming ctx (meta, spine) renaming

solveFlexRigidWithRenaming :: forall builtin m. MonadUnify builtin m => ConstraintContext builtin -> (MetaID, Spine builtin) -> Renaming -> NormExpr builtin -> m (UnificationResult builtin)
solveFlexRigidWithRenaming ctx (meta, spine) renaming e2 = do
  metasInE2 <- metasInWithDependencies e2

  -- If `i` is inside the term we're trying to unify it with then error.
  -- Unsure if this should be a user or a developer error.
  when (meta `MetaMap.member` metasInE2) $
    compilerDeveloperError $
      "Meta variable"
        <+> pretty meta
        <+> "found in own solution"
        <+> squotes (prettyVerbose e2)

  let (deps, _) = getNormMetaDependencies spine

  -- Restrict any arguments to each sub-meta on the RHS to those of i.
  for_ (MetaMap.toList metasInE2) $ \(j, jSpine) -> do
    jOrigin <- getMetaProvenance (Proxy @builtin) j
    jType <- getMetaType j
    let (jDeps, _) = getNormMetaDependencies jSpine
    let sharedDependencies = deps `intersect` jDeps
    if sharedDependencies == jDeps
      then return False
      else do
        newMeta <- createMetaWithRestrictedDependencies ctx jOrigin jType sharedDependencies
        solveMeta j newMeta (boundContext ctx)
        return True

  unnormSolution <- quote mempty (contextDBLevel ctx) e2
  let substSolution = substDBAll 0 (\v -> unIndex v `IntMap.lookup` renaming) unnormSolution
  solveMeta meta substSolution (boundContext ctx)
  return $ Success mempty

createMetaWithRestrictedDependencies ::
  MonadUnify builtin m =>
  ConstraintContext builtin ->
  Provenance ->
  CheckedType builtin ->
  [DBLevel] ->
  m (CheckedExpr builtin)
createMetaWithRestrictedDependencies ctx p metaType newDependencies = do
  logCompilerPass MaxDetail "restricting dependencies" $ do
    let restrictedContext = restrictBoundContext newDependencies (boundContext ctx)
    meta <- freshExprMeta p metaType restrictedContext
    let constraintLevel = contextDBLevel ctx
    let substitution = IntMap.fromAscList (zip [0 ..] (fmap (dbLevelToIndex constraintLevel) newDependencies))
    let newMetaExpr = substDBAll 0 (\v -> unIndex v `IntMap.lookup` substitution) (unnormalised meta)

    logDebug MaxDetail ("Result:" <+> prettyVerbose newMetaExpr)

    return newMetaExpr

unify :: ConstraintContext builtin -> NormExpr builtin -> NormExpr builtin -> WithContext (UnificationConstraint builtin)
unify ctx e1 e2 = WithContext (Unify e1 e2) (copyContext ctx)

restrictBoundContext :: [DBLevel] -> TypingBoundCtx builtin -> TypingBoundCtx builtin
restrictBoundContext levels ctx = do
  let levelSet = IntSet.fromList $ fmap unLevel levels
  let makeElem (i, v) = if i `IntSet.member` levelSet then Just v else Nothing
  mapMaybe makeElem (zip [length ctx - 1 .. 0 :: Int] ctx)

--------------------------------------------------------------------------------
-- Argument patterns

type Renaming = IntMap DBIndex

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
invert :: DBLevel -> Spine builtin -> Maybe Renaming
invert ctxSize args = go (length args - 1) IntMap.empty args
  where
    go :: Int -> IntMap DBIndex -> Spine builtin -> Maybe Renaming
    go i revMap = \case
      [] -> Just revMap
      (ExplicitArg _ (VBoundVar j []) : restArgs) -> do
        -- TODO: we could eta-reduce arguments too, if possible
        let jIndex = dbLevelToIndex ctxSize j
        if IntMap.member (unIndex jIndex) revMap
          then -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning;
          -- but then we should make sure the solution is well-typed
            Nothing
          else go (i - 1) (IntMap.insert (unIndex jIndex) (DBIndex i) revMap) restArgs
      -- Not a pattern so return nothing.
      _ -> Nothing

metasInWithDependencies :: MonadUnify builtin m => NormExpr builtin -> m (MetaMap (Spine builtin))
metasInWithDependencies e = execWriterT (go e)
  where
    go :: (MonadUnify builtin m, MonadWriter (MetaMap (Spine builtin)) m) => NormExpr builtin -> m ()
    go expr = case expr of
      VMeta m spine -> do
        metaSubst <- getMetaSubstitution
        case MetaMap.lookup m metaSubst of
          Nothing -> tell (MetaMap.singleton m spine)
          Just solution -> go =<< evalApp (normalised solution) spine
      VUniverse {} -> return ()
      VLiteral {} -> return ()
      VBuiltin _ spine -> goSpine spine
      VBoundVar _ spine -> goSpine spine
      VFreeVar _ spine -> goSpine spine
      VLVec xs spine -> do traverse_ go xs; goSpine spine
      VPi binder result -> do traverse_ go binder; go result
      -- Definitely going to have come back and fix this one later.
      -- Can't inspect the metas in the environment, as not every variable
      -- in the environment will be used?
      VLam {} -> return ()

    goSpine :: (MonadUnify builtin m, MonadWriter (MetaMap (Spine builtin)) m) => Spine builtin -> m ()
    goSpine = traverse_ (traverse_ go)
