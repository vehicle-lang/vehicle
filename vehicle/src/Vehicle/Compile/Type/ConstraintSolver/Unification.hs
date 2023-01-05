{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Vehicle.Compile.Type.ConstraintSolver.Unification
  ( solveUnificationConstraint,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..), throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer (execWriterT)
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Foldable (for_, traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalApp)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup, member, singleton, toList)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (null, unions)
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Unification algorithm

-- See
-- https://github.com/AndrasKovacs/elaboration-zoo/blob/master/03-holes/Main.hs
-- for an excellent tutorial on the algorithm.

solveUnificationConstraint :: TCM m => WithContext UnificationConstraint -> m ()
solveUnificationConstraint constraint = do
  -- In theory this substitution shouldn't be needed, but in practice it is as if
  -- not all the meta-variables are substituted through then the scope of some
  -- meta-variables may be larger than the current scope of the constraint.
  -- These dependencies only disappear on substitution. Need to work out how to
  -- avoid doing this.
  (WithContext (Unify e1 e2) ctx) <- substMetas constraint

  (ne1, e1BlockingMetas) <- forceHead e1
  (ne2, e2BlockingMetas) <- forceHead e2
  let nu = Unify ne1 ne2
  result <- unification ctx (ne1, ne2)
  case result of
    Success newConstraints -> do
      addConstraints (fmap (mapObject UnificationConstraint) newConstraints)
    SoftFailure -> do
      let blockingMetas = MetaSet.unions [e1BlockingMetas, e2BlockingMetas]
      if MetaSet.null blockingMetas
        then throwError (FailedUnificationConstraints [WithContext nu ctx])
        else do
          let normConstraint = WithContext (UnificationConstraint nu) ctx
          let blockedConstraint = blockConstraintOn normConstraint blockingMetas
          addConstraints [blockedConstraint]
    HardFailure -> do
      throwError (FailedUnificationConstraints [WithContext nu ctx])

data UnificationResult
  = Success [WithContext UnificationConstraint]
  | -- | Always an error
    HardFailure
  | -- | Only an error when further reduction will never occur.
    SoftFailure

instance Semigroup UnificationResult where
  HardFailure <> _ = HardFailure
  _ <> HardFailure = HardFailure
  SoftFailure <> _ = SoftFailure
  _ <> SoftFailure = SoftFailure
  Success cs1 <> Success cs2 = Success (cs1 <> cs2)

instance Monoid UnificationResult where
  mempty = Success mempty

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x, y)

unification :: TCM m => ConstraintContext -> (NormExpr, NormExpr) -> m UnificationResult
unification ctx = \case
  -----------------------
  -- Rigid-rigid cases --
  -----------------------

  VUniverse _ l1 :~: VUniverse _ l2
    | l1 == l2 -> solveTrivially
  VLiteral _ l1 :~: VLiteral _ l2
    | l1 == l2 -> solveTrivially
  VBoundVar _ v1 spine1 :~: VBoundVar _ v2 spine2
    | v1 == v2 -> solveSpine ctx spine1 spine2
  VFreeVar _ v1 spine1 :~: VFreeVar _ v2 spine2
    | v1 == v2 -> solveSpine ctx spine1 spine2
  VBuiltin _ b1 spine1 :~: VBuiltin _ b2 spine2
    | b1 == b2 -> solveSpine ctx spine1 spine2
  VLVec _ xs1 spine1 :~: VLVec _ xs2 spine2
    | length xs1 == length xs2 -> solveVec ctx (xs1, spine1) (xs2, spine2)
  VPi _ binder1 body1 :~: VPi _ binder2 body2
    | visibilityMatches binder1 binder2 -> solvePi ctx (binder1, body1) (binder2, body2)
  VLam _ binder1 env1 body1 :~: VLam _ binder2 env2 body2 ->
    solveLam (binder1, env1, body1) (binder2, env2, body2)
  ---------------------
  -- Flex-flex cases --
  ---------------------

  VMeta _ meta1 spine1 :~: VMeta _ meta2 spine2
    | meta1 == meta2 -> solveSpine ctx spine1 spine2
    | otherwise -> solveFlexFlex ctx (meta1, spine1) (meta2, spine2)
  ----------------------
  -- Flex-rigid cases --
  ----------------------

  VMeta _ meta spine :~: e -> solveFlexRigid ctx (meta, spine) e
  e :~: VMeta _ meta spine -> solveFlexRigid ctx (meta, spine) e
  -- Catch-all
  _ -> return SoftFailure

solveTrivially :: TCM m => m UnificationResult
solveTrivially = do
  logDebug MaxDetail "solved-trivially"
  return $ Success mempty

solveArg :: ConstraintContext -> (NormArg, NormArg) -> Maybe UnificationResult
solveArg ctx (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = Just HardFailure
  | isInstance arg1 = Nothing
  | otherwise = Just $ Success [unify ctx (argExpr arg1) (argExpr arg2)]

solveSpine ::
  TCM m =>
  ConstraintContext ->
  [NormArg] ->
  [NormArg] ->
  m UnificationResult
solveSpine ctx args1 args2
  | length args1 /= length args2 = return HardFailure
  | otherwise = return $ mconcat $ mapMaybe (solveArg ctx) (zip args1 args2)

solveLam ::
  TCM m =>
  (NormBinder, Env, CheckedExpr) ->
  (NormBinder, Env, CheckedExpr) ->
  m UnificationResult
solveLam _l1 _l2 = compilerDeveloperError "unification of type-level lambdas not yet supported"

solveVec ::
  TCM m =>
  ConstraintContext ->
  ([NormExpr], Spine) ->
  ([NormExpr], Spine) ->
  m UnificationResult
solveVec ctx (xs1, spine1) (xs2, spine2) = do
  let elemProgress = Success $ zipWith (unify ctx) xs1 xs2
  argsProgress <- solveSpine ctx spine1 spine2
  return $ elemProgress <> argsProgress

solvePi ::
  TCM m =>
  ConstraintContext ->
  (NormBinder, NormExpr) ->
  (NormBinder, NormExpr) ->
  m UnificationResult
solvePi ctx (binder1, body1) (binder2, body2) = do
  -- !!TODO!! Block until binders are solved
  -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
  -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
  let binderConstraint = unify ctx (typeOf binder1) (typeOf binder2)
  let bodyConstraint = unify ctx body1 body2
  return $ Success [binderConstraint, bodyConstraint]

{-
solveFlexFlex :: TCM m => DBLevel -> MetaID -> Spine -> MetaID -> Spine -> m ConstraintProgress
solveFlexFlex level meta1 spine1 meta2 spine2
  -- The longer spine normally means its in a deeper scope. This minor
  -- optimisation tries to solve the deeper meta first.
  | length spine1 < length spine2 = go meta2 spine2 meta1 spine1
  | otherwise = go meta1 spine1 meta2 spine2
  where
  -- It may be that only one of the two spines is invertible
  go :: MetaID -> Spine -> MetaID -> Spine -> m ConstraintProgress
  go m1 sp1 m2 sp2 = do
    let maybeRenaming = invert level sp1
    case maybeRenaming of
      Nothing -> solveFlexRigid level m' sp' (VFlex m sp)
      Just renaming -> _ --solveWithPRen m pren (VFlex m' sp')
-}
solveFlexFlex :: TCM m => ConstraintContext -> (MetaID, Spine) -> (MetaID, Spine) -> m UnificationResult
solveFlexFlex ctx (meta1, spine1) (meta2, spine2) = do
  -- If the meta-variables are different then we have more
  -- flexibility as to how the arguments can relate to each other. In
  -- particular they can be re-arranged, and therefore we calculate the
  -- non-positional intersection of their arguments.
  let (deps1, remainingSpine1) = getNormMetaDependencies spine1
  let (deps2, remainingSpine2) = getNormMetaDependencies spine2
  if not (null remainingSpine1) || not (null remainingSpine2)
    then return SoftFailure
    else do
      let constraintLevel = contextDBLevel ctx
      let jointContext = deps1 `intersect` deps2

      meta1Type <- getMetaType meta1
      meta2Type <- getMetaType meta2
      typeEq <- unify ctx <$> whnfNBE constraintLevel meta1Type <*> whnfNBE constraintLevel meta2Type

      meta1Origin <- getMetaProvenance meta1
      meta2Origin <- getMetaProvenance meta2
      let newOrigin = meta1Origin <> meta2Origin

      meta1Level <- DBLevel <$> getMetaCtxSize meta1
      meta2Level <- DBLevel <$> getMetaCtxSize meta2
      let newLevel = min meta1Level meta2Level

      newMeta <- createMetaWithRestrictedDependencies newLevel newOrigin meta1Type jointContext

      solveMeta meta1 newMeta constraintLevel
      solveMeta meta2 newMeta constraintLevel

      return $ Success [typeEq]

solveFlexRigid :: TCM m => ConstraintContext -> (MetaID, Spine) -> NormExpr -> m UnificationResult
solveFlexRigid ctx (meta, spine) e2 = do
  let constraintLevel = DBLevel $ length (boundContext ctx)
  -- Check that 'args' is a pattern and try to calculate a substitution
  -- that renames the variables in 'e2' to ones available to meta `i`
  case invert constraintLevel spine of
    -- This constraint is stuck because it is not pattern; shelve
    -- it for now and hope that another constraint allows us to
    -- progress.
    Nothing -> return SoftFailure
    Just argPattern -> do
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
        jOrigin <- getMetaProvenance j
        jType <- getMetaType j
        let (jDeps, _) = getNormMetaDependencies jSpine
        let sharedDependencies = deps `intersect` jDeps
        if sharedDependencies == jDeps
          then return False
          else do
            newMeta <- createMetaWithRestrictedDependencies constraintLevel jOrigin jType sharedDependencies
            solveMeta j newMeta constraintLevel
            return True

      unnormSolution <- quote constraintLevel e2
      let substSolution = substDBAll 0 (\v -> unIndex v `IntMap.lookup` argPattern) unnormSolution
      solveMeta meta substSolution constraintLevel
      return $ Success mempty

createMetaWithRestrictedDependencies ::
  TCM m =>
  DBLevel ->
  Provenance ->
  CheckedType ->
  [DBLevel] ->
  m CheckedExpr
createMetaWithRestrictedDependencies level p metaType newDependencies = do
  logCompilerPass MaxDetail "restricting dependencies" $ do
    meta <- freshExprMeta p metaType (length newDependencies)
    let substitution = IntMap.fromAscList (zip [0 ..] (fmap (dbLevelToIndex level) newDependencies))
    let newMetaExpr = substDBAll 0 (\v -> unIndex v `IntMap.lookup` substitution) (unnormalised meta)

    logDebug MaxDetail ("Result:" <+> prettyVerbose newMetaExpr)

    return newMetaExpr

unify :: ConstraintContext -> NormExpr -> NormExpr -> WithContext UnificationConstraint
unify ctx e1 e2 = WithContext (Unify e1 e2) (copyContext ctx)

--------------------------------------------------------------------------------
-- Argument patterns

type Renaming = IntMap DBIndex

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
invert :: DBLevel -> Spine -> Maybe Renaming
invert ctxSize args = go (length args - 1) IntMap.empty args
  where
    go :: Int -> IntMap DBIndex -> Spine -> Maybe Renaming
    go i revMap = \case
      [] -> Just revMap
      (ExplicitArg _ (VBoundVar _ j []) : restArgs) -> do
        -- TODO: we could eta-reduce arguments too, if possible
        let jIndex = dbLevelToIndex ctxSize j
        if IntMap.member (unIndex jIndex) revMap
          then -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning;
          -- but then we should make sure the solution is well-typed
            Nothing
          else go (i - 1) (IntMap.insert (unIndex jIndex) (DBIndex i) revMap) restArgs
      -- Not a pattern so return nothing.
      _ -> Nothing

metasInWithDependencies :: MonadTypeChecker m => NormExpr -> m (MetaMap Spine)
metasInWithDependencies e = execWriterT (go e)
  where
    go :: (MonadTypeChecker m, MonadWriter (MetaMap Spine) m) => NormExpr -> m ()
    go expr = case expr of
      VMeta _ m spine -> do
        metaSubst <- getMetaSubstitution
        case MetaMap.lookup m metaSubst of
          Nothing -> tell (MetaMap.singleton m spine)
          Just solution -> do
            declSubst <- getDeclSubstitution
            go =<< runReaderT (evalApp (normalised solution) spine) (declSubst, metaSubst)
      VUniverse {} -> return ()
      VLiteral {} -> return ()
      VBuiltin _ _ spine -> goSpine spine
      VBoundVar _ _ spine -> goSpine spine
      VFreeVar _ _ spine -> goSpine spine
      VLVec _ xs spine -> do traverse_ go xs; goSpine spine
      VPi _ binder result -> do traverse_ go binder; go result
      -- Definitely going to have come back and fix this one later.
      -- Can't inspect the metas in the environment, as not every variable
      -- in the environment will be used?
      VLam {} -> return ()

    goSpine :: (MonadTypeChecker m, MonadWriter (MetaMap Spine) m) => Spine -> m ()
    goSpine = traverse_ (traverse_ go)
