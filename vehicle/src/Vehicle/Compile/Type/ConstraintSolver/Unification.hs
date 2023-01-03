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
import Data.Maybe (catMaybes)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalApp)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.Core
  ( unify,
  )
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Map (MetaMap)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup, member, singleton, toList)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (fromList, null, singleton, unions)
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Unification algorithm

-- See
-- https://github.com/AndrasKovacs/elaboration-zoo/blob/master/03-holes/Main.hs
-- for an excellent tutorial on the algorithm.

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x, y)

solveUnificationConstraint :: TCM m => WithContext UnificationConstraint -> m ()
solveUnificationConstraint uc = do
  (WithContext cu@(Unify e1 e2) ctx) <- return uc
  result <- unification ctx e1 e2
  case result of
    Stuck metas -> do
      let blockedConstraint = blockConstraintOn (WithContext (UnificationConstraint cu) ctx) metas
      addConstraints [blockedConstraint]
    Progress newConstraints -> do
      -- In theory this shouldn't be needed, but in practice it is as if
      -- not all the meta-variables are substituted through then the scope of some
      -- meta-variables may be larger than the current scope of the constraint.
      -- These dependencies only disappear on substitution.
      substNewConstraints <- substMetas newConstraints
      addConstraints substNewConstraints

unification :: TCM m => ConstraintContext -> NormExpr -> NormExpr -> m ConstraintProgress
unification ctx e1 e2 = do
  (ne1, e1BlockingMetas) <- forceHead e1
  (ne2, e2BlockingMetas) <- forceHead e2

  let c = WithContext (Unify ne1 ne2) ctx
  let constraintLevel = DBLevel $ length (boundContext ctx)

  progress <- case (ne1, ne2) of
    -----------------------
    -- Rigid-rigid cases --
    -----------------------

    VUniverse _ l1 :~: VUniverse _ l2
      | l1 == l2 -> solveTrivially
    VLiteral _ l1 :~: VLiteral _ l2
      | l1 == l2 -> solveTrivially
    VBuiltin _ b1 spine1 :~: VBuiltin _ b2 spine2
      | b1 == b2 -> solveSpine c spine1 spine2
    VBoundVar _ v1 spine1 :~: VBoundVar _ v2 spine2
      | v1 == v2 -> solveSpine c spine1 spine2
    VFreeVar _ v1 spine1 :~: VFreeVar _ v2 spine2
      | v1 == v2 -> solveSpine c spine1 spine2
    -- We ASSUME that all terms here are in normal form, so there
    -- will never be an unreduced redex.
    VLam _ _binder1 _env1 _body1 :~: VLam _ _binder2 _env2 _body2 ->
      compilerDeveloperError "unification of type-level lambdas not yet supported"
    -- \| visibilityMatches binder1 binder2 -> return $ Progress [unify ctx body1 body2]
    -- \| otherwise                         -> throwError $ FailedUnificationConstraints [c]

    VLVec _ es1 args1 :~: VLVec _ es2 args2
      | length es1 == length es2 -> do
          let elemEqs = zipWith (unify ctx) es1 es2
          argsProgress <- solveSpine c args1 args2
          return $ Progress elemEqs <> argsProgress
    VPi _ binder1 body1 :~: VPi _ binder2 body2
      | visibilityMatches binder1 binder2 -> do
          -- !!TODO!! Block until binders are solved
          -- One possible implementation, blocked metas = set of sets where outer is conjunction and inner is disjunction
          -- BOB: this effectively blocks until the binders are solved, because we usually just try to eagerly solve problems
          let binderConstraint = unify ctx (typeOf binder1) (typeOf binder2)
          let bodyConstraint = unify ctx body1 body2
          return $ Progress [binderConstraint, bodyConstraint]

    ---------------------
    -- Flex-flex cases --
    ---------------------

    VMeta _ i args1 :~: VMeta _ j args2
      -- If the meta-variables are equal then simply discard the constraint
      -- as it doesn't tell us anything.
      | i == j -> solveSpine c args1 args2
      -- If the meta-variables are different then we have more
      -- flexibility as to how the arguments can relate to each other. In
      -- particular they can be re-arranged, and therefore we calculate the
      -- non-positional intersection of their arguments.
      | otherwise -> do
          let (deps1, remainingSpine1) = getNormMetaDependencies args1
          let (deps2, remainingSpine2) = getNormMetaDependencies args2
          if not (null remainingSpine1) || not (null remainingSpine2)
            then return $ Stuck (MetaSet.fromList [i, j])
            else do
              let jointContext = deps1 `intersect` deps2

              meta1Type <- getMetaType i
              meta2Type <- getMetaType j
              typeEq <- unify ctx <$> whnfNBE constraintLevel meta1Type <*> whnfNBE constraintLevel meta2Type

              meta1Origin <- getMetaProvenance i
              meta2Origin <- getMetaProvenance j
              let newOrigin = meta1Origin <> meta2Origin

              meta1Level <- DBLevel <$> getMetaCtxSize i
              meta2Level <- DBLevel <$> getMetaCtxSize j
              let newLevel = min meta1Level meta2Level

              newMeta <- createMetaWithRestrictedDependencies newLevel newOrigin meta1Type jointContext

              solveMeta i newMeta constraintLevel
              solveMeta j newMeta constraintLevel

              return $ Progress [typeEq]

    ----------------------
    -- Flex-rigid cases --
    ----------------------

    VMeta _ m spine :~: _ -> solveFlexRigid constraintLevel m spine e2
    _ :~: VMeta _ m spine -> solveFlexRigid constraintLevel m spine e1
    -- Catch-all
    _ -> do
      let blockingMetas = MetaSet.unions [e1BlockingMetas, e2BlockingMetas]
      if not (MetaSet.null blockingMetas)
        then return $ Stuck blockingMetas
        else throwError (FailedUnificationConstraints [c])

  return progress

solveFlexRigid :: TCM m => DBLevel -> MetaID -> Spine -> NormExpr -> m ConstraintProgress
solveFlexRigid constraintLevel meta spine e2 = do
  -- Check that 'args' is a pattern and try to calculate a substitution
  -- that renames the variables in 'e2' to ones available to meta `i`
  case getArgPattern constraintLevel spine of
    -- This constraint is stuck because it is not pattern; shelve
    -- it for now and hope that another constraint allows us to
    -- progress.
    -- TODO need to check with Bob what this is stuck on, presumably i?
    Nothing -> return $ Stuck $ MetaSet.singleton meta
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
      return $ Progress mempty

solveArg ::
  TCM m =>
  WithContext UnificationConstraint ->
  (NormArg, NormArg) ->
  m (Maybe (WithContext Constraint))
solveArg c (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = throwError $ FailedUnificationConstraints [c]
  | isInstance arg1 = return Nothing
  | otherwise = return $ Just $ unify (contextOf c) (argExpr arg1) (argExpr arg2)

solveSpine ::
  TCM m =>
  WithContext UnificationConstraint ->
  [NormArg] ->
  [NormArg] ->
  m ConstraintProgress
solveSpine constraint args1 args2
  | length args1 /= length args2 = throwError $ FailedUnificationConstraints [constraint]
  | null args1 = solveTrivially
  | otherwise = do
      newConstraints <- catMaybes <$> traverse (solveArg constraint) (zip args1 args2)
      return $ Progress newConstraints

solveTrivially :: TCM m => m ConstraintProgress
solveTrivially = do
  logDebug MaxDetail "solved-trivially"
  return $ Progress mempty

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

--------------------------------------------------------------------------------
-- Argument patterns

type ArgPattern = IntMap DBIndex

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
getArgPattern :: DBLevel -> Spine -> Maybe ArgPattern
getArgPattern ctxSize args = go (length args - 1) IntMap.empty args
  where
    go :: Int -> IntMap DBIndex -> Spine -> Maybe ArgPattern
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
