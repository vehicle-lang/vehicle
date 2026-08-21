{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Type.Constraint.UnificationSolver
  ( runUnificationSolver,
    solveUnificationConstraint,
    unify,
    UnificationResult (..),
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Prettyprinter (sep)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (null, singleton)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Bound.Context.Name (runNameBoundContextT)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))

--------------------------------------------------------------------------------
-- Unification solver

-- See https://github.com/AndrasKovacs/elaboration-zoo/
-- for an excellent tutorial on the algorithm.

-- | Attempts to solve as many unification constraints as possible.
runUnificationSolver :: (MonadUnify builtin m) => Proxy builtin -> Bool -> m ()
runUnificationSolver proxy topLevel =
  logCompilerSection2 MaxDetail "unification solver run" $
    runConstraintSolver
      getActiveUnificationConstraints
      setUnificationConstraints
      solveUnificationConstraint
      topLevel
      proxy

--------------------------------------------------------------------------------
-- Unification algorithm

type MonadUnify builtin m =
  ( MonadTypeChecker builtin m,
    TypableBuiltin builtin
  )

type UnificationProblem builtin =
  ( BoundCtx (Type builtin),
    ThunkWithMetas builtin,
    ThunkWithMetas builtin
  )

type ConstraintInfo builtin =
  ( UnificationProblem builtin,
    MetaSet
  )

infoBoundCtx :: ConstraintInfo builtin -> BoundCtx (Type builtin)
infoBoundCtx ((ctx, _, _), _) = ctx

data UnificationResult builtin
  = Success
  | -- | Always an error
    HardFailure (NonEmpty (UnificationProblem builtin))
  | -- | Only an error when further reduction will never occur.
    Blocked (NonEmpty (ConstraintInfo builtin))

solveUnificationConstraint ::
  forall builtin m.
  (MonadUnify builtin m) =>
  WithContext (UnificationConstraint builtin) ->
  m ()
solveUnificationConstraint (WithContext (Unify origin e1 e2) ctx) = do
  result <- unify (boundContextOf ctx) e1 e2
  case result of
    Success -> return ()
    Blocked blockedProblems -> do
      newConstraints <- forM blockedProblems $ createNewConstraint ctx origin
      addUnificationConstraints $ NonEmpty.toList newConstraints
    HardFailure failedProblems -> do
      finalFailedConstraints <- forM failedProblems $ \problem ->
        createNewConstraint ctx origin (problem, mempty)
      freeCtx <- getFreeCtx (Proxy @builtin)
      throwError $ TypingError $ FailedUnificationConstraints $ FailedUnificationConstraintsError freeCtx finalFailedConstraints

createNewConstraint ::
  (MonadUnify builtin m) =>
  ConstraintContext builtin ->
  UnificationConstraintOrigin builtin ->
  (UnificationProblem builtin, MetaSet) ->
  m (WithContext (UnificationConstraint builtin))
createNewConstraint constraintCtx origin ((boundCtx, e1, e2), blockingMetas) = do
  newConstraint <- WithContext (Unify origin e1 e2) <$> copyContext constraintCtx (Just boundCtx)
  return $ blockConstraintOn newConstraint blockingMetas

unify ::
  forall builtin m.
  (MonadUnify builtin m) =>
  BoundCtx (Type builtin) ->
  ThunkWithMetas builtin ->
  ThunkWithMetas builtin ->
  m (UnificationResult builtin)
unify ctx e1 e2 = do
  -- Force the heads of both expressions
  let namedCtx = toNamedBoundCtx ctx
  let prettyExpr e = prettyExternal (WithContext e namedCtx)
  let passDoc = "unifying" <+> prettyExpr e1 <+> "~" <+> prettyExpr e2 -- <+> "in context" <+> prettyVerbose ctx
  logIndent MaxDetail passDoc $ do
    (ne1, e1BlockingMetas) <- forceThunkWithMetas namedCtx e1
    (ne2, e2BlockingMetas) <- forceThunkWithMetas namedCtx e2
    logDebug MaxDetail $ "forced-lhs:" <+> prettyExpr (Forced ne1)
    logDebug MaxDetail $ "forced-rhs:" <+> prettyExpr (Forced ne2)
    -- Construct the new constraint information
    let blockingMetas = e1BlockingMetas <> e2BlockingMetas
    let constraintInfo = ((ctx, Forced ne1, Forced ne2), blockingMetas)

    -- Perform the unification
    unification constraintInfo (ne1, ne2)

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

-- | Create a new unification constraint, copying the context as appropriate.
subUnify ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  ThunkWithMetas builtin ->
  ThunkWithMetas builtin ->
  m (UnificationResult builtin)
subUnify info = unify (infoBoundCtx info)

block ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  Maybe MetaSet ->
  m (UnificationResult builtin)
block (problem, originalBlockingMetas) maybeRefinedBlockingMetas = do
  let blockingMetas = fromMaybe originalBlockingMetas maybeRefinedBlockingMetas
  if MetaSet.null blockingMetas
    then return $ HardFailure [problem]
    else return $ Blocked [(problem, blockingMetas)]

pattern (:~:) :: a -> b -> (a, b)
pattern x :~: y = (x, y)

unification ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (ForcedValueWithMetas builtin, ForcedValueWithMetas builtin) ->
  m (UnificationResult builtin)
unification info = \case
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
    | isConstructor b1 && isConstructor b2 -> hardFail info
  VPi binder1 closure1 :~: VPi binder2 closure2
    | visibilityMatches binder1 binder2 -> solveClosure info (binder1, closure1) (binder2, closure2)
  VLam binder1 closure1 :~: VLam binder2 closure2 ->
    solveClosure info (binder1, closure1) (binder2, closure2)
  VRecord ident1 fields1 :~: VRecord ident2 fields2
    | ident1 == ident2 -> solveRecords info fields1 fields2
  VRecordAcc _recordType1 record1 field1 spine1 :~: VRecordAcc _recordType2 record2 field2 spine2
    | field1 == field2 -> do
        recordResult <- subUnify info record1 record2
        spineResult <- solveSpine info spine1 spine2
        return $ recordResult <> spineResult
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
  (UnforcedArgWithMetas builtin, UnforcedArgWithMetas builtin) ->
  m (UnificationResult builtin)
solveArg info (arg1, arg2)
  | not (visibilityMatches arg1 arg2) = hardFail info
  -- Don't unify instances, they should be uniquely determined by the type.
  | isInstance arg1 = return Success
  | otherwise = subUnify info (argExpr arg1) (argExpr arg2)

solveSpine ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  UnforcedSpineWithMetas builtin ->
  UnforcedSpineWithMetas builtin ->
  m (UnificationResult builtin)
solveSpine info args1 args2
  | length args1 /= length args2 = hardFail info
  | otherwise = mconcat <$> traverse (solveArg info) (zip args1 args2)

solveRecords ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  SearchableRecordFields (ThunkWithMetas builtin) ->
  SearchableRecordFields (ThunkWithMetas builtin) ->
  m (UnificationResult builtin)
solveRecords info fields1 fields2 = do
  -- Note we don't need to check that the fields align as scope checking should have
  -- already done this for us.
  let sharedFields = OMap.assocs $ OMap.intersectionWith (const (,)) fields1 fields2
  let solveField (_name, (v1, v2)) = subUnify info v1 v2
  mconcat <$> traverse solveField sharedFields

solveClosure ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (UnforcedBinderWithMetas builtin, ClosureWithMetas builtin) ->
  (UnforcedBinderWithMetas builtin, ClosureWithMetas builtin) ->
  m (UnificationResult builtin)
solveClosure info (binder1, closure1) (binder2, closure2) = do
  -- Unify binder constraints
  binderConstraint <- subUnify info (typeOf binder1) (typeOf binder2)

  -- Evaluate the normalised bodies of the lambdas
  let lv = boundCtxLv $ infoBoundCtx info
  let nbody1 = extendClosureWithBound closure1 binder1 lv
  let nbody2 = extendClosureWithBound closure2 binder2 lv

  -- Update the context.
  let updatedInfo = updateInfoUnderBinder info (binder1, binder2)

  -- Unify the two bodies
  bodyConstraint <- subUnify updatedInfo nbody1 nbody2

  -- Return the result
  return $ binderConstraint <> bodyConstraint

solveFlexFlex ::
  forall builtin m.
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (MetaID, UnforcedSpineWithMetas builtin) ->
  (MetaID, UnforcedSpineWithMetas builtin) ->
  m (UnificationResult builtin)
solveFlexFlex info (meta1, spine1) (meta2, spine2) = do
  let proxy = Proxy @builtin
  c1 <- length <$> getMetaCtx proxy meta1
  c2 <- length <$> getMetaCtx proxy meta2
  let (ctx1Args, extraArgs1) = splitAt c1 spine1
  let (ctx2Args, extraArgs2) = splitAt c2 spine2

  if not (null extraArgs1) && length extraArgs1 == length extraArgs2
    then do
      -- This is a massive hack assuming that the meta is always an injective function.
      -- This is to allow the instance unification to work in the `Decidable` typing
      -- subsystem when inferring if `(Tensor Bool) ds` -> `(\_ds -> Type)` or `Tensor Bool`)
      metaResult <- subUnify info (Forced $ VMeta meta1 ctx1Args) (Forced $ VMeta meta2 ctx2Args)
      spineResults <- solveSpine info extraArgs1 extraArgs2
      return $ metaResult <> spineResults
    else do
      -- It may be that only one of the two spines is invertible
      maybeRenaming <- invert (infoBoundCtx info) (meta1, spine1)
      case maybeRenaming of
        Nothing -> solveFlexRigid info (meta2, spine2) (VMeta meta1 spine1)
        Just renaming -> solveFlexRigidWithRenaming (infoBoundCtx info) (meta1, spine1) renaming (Forced $ VMeta meta2 spine2)

solveFlexRigid ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  (MetaID, UnforcedSpineWithMetas builtin) ->
  ForcedValueWithMetas builtin ->
  m (UnificationResult builtin)
solveFlexRigid info (metaID, spine) solution = do
  let ctx = infoBoundCtx info
  -- Check that 'spine' is a pattern and try to calculate a substitution
  -- that renames the variables in `solution` to ones available to `meta`
  maybeRenaming <- invert ctx (metaID, spine)
  case maybeRenaming of
    Just renaming -> solveFlexRigidWithRenaming ctx (metaID, spine) renaming (Forced solution)
    -- This constraint is stuck because it is not pattern; shelve
    -- it for now and hope that another constraint allows us to
    -- progress.
    Nothing -> block info (Just (MetaSet.singleton metaID))

solveFlexRigidWithRenaming ::
  forall builtin m.
  (MonadUnify builtin m) =>
  BoundCtx (Type builtin) ->
  (MetaID, UnforcedSpineWithMetas builtin) ->
  Renaming ->
  ThunkWithMetas builtin ->
  m (UnificationResult builtin)
solveFlexRigidWithRenaming ctx (metaID, spine) renaming solution = do
  prunedSolution <-
    if useDependentMetas (Proxy @builtin)
      then do
        (deps, _) <- getNormMetaDependencies metaID spine
        pruneMetaDependencies ctx metaID deps solution
      else return solution

  let unnormSolution = unnormalise (boundCtxLv ctx) prunedSolution
  let substSolution = substDBAll 0 (\v -> unIx v `IntMap.lookup` renaming) unnormSolution
  solveMeta metaID substSolution ctx
  return Success

pruneMetaDependencies ::
  forall builtin m.
  (MonadUnify builtin m) =>
  BoundCtx (Type builtin) ->
  MetaID ->
  [Lv] ->
  ThunkWithMetas builtin ->
  m (ThunkWithMetas builtin)
pruneMetaDependencies ctx solvingMetaID solvingMetaDependencies attemptedSolution = do
  goThunk attemptedSolution
  where
    goThunk ::
      (MonadUnify builtin m) =>
      ThunkWithMetas builtin ->
      m (ThunkWithMetas builtin)
    goThunk value = do
      (forcedValue, _) <- forceThunkWithMetas (toNamedBoundCtx ctx) value
      Forced <$> goForcedValue forcedValue

    goForcedValue ::
      (MonadUnify builtin m) =>
      ForcedValueWithMetas builtin ->
      m (ForcedValueWithMetas builtin)
    goForcedValue = \case
      VMeta m spine -> pruneMeta m spine
      VUniverse l -> return $ VUniverse l
      VBuiltin b spine -> VBuiltin b <$> traverse (traverse goThunk) spine
      VBoundVar v spine -> VBoundVar v <$> traverse (traverse goThunk) spine
      VFreeVar v spine -> VFreeVar v <$> traverse (traverse goThunk) spine
      VRecord ident fields -> VRecord ident <$> traverse goThunk fields
      VRecordAcc recordType record field spine ->
        VRecordAcc <$> goThunk recordType <*> goThunk record <*> pure field <*> traverse (traverse goThunk) spine
      -- Definitely going to have come back and fix this one later.
      -- Can't inspect the metas in the environment, as not every variable
      -- in the environment will be used?
      -- The elaboration zoo has pruning and renaming actually return an `Expr`
      -- rather than a `Val`?
      VPi binder body -> VPi <$> traverse goThunk binder <*> pure body
      VLam binder body -> VLam <$> traverse goThunk binder <*> pure body

    pruneMeta ::
      MetaID ->
      UnforcedSpineWithMetas builtin ->
      m (ForcedValueWithMetas builtin)
    pruneMeta m spine
      | m == solvingMetaID =
          -- If `i` is inside the term we're trying to unify it with then error.
          -- Unsure if this should be a user or a developer error.
          compilerDeveloperError $
            "Meta variable"
              <+> pretty m
              <+> "found in own solution"
              <+> squotes (prettyVerbose attemptedSolution)
      | otherwise = do
          metaInfo <- getMetaInfo m
          case metaSolution metaInfo of
            Just solution -> do
              forceApplicationWithMetas (toNamedBoundCtx ctx) (normalised solution) spine
            Nothing -> do
              (jDeps, remainingSpine) <- getNormMetaDependencies m spine
              let sharedDependencies = solvingMetaDependencies `intersect` jDeps
              if sharedDependencies /= jDeps
                then do
                  -- We first recursive prune the type of the meta-variable
                  metaType <- getMetaType m
                  prunedMetaType <- pruneMetaDependencies ctx solvingMetaID solvingMetaDependencies (Unforced (boundContextToEnv ctx) metaType)
                  let unnormalisedPrunedMetaType = unnormalise (boundCtxLv ctx) prunedMetaType
                  -- And then create a new meta-variable recursively.
                  createMetaWithRestrictedDependencies ctx m unnormalisedPrunedMetaType sharedDependencies remainingSpine
                else return $ VMeta m spine

getNormMetaDependencies ::
  forall builtin m.
  (MonadUnify builtin m) =>
  MetaID ->
  UnforcedSpineWithMetas builtin ->
  m ([Lv], UnforcedSpineWithMetas builtin)
getNormMetaDependencies meta spine = do
  metaCtx <- getMetaCtx (Proxy @builtin) meta
  let (deps, remainingArgs) = splitAt (length metaCtx) spine
  forcedDeps <- traverse (\a -> fst <$> forceThunkWithMetas (toNamedBoundCtx metaCtx) (argExpr a)) deps
  let getLv a = case a of
        VBoundVar i [] -> i
        _ -> developerError $ "Meta variable" <+> pretty meta <+> "has non-index arg"
  let lvs = fmap getLv forcedDeps
  return (lvs, remainingArgs)

createMetaWithRestrictedDependencies ::
  forall builtin m.
  (MonadUnify builtin m) =>
  BoundCtx (Type builtin) ->
  MetaID ->
  Type builtin ->
  [Lv] ->
  UnforcedSpineWithMetas builtin ->
  m (ForcedValueWithMetas builtin)
createMetaWithRestrictedDependencies ctx meta metaType newDependencies spine = do
  p <- getMetaProvenance (Proxy @builtin) meta

  let constraintLevel = boundCtxLv ctx
  let dbIndices = fmap (dbLevelToIndex constraintLevel) newDependencies
  let newDeps = fmap (\v -> prettyFriendly (WithContext (BoundVar p v :: Expr builtin) (toNamedBoundCtx ctx))) dbIndices

  logCompilerSection MaxDetail ("restricting dependencies of" <+> pretty meta <+> "to" <+> sep newDeps) $ do
    let levelSet = IntSet.fromList $ fmap unLv newDependencies
    let makeElem (i, v) = if i `IntSet.member` levelSet then Just v else Nothing
    let ctxWithLevels = zip (reverse [0 .. length ctx - 1 :: Int]) ctx
    let restrictedContext = mapMaybe makeElem ctxWithLevels
    newMetaExpr <- freshMetaExpr p metaType restrictedContext

    let substitution = IntMap.fromAscList (zip [0 ..] (reverse dbIndices))
    let substMetaExpr = substDBAll 0 (\v -> unIx v `IntMap.lookup` substitution) newMetaExpr
    solveMeta meta substMetaExpr ctx

    let normMetaExpr = Unforced (boundContextToEnv restrictedContext) newMetaExpr
    runNameBoundContextT (toNamedBoundCtx ctx) $ forceApplicationWithMetas (toNamedBoundCtx ctx) normMetaExpr spine

updateInfoUnderBinder ::
  ConstraintInfo builtin ->
  (UnforcedBinderWithMetas builtin, UnforcedBinderWithMetas builtin) ->
  ConstraintInfo builtin
updateInfoUnderBinder ((ctx, e1, e2), blockingMetas) (binder1, _binder2) = do
  -- Update the context.
  -- NOTE: that we have to unnormalise here indicates something is wrong.
  let unnormBinder = fmap (unnormalise (boundCtxLv ctx)) binder1
  ((unnormBinder : ctx, e1, e2), blockingMetas)

hardFail ::
  (MonadUnify builtin m) =>
  ConstraintInfo builtin ->
  m (UnificationResult builtin)
hardFail (problem, _) = do
  logDebug MaxDetail "failed"
  return $ HardFailure [problem]

--------------------------------------------------------------------------------
-- Argument patterns

type Renaming = IntMap Ix

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
invert ::
  forall builtin m.
  (MonadUnify builtin m) =>
  BoundCtx (Type builtin) ->
  (MetaID, UnforcedSpineWithMetas builtin) ->
  m (Maybe Renaming)
invert ctx (metaID, spine) = do
  metaCtxSize <- length <$> getMetaCtx (Proxy @builtin) metaID
  if metaCtxSize < length spine
    then return Nothing
    else go (metaCtxSize - 1) IntMap.empty spine
  where
    go :: Int -> IntMap Ix -> UnforcedSpineWithMetas builtin -> m (Maybe Renaming)
    go i revMap = \case
      [] -> return $ Just revMap
      (ExplicitArg _ arg : restArgs) -> do
        (fArg, _) <- forceThunkWithMetas (toNamedBoundCtx ctx) arg
        case fArg of
          (VBoundVar j []) -> do
            -- TODO: we could eta-reduce arguments too, if possible
            let jIndex = dbLevelToIndex (boundCtxLv ctx) j
            if IntMap.member (unIx jIndex) revMap
              then -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning;
              -- but then we should make sure the solution is well-typed
                return Nothing
              else go (i - 1) (IntMap.insert (unIx jIndex) (Ix i) revMap) restArgs
          _ -> return Nothing
      -- Not a pattern so return nothing.
      _ -> return Nothing
