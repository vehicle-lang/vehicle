{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}
module Vehicle.Compile.Type.Monad.Class
  ( MonadTypeChecker (..),
    TypeCheckerState (..),
    emptyTypeCheckerState,
    substMetas,
    freshExprMeta,
    freshPolarityMeta,
    freshLinearityMeta,
    freshTypeClassPlacementMeta,
    getMetaIndex,
    solveMeta,
    filterMetasByTypes,
    getUnsolvedAuxiliaryMetas,
    getMetasLinkedToMetasIn,
    getAndClearRecentlySolvedMetas,
    clearMetaSubstitution,
    incrementMetaCtxSize,
    removeMetaDependencies,
    getMetaProvenance,
    getMetaType,
    getSubstMetaTypes,
    getMetaCtxSize,
    prettyMetas,
    prettyMeta,
    clearMetaCtx,
    getDeclType,
    getMetaSubstitution,
    getDeclSubstitution,
    getUnsolvedMetas,
    getBinderNameOrFreshName,
    whnf,
    whnfNBE,
    glueNBE,
    forceHead,
    -- Constraints
    createFreshUnificationConstraint,
    createFreshTypeClassConstraint,
    getActiveConstraints,
    getActiveUnificationConstraints,
    getActiveTypeClassConstraints,
    addConstraints,
    addUnificationConstraints,
    addTypeClassConstraints,
    setConstraints,
    setUnificationConstraints,
    setTypeClassConstraints,
  )
where

import Control.Monad (foldM, unless)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.Map qualified as Map
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise (NormalisationOptions (..), normaliseExpr)
import Vehicle.Compile.Normalise.NBE (forceExpr)
import Vehicle.Compile.Normalise.NBE qualified as NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
  ( HasMetas (..),
    MetaCtxSize,
    MetaInfo (..),
    increaseMetaCtxSize,
    makeMetaExpr,
  )
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitutable,
    MetaSubstitution,
    substituteMetas,
  )
import Vehicle.Compile.Type.VariableContext
  ( DeclSubstitution,
    TypingBoundCtx,
    TypingDeclCtx,
    toNormalisationDeclContext,
  )
import Vehicle.Expr.DeBruijn (DBLevel (..))
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- The overall meta variable context

-- | State for generating fresh names.
type FreshNameState = Int

-- | The meta-variables and constraints relating the variables currently in scope.
data TypeCheckerState = TypeCheckerState
  { -- | The origin and type of each meta variable.
    -- NB: these are stored in *reverse* order from which they were created.
    metaInfo :: [MetaInfo],
    currentSubstitution :: MetaSubstitution,
    unificationConstraints :: [WithContext UnificationConstraint],
    typeClassConstraints :: [WithContext TypeClassConstraint],
    freshNameState :: FreshNameState,
    recentlySolvedMetas :: MetaSet
  }

emptyTypeCheckerState :: TypeCheckerState
emptyTypeCheckerState =
  TypeCheckerState
    { metaInfo = mempty,
      currentSubstitution = mempty,
      unificationConstraints = mempty,
      typeClassConstraints = mempty,
      freshNameState = 0,
      recentlySolvedMetas = mempty
    }

--------------------------------------------------------------------------------
-- The type-checking monad class

-- | The type-checking monad.
class MonadCompile m => MonadTypeChecker m where
  getDeclContext :: m TypingDeclCtx
  addDeclContext :: TypedDecl -> m a -> m a
  getMetaCtx :: m TypeCheckerState
  modifyMetaCtx :: (TypeCheckerState -> TypeCheckerState) -> m ()
  getFreshName :: CheckedType -> m Name
  clearFreshNames :: m ()

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (WriterT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapWriterT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift clearFreshNames

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (ReaderT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapReaderT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift clearFreshNames

instance MonadTypeChecker m => MonadTypeChecker (StateT s m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapStateT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift clearFreshNames

--------------------------------------------------------------------------------
-- Operations

getsMetaCtx :: MonadTypeChecker m => (TypeCheckerState -> a) -> m a
getsMetaCtx f = f <$> getMetaCtx

getNumberOfMetasCreated :: MonadTypeChecker m => m Int
getNumberOfMetasCreated = getsMetaCtx (length . metaInfo)

getMetaSubstitution :: MonadTypeChecker m => m MetaSubstitution
getMetaSubstitution = getsMetaCtx currentSubstitution

-- | Returns the list of metas that have been solved since the last
-- call to this method.
getAndClearRecentlySolvedMetas :: MonadTypeChecker m => m MetaSet
getAndClearRecentlySolvedMetas = do
  result <- getsMetaCtx recentlySolvedMetas
  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {recentlySolvedMetas = mempty, ..}
  return result

getUnsolvedMetas :: MonadTypeChecker m => m MetaSet
getUnsolvedMetas = do
  metasSolved <- MetaMap.keys <$> getMetaSubstitution
  numberOfMetasCreated <- getNumberOfMetasCreated
  let metasCreated = MetaSet.fromList $ fmap MetaID [0 .. numberOfMetasCreated - 1]
  return $ MetaSet.difference metasCreated metasSolved

getDeclSubstitution :: MonadTypeChecker m => m DeclSubstitution
getDeclSubstitution = Map.mapMaybe (fmap normalised . snd) <$> getDeclContext

{-
-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadTypeChecker m => MetaSet -> m [WithContext Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getUnsolvedConstraints
  let (blockedConstraints, unblockedConstraints) = partition (constraintIsBlocked metasSolved) allConstraints
  setConstraints blockedConstraints
  return unblockedConstraints
-}
--------------------------------------------------------------------------------
-- Meta subsitution

substMetas :: (MonadTypeChecker m, MetaSubstitutable a) => a -> m a
substMetas e = do
  metaSubst <- getMetaSubstitution
  declCtx <- getDeclSubstitution
  substituteMetas declCtx metaSubst e

--------------------------------------------------------------------------------
-- Meta-variable creation

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMeta ::
  MonadTypeChecker m =>
  Provenance ->
  CheckedType ->
  MetaCtxSize ->
  m (MetaID, GluedExpr)
freshMeta p metaType metaCtxSize = do
  -- Create a fresh id for the meta
  TypeCheckerState {..} <- getMetaCtx
  let nextMetaID = length metaInfo
  let metaID = MetaID nextMetaID

  -- Construct the information about the meta-variable
  let info = MetaInfo p metaType metaCtxSize

  -- Update the meta context
  modifyMetaCtx $ const $ TypeCheckerState {metaInfo = info : metaInfo, ..}

  -- Create the expression
  let metaExpr = makeMetaExpr p metaID metaCtxSize

  logDebug MaxDetail $ "fresh-meta" <+> pretty metaID <+> ":" <+> prettyVerbose metaType
  return (metaID, metaExpr)

-- | Ensures the meta has no dependencies on the bound context. Returns true
-- if dependencies were removed to achieve this.
removeMetaDependencies :: MonadTypeChecker m => MetaID -> m Bool
removeMetaDependencies m = do
  MetaInfo p t ctxSize <- getMetaInfo m
  if ctxSize == 0
    then return False
    else do
      newMeta <- freshExprMeta p t 0
      solveMeta m (unnormalised newMeta) (DBLevel ctxSize)
      return True

freshExprMeta ::
  MonadTypeChecker m =>
  Provenance ->
  CheckedType ->
  Int ->
  m GluedExpr
freshExprMeta p t boundCtxSize = snd <$> freshMeta p t boundCtxSize

freshPolarityMeta :: MonadTypeChecker m => Provenance -> m GluedExpr
freshPolarityMeta p = snd <$> freshMeta p (PolarityUniverse p) 0

freshLinearityMeta :: MonadTypeChecker m => Provenance -> m GluedExpr
freshLinearityMeta p = snd <$> freshMeta p (LinearityUniverse p) 0

freshTypeClassPlacementMeta ::
  MonadTypeChecker m =>
  Provenance ->
  CheckedType ->
  Int ->
  m (MetaID, GluedExpr)
freshTypeClassPlacementMeta = freshMeta

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaInfo :: MonadTypeChecker m => MetaID -> m MetaInfo
getMetaInfo m = do
  TypeCheckerState {..} <- getMetaCtx
  case metaInfo !!? getMetaIndex metaInfo m of
    Just info -> return info
    Nothing ->
      compilerDeveloperError $
        "Requesting info for unknown meta" <+> pretty m <+> "not in context"

getMetaIndex :: [MetaInfo] -> MetaID -> Int
getMetaIndex metaInfo (MetaID m) = length metaInfo - m - 1

getMetaProvenance :: MonadTypeChecker m => MetaID -> m Provenance
getMetaProvenance m = metaProvenance <$> getMetaInfo m

getMetaType :: MonadTypeChecker m => MetaID -> m CheckedType
getMetaType m = metaType <$> getMetaInfo m

getSubstMetaType :: MonadTypeChecker m => MetaID -> m CheckedType
getSubstMetaType m = substMetas =<< getMetaType m

getMetaCtxSize :: MonadTypeChecker m => MetaID -> m MetaCtxSize
getMetaCtxSize m = metaCtxSize <$> getMetaInfo m

incrementMetaCtxSize :: MonadTypeChecker m => MetaID -> m ()
incrementMetaCtxSize m =
  modifyMetaCtx
    ( \TypeCheckerState {..} -> do
        let metaIndex = getMetaIndex metaInfo m
        case splitAt metaIndex metaInfo of
          (_, []) ->
            developerError $
              "Increment meta-ctx for unknown meta-variable" <+> pretty m
          (xs, info : ys) -> do
            let info' = increaseMetaCtxSize info
            TypeCheckerState
              { metaInfo = xs <> (info' : ys),
                ..
              }
    )

clearMetaSubstitution :: MonadTypeChecker m => m ()
clearMetaSubstitution = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {currentSubstitution = mempty, ..}

getUnsolvedAuxiliaryMetas :: MonadTypeChecker m => m MetaSet
getUnsolvedAuxiliaryMetas = filterMetasByTypes isAuxiliaryUniverse =<< getUnsolvedMetas

getSubstMetaTypes :: MonadTypeChecker m => MetaSet -> m [(MetaID, CheckedType)]
getSubstMetaTypes metas = traverse (\m -> (m,) <$> getSubstMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn ::
  forall m.
  MonadTypeChecker m =>
  [WithContext Constraint] ->
  (CheckedType -> Bool) ->
  CheckedType ->
  m MetaSet
getMetasLinkedToMetasIn allConstraints typeFilter typeOfInterest = do
  let constraints = fmap objectIn allConstraints
  metasInType <- metasIn typeOfInterest
  directMetasInType <- filterMetasByTypes typeFilter metasInType
  loopOverConstraints constraints directMetasInType
  where
    loopOverConstraints :: [Constraint] -> MetaSet -> m MetaSet
    loopOverConstraints constraints metas = do
      (unrelatedConstraints, newMetas) <- foldM processConstraint ([], metas) constraints
      if metas /= newMetas
        then loopOverConstraints unrelatedConstraints newMetas
        else return metas

    processConstraint ::
      ([Constraint], MetaSet) ->
      Constraint ->
      m ([Constraint], MetaSet)
    processConstraint (nonRelatedConstraints, typeMetas) constraint = do
      allConstraintMetas <- metasIn constraint
      constraintMetas <- filterMetasByTypes typeFilter allConstraintMetas
      return $
        if MetaSet.disjoint constraintMetas typeMetas
          then (constraint : nonRelatedConstraints, typeMetas)
          else (nonRelatedConstraints, MetaSet.unions [constraintMetas, typeMetas])

filterMetasByTypes :: MonadTypeChecker m => (CheckedType -> Bool) -> MetaSet -> m MetaSet
filterMetasByTypes typeFilter metas = do
  typedMetas <- getSubstMetaTypes metas
  let filteredMetas = filter (typeFilter . snd) typedMetas
  return $ MetaSet.fromList (fmap fst filteredMetas)

abstractOverCtx :: Int -> CheckedExpr -> CheckedExpr
abstractOverCtx ctxSize body = do
  let p = mempty
  let lamBinderForm i = BinderForm (OnlyName (layoutAsText $ "x" <> pretty i)) True
  let lam i = Lam p (Binder p (lamBinderForm i) Explicit Relevant () (TypeUniverse p 0))
  foldr lam body ([0 .. ctxSize - 1] :: [Int])

solveMeta :: MonadTypeChecker m => MetaID -> CheckedExpr -> DBLevel -> m ()
solveMeta m solution currentLevel = do
  MetaInfo p _ ctxSize <- getMetaInfo m
  let abstractedSolution = abstractOverCtx ctxSize solution
  gluedSolution <- glueNBE currentLevel abstractedSolution

  logDebug MaxDetail $ "solved" <+> pretty m <+> "as" <+> prettyVerbose abstractedSolution

  metaSubst <- getMetaSubstitution
  case MetaMap.lookup m metaSubst of
    Just existing ->
      compilerDeveloperError $
        "meta-variable"
          <+> pretty m
          <+> "already solved as"
          <+> line
            <> indent 2 (squotes (prettyVerbose (unnormalised existing)))
            <> line
            <> "but is being re-solved as"
          <+> line
            <> indent 2 (squotes (prettyVerbose solution))
            <> line
            <> "at"
          <+> pretty p
    -- Could use `insertWith` instead of `insert` here for one lookup instead of
    -- two, but not possible to throw a monadic error unfortunately.
    Nothing -> do
      modifyMetaCtx $ \TypeCheckerState {..} ->
        TypeCheckerState
          { currentSubstitution = MetaMap.insert m gluedSolution currentSubstitution,
            recentlySolvedMetas = MetaSet.insert m recentlySolvedMetas,
            ..
          }

prettyMetas :: MonadTypeChecker m => MetaSet -> m (Doc a)
prettyMetas metas = do
  typedMetaList <- getSubstMetaTypes metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: MonadTypeChecker m => MetaID -> m (Doc a)
prettyMeta meta = prettyMetaInternal meta <$> getMetaType meta

prettyMetaInternal :: MetaID -> CheckedType -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: MonadTypeChecker m => m ()
clearMetaCtx = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyMetaCtx (const emptyTypeCheckerState)

getDeclType :: MonadTypeChecker m => Provenance -> Identifier -> m CheckedType
getDeclType p ident = do
  ctx <- getDeclContext
  case Map.lookup ident ctx of
    Just (checkedType, _) -> return checkedType
    -- This should have been caught during scope checking
    Nothing ->
      compilerDeveloperError $
        "Declaration'"
          <+> pretty ident
          <+> "'not found when"
          <+> "looking up variable in context"
          <+> pretty (Map.keys ctx)
          <+> "at"
          <+> pretty p

--------------------------------------------------------------------------------
-- Constraints

getActiveConstraints :: MonadTypeChecker m => m [WithContext Constraint]
getActiveConstraints = do
  us <- fmap (mapObject UnificationConstraint) <$> getActiveUnificationConstraints
  ts <- fmap (mapObject TypeClassConstraint) <$> getActiveTypeClassConstraints
  return $ us <> ts

getActiveUnificationConstraints :: MonadTypeChecker m => m [WithContext UnificationConstraint]
getActiveUnificationConstraints = getsMetaCtx unificationConstraints

getActiveTypeClassConstraints :: MonadTypeChecker m => m [WithContext TypeClassConstraint]
getActiveTypeClassConstraints = getsMetaCtx typeClassConstraints

setConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
setConstraints constraints = do
  let (us, ts) = separateConstraints constraints
  setUnificationConstraints us
  setTypeClassConstraints ts

setTypeClassConstraints :: MonadTypeChecker m => [WithContext TypeClassConstraint] -> m ()
setTypeClassConstraints newConstraints = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {typeClassConstraints = newConstraints, ..}

setUnificationConstraints :: MonadTypeChecker m => [WithContext UnificationConstraint] -> m ()
setUnificationConstraints newConstraints = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {unificationConstraints = newConstraints, ..}

addConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
addConstraints constraints = do
  let (us, ts) = separateConstraints constraints
  addUnificationConstraints us
  addTypeClassConstraints ts

addUnificationConstraints :: MonadTypeChecker m => [WithContext UnificationConstraint] -> m ()
addUnificationConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyFriendly constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {unificationConstraints = unificationConstraints ++ constraints, ..}

addTypeClassConstraints :: MonadTypeChecker m => [WithContext TypeClassConstraint] -> m ()
addTypeClassConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyFriendly constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {typeClassConstraints = typeClassConstraints ++ constraints, ..}

-- | Adds an entirely new unification constraint (as opposed to one
-- derived from another constraint).
createFreshUnificationConstraint ::
  MonadTypeChecker m =>
  ConstraintGroup ->
  Provenance ->
  TypingBoundCtx ->
  ConstraintOrigin ->
  CheckedType ->
  CheckedType ->
  m ()
createFreshUnificationConstraint group p ctx origin expectedType actualType = do
  let currentLevel = DBLevel $ length ctx
  normExpectedType <- whnfNBE currentLevel expectedType
  normActualType <- whnfNBE currentLevel actualType
  let context = ConstraintContext p origin p unknownBlockingStatus ctx group
  let unification = Unify normExpectedType normActualType
  let constraint = WithContext unification context

  addUnificationConstraints [constraint]

-- | Adds an entirely new type-class constraint (as opposed to one
-- derived from another constraint).
createFreshTypeClassConstraint ::
  MonadTypeChecker m =>
  TypingBoundCtx ->
  CheckedExpr ->
  [CheckedArg] ->
  CheckedType ->
  m CheckedExpr
createFreshTypeClassConstraint ctx fun funArgs tcExpr = do
  (tc, args) <- case tcExpr of
    BuiltinTypeClass _ tc args -> return (tc, args)
    _ ->
      compilerDeveloperError $
        "Malformed type class constraint" <+> prettyVerbose tcExpr

  let ctxSize = length ctx
  nArgs <- traverse (traverse (whnfNBE (DBLevel ctxSize))) args

  let p = provenanceOf fun
  (meta, metaExpr) <- freshTypeClassPlacementMeta p tcExpr ctxSize

  let originProvenance = provenanceOf tcExpr
  let group = typeClassGroup tc
  let origin = CheckingTypeClass fun funArgs tc
  let context = ConstraintContext originProvenance origin p unknownBlockingStatus ctx group
  let constraint = WithContext (Has meta tc nArgs) context

  addTypeClassConstraints [constraint]

  return $ unnormalised metaExpr

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead :: forall m. MonadTypeChecker m => NormExpr -> m (NormExpr, MetaSet)
forceHead expr = do
  declSubst <- getDeclSubstitution
  metaSubst <- getMetaSubstitution
  (maybeForcedExpr, blockingMetas) <- runReaderT (forceExpr expr) (declSubst, metaSubst)
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      logDebug MaxDetail $ "forced" <+> prettyVerbose expr <+> "to" <+> prettyVerbose forcedExpr
      return forcedExpr
  return (forcedExpr, blockingMetas)

whnf :: MonadTypeChecker m => CheckedExpr -> m CheckedExpr
whnf e = do
  declCtx <- getDeclContext
  runSilentLoggerT $
    normaliseExpr
      e
      Options
        { declContext = toNormalisationDeclContext declCtx,
          boundContext = mempty, -- see issue #129
          normaliseDeclApplications = True,
          normaliseLambdaApplications = True,
          normaliseStdLibApplications = True,
          normaliseBuiltin = const True
        }

getBinderNameOrFreshName :: MonadTypeChecker m => Maybe Name -> CheckedType -> m Name
getBinderNameOrFreshName piName typ = case piName of
  Just x -> return x
  Nothing -> getFreshName typ

whnfNBE :: MonadTypeChecker m => DBLevel -> CheckedExpr -> m NormExpr
whnfNBE boundCtxSize e = do
  declSubst <- getDeclSubstitution
  metaSubst <- getMetaSubstitution
  NBE.whnf boundCtxSize declSubst metaSubst e

glueNBE :: MonadTypeChecker m => DBLevel -> CheckedExpr -> m GluedExpr
glueNBE boundCtxSize e = Glued e <$> whnfNBE boundCtxSize e
