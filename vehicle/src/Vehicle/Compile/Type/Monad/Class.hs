module Vehicle.Compile.Type.Monad.Class
  ( MonadTypeChecker (..),
    TypingMetaCtx,
    emptyTypingMetaCtx,
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
    substConstraintMetas,
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
    getTypeClassConstraints,
    addConstraints,
    addFreshUnificationConstraint,
    addFreshTypeClassConstraint,
    setConstraints,
    getMetaSubstitution,
    getUnsolvedMetas,
    getUnsolvedConstraints,
    popActivatedConstraints,
    whnf,
    whnfNBE,
    glueNBE,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise (NormalisationOptions (..), normaliseExpr)
import Vehicle.Compile.Normalise.NBE qualified as NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
  ( HasMetas (..),
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
  ( TypingBoundCtx,
    TypingDeclCtx,
    toNBEDeclContext,
    toNormalisationDeclContext,
  )
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- The overall meta variable context

-- | The meta-variables and constraints relating the variables currently in scope.
data TypingMetaCtx = TypingMetaCtx
  { -- | The origin and type of each meta variable.
    -- NB: these are stored in *reverse* order from which they were created.
    metaInfo :: [MetaInfo],
    currentSubstitution :: MetaSubstitution,
    constraints :: [WithContext Constraint],
    recentlySolvedMetas :: MetaSet
  }

emptyTypingMetaCtx :: TypingMetaCtx
emptyTypingMetaCtx =
  TypingMetaCtx
    { metaInfo = mempty,
      currentSubstitution = mempty,
      constraints = mempty,
      recentlySolvedMetas = mempty
    }

--------------------------------------------------------------------------------
-- The type-checking monad class

-- | The type-checking monad.
class MonadCompile m => MonadTypeChecker m where
  getDeclContext :: m TypingDeclCtx
  addDeclContext :: TypedDecl -> m a -> m a
  getMetaCtx :: m TypingMetaCtx
  getsMetaCtx :: (TypingMetaCtx -> a) -> m a
  putMetaCtx :: TypingMetaCtx -> m ()
  modifyMetaCtx :: (TypingMetaCtx -> TypingMetaCtx) -> m ()

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (WriterT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapWriterT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (ReaderT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapReaderT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance MonadTypeChecker m => MonadTypeChecker (StateT s m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapStateT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

--------------------------------------------------------------------------------
-- Operations

getUnsolvedConstraints :: MonadTypeChecker m => m [WithContext Constraint]
getUnsolvedConstraints = getsMetaCtx constraints

getNumberOfMetasCreated :: MonadTypeChecker m => m Int
getNumberOfMetasCreated = getsMetaCtx (length . metaInfo)

getMetaSubstitution :: MonadTypeChecker m => m MetaSubstitution
getMetaSubstitution = getsMetaCtx currentSubstitution

-- | Returns the list of metas that have been solved since the last
-- call to this method.
getAndClearRecentlySolvedMetas :: MonadTypeChecker m => m MetaSet
getAndClearRecentlySolvedMetas = do
  result <- getsMetaCtx recentlySolvedMetas
  modifyMetaCtx $ \TypingMetaCtx {..} ->
    TypingMetaCtx {recentlySolvedMetas = mempty, ..}
  return result

getUnsolvedMetas :: MonadTypeChecker m => m MetaSet
getUnsolvedMetas = do
  metasSolved <- MetaMap.keys <$> getMetaSubstitution
  numberOfMetasCreated <- getNumberOfMetasCreated
  let metasCreated = MetaSet.fromList $ fmap MetaID [0 .. numberOfMetasCreated - 1]
  return $ MetaSet.difference metasCreated metasSolved

setConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \TypingMetaCtx {..} ->
  TypingMetaCtx {constraints = newConstraints, ..}

-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadTypeChecker m => MetaSet -> m [WithContext Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getUnsolvedConstraints
  let (blockedConstraints, unblockedConstraints) = partition (constraintIsBlocked metasSolved) allConstraints
  setConstraints blockedConstraints
  return unblockedConstraints

--------------------------------------------------------------------------------
-- Meta subsitution

substMetas :: (MonadTypeChecker m, MetaSubstitutable a) => a -> m a
substMetas e = do
  metaSubst <- getMetaSubstitution
  declCtx <- toNBEDeclContext <$> getDeclContext
  substituteMetas declCtx metaSubst e

substConstraintMetas ::
  MonadTypeChecker m =>
  WithContext Constraint ->
  m (WithContext Constraint)
substConstraintMetas (WithContext constraint context) = do
  newConstraint <- substMetas constraint
  return $ WithContext newConstraint context

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
  Int ->
  m (MetaID, GluedExpr)
freshMeta p metaType boundCtxSize = do
  -- Create a fresh id for the meta
  TypingMetaCtx {..} <- getMetaCtx
  let nextMetaID = length metaInfo
  let metaID = MetaID nextMetaID

  -- Construct the information about the meta-variable
  let info = MetaInfo p metaType boundCtxSize

  -- Update the meta context
  putMetaCtx $ TypingMetaCtx {metaInfo = info : metaInfo, ..}

  -- Create the expression
  let metaExpr = makeMetaExpr p metaID boundCtxSize

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
      solveMeta m (unnormalised newMeta) ctxSize
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
  TypingMetaCtx {..} <- getMetaCtx
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

getMetaCtxSize :: MonadTypeChecker m => MetaID -> m Int
getMetaCtxSize m = metaCtxSize <$> getMetaInfo m

incrementMetaCtxSize :: MonadTypeChecker m => MetaID -> m ()
incrementMetaCtxSize m =
  modifyMetaCtx
    ( \TypingMetaCtx {..} -> do
        let metaIndex = getMetaIndex metaInfo m
        case splitAt metaIndex metaInfo of
          (_, []) ->
            developerError $
              "Increment meta-ctx for unknown meta-variable" <+> pretty m
          (xs, info : ys) -> do
            let info' = increaseMetaCtxSize info
            TypingMetaCtx
              { metaInfo = xs <> (info' : ys),
                ..
              }
    )

clearMetaSubstitution :: MonadTypeChecker m => m ()
clearMetaSubstitution = modifyMetaCtx $ \TypingMetaCtx {..} ->
  TypingMetaCtx {currentSubstitution = mempty, ..}

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
  let lam _ = Lam p (Binder p (BinderForm OnlyName True) Explicit Relevant Nothing (TypeUniverse p 0))
  foldr lam body ([0 .. ctxSize - 1] :: [Int])

solveMeta :: MonadTypeChecker m => MetaID -> CheckedExpr -> Int -> m ()
solveMeta m solution currentCtxSize = do
  MetaInfo p _ ctxSize <- getMetaInfo m
  let abstractedSolution = abstractOverCtx ctxSize solution
  gluedSolution <- glueNBE currentCtxSize abstractedSolution

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
      modifyMetaCtx $ \TypingMetaCtx {..} ->
        TypingMetaCtx
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
  modifyMetaCtx (const emptyTypingMetaCtx)

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

-- | Adds an entirely new unification constraint (as opposed to one
-- derived from another constraint).
addFreshUnificationConstraint ::
  MonadTypeChecker m =>
  ConstraintGroup ->
  Provenance ->
  TypingBoundCtx ->
  ConstraintOrigin ->
  CheckedType ->
  CheckedType ->
  m ()
addFreshUnificationConstraint group p ctx origin expectedType actualType = do
  normExpectedType <- whnfNBE (length ctx) expectedType
  normActualType <- whnfNBE (length ctx) actualType
  let constraint = UnificationConstraint $ Unify normExpectedType normActualType
  let context = ConstraintContext p origin p unknownBlockingStatus ctx group
  addConstraints [WithContext constraint context]

-- | Adds an entirely new type-class constraint (as opposed to one
-- derived from another constraint).
addFreshTypeClassConstraint ::
  MonadTypeChecker m =>
  TypingBoundCtx ->
  CheckedExpr ->
  [CheckedArg] ->
  CheckedType ->
  m CheckedExpr
addFreshTypeClassConstraint ctx fun funArgs tcExpr = do
  (tc, args) <- case tcExpr of
    BuiltinTypeClass _ tc args -> return (tc, args)
    _ ->
      compilerDeveloperError $
        "Malformed type class constraint" <+> prettyVerbose tcExpr

  let ctxSize = length ctx
  nArgs <- traverse (traverse (whnfNBE ctxSize)) args

  let p = provenanceOf fun
  (meta, metaExpr) <- freshTypeClassPlacementMeta p tcExpr ctxSize

  let originProvenance = provenanceOf tcExpr

  let group = typeClassGroup tc
  let origin = CheckingTypeClass fun funArgs tc
  let constraint = TypeClassConstraint (Has meta tc nArgs)
  let context = ConstraintContext originProvenance origin p unknownBlockingStatus ctx group
  addConstraints [WithContext constraint context]

  return $ unnormalised metaExpr

getTypeClassConstraints :: MonadTypeChecker m => m [WithContext TypeClassConstraint]
getTypeClassConstraints = mapMaybe getTypeClassConstraint <$> getUnsolvedConstraints

addConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
addConstraints [] = return ()
addConstraints newConstraints = do
  logDebug MaxDetail ("add-constraints " <> align (prettyVerbose newConstraints))
  modifyMetaCtx $ \TypingMetaCtx {..} ->
    TypingMetaCtx {constraints = constraints ++ newConstraints, ..}

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

whnfNBE :: MonadTypeChecker m => Int -> CheckedExpr -> m NormExpr
whnfNBE boundCtxSize e = do
  declCtx <- getDeclContext
  NBE.whnf boundCtxSize (Map.mapMaybe snd declCtx) e

glueNBE :: MonadTypeChecker m => Int -> CheckedExpr -> m GluedExpr
glueNBE boundCtxSize e = Glued e <$> whnfNBE boundCtxSize e
