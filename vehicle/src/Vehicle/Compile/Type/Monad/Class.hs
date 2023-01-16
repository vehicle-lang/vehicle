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
    clearMetaSubstitution,
    extendBoundCtxOfMeta,
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
    trackSolvedMetas,
    -- Constraints
    instantiateArgForNonExplicitBinder,
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
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise (NormalisationOptions (..), normaliseExpr)
import Vehicle.Compile.Normalise.NBE (forceExpr)
import Vehicle.Compile.Normalise.NBE qualified as NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
  ( HasMetas (..),
    MetaCtxSize,
    MetaInfo (..),
    extendMetaCtx,
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
-- Solved meta-state

-- | Tracks meta-variables that have been solved in certain regions of the code.
-- Each element in the list represent one nested tracking region, with the
-- head of the list representing the most recent.
newtype SolvedMetaState = SolvedMetaState [MetaSet]

enterSolvedMetaTrackingRegion :: SolvedMetaState -> SolvedMetaState
enterSolvedMetaTrackingRegion (SolvedMetaState state) =
  SolvedMetaState (mempty : state)

registerSolvedMeta :: MetaID -> SolvedMetaState -> SolvedMetaState
registerSolvedMeta m (SolvedMetaState state) = SolvedMetaState $
  case state of
    [] -> []
    l : ls -> MetaSet.insert m l : ls

getMostRecentlySolvedMetas :: SolvedMetaState -> MetaSet
getMostRecentlySolvedMetas (SolvedMetaState state) =
  fromMaybe mempty (state !!? 0)

exitSolvedMetaTrackingRegion :: SolvedMetaState -> SolvedMetaState
exitSolvedMetaTrackingRegion (SolvedMetaState state) = SolvedMetaState $
  case state of
    [] -> []
    [_] -> []
    l1 : l2 : ls -> l1 <> l2 : ls

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
    solvedMetaState :: SolvedMetaState
  }

emptyTypeCheckerState :: TypeCheckerState
emptyTypeCheckerState =
  TypeCheckerState
    { metaInfo = mempty,
      currentSubstitution = mempty,
      unificationConstraints = mempty,
      typeClassConstraints = mempty,
      freshNameState = 0,
      solvedMetaState = SolvedMetaState mempty
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

modifySolvedMetaState :: MonadTypeChecker m => (SolvedMetaState -> SolvedMetaState) -> m ()
modifySolvedMetaState f = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState
    { solvedMetaState = f solvedMetaState,
      ..
    }

-- | Track the metas solved while performing the provided computation.
-- Multiple calls can be nested arbitrarily deepily.
trackSolvedMetas :: MonadTypeChecker m => m () -> m MetaSet
trackSolvedMetas performComputation = do
  modifySolvedMetaState enterSolvedMetaTrackingRegion

  performComputation

  solvedMetas <- getsMetaCtx (getMostRecentlySolvedMetas . solvedMetaState)
  modifySolvedMetaState exitSolvedMetaTrackingRegion

  return solvedMetas

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
  TypingBoundCtx ->
  m (MetaID, GluedExpr)
freshMeta p metaType boundCtx = do
  -- Create a fresh id for the meta
  TypeCheckerState {..} <- getMetaCtx
  let nextMetaID = length metaInfo
  let metaID = MetaID nextMetaID

  -- Construct the information about the meta-variable
  let info = MetaInfo p metaType boundCtx

  -- Update the meta context
  modifyMetaCtx $ const $ TypeCheckerState {metaInfo = info : metaInfo, ..}

  -- Create the expression
  let metaExpr = makeMetaExpr p metaID boundCtx

  logDebug MaxDetail $ "fresh-meta" <+> pretty metaID <+> ":" <+> prettyVerbose metaType
  return (metaID, metaExpr)

-- | Ensures the meta has no dependencies on the bound context. Returns true
-- if dependencies were removed to achieve this.
removeMetaDependencies :: MonadTypeChecker m => MetaID -> m Bool
removeMetaDependencies m = do
  MetaInfo p t ctx <- getMetaInfo m
  if null ctx
    then return False
    else do
      newMeta <- freshExprMeta p t mempty
      solveMeta m (unnormalised newMeta) (DBLevel $ length ctx)
      return True

freshExprMeta ::
  MonadTypeChecker m =>
  Provenance ->
  CheckedType ->
  TypingBoundCtx ->
  m GluedExpr
freshExprMeta p t boundCtx = snd <$> freshMeta p t boundCtx

freshPolarityMeta :: MonadTypeChecker m => Provenance -> m GluedExpr
freshPolarityMeta p = snd <$> freshMeta p (PolarityUniverse p) mempty

freshLinearityMeta :: MonadTypeChecker m => Provenance -> m GluedExpr
freshLinearityMeta p = snd <$> freshMeta p (LinearityUniverse p) mempty

freshTypeClassPlacementMeta ::
  MonadTypeChecker m =>
  Provenance ->
  CheckedType ->
  TypingBoundCtx ->
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
getMetaCtxSize m = length . metaCtx <$> getMetaInfo m

extendBoundCtxOfMeta :: MonadTypeChecker m => MetaID -> CheckedBinder -> m ()
extendBoundCtxOfMeta m binder =
  modifyMetaCtx
    ( \TypeCheckerState {..} -> do
        let metaIndex = getMetaIndex metaInfo m
        case splitAt metaIndex metaInfo of
          (_, []) ->
            developerError $
              "Increment meta-ctx for unknown meta-variable" <+> pretty m
          (xs, info : ys) -> do
            let info' = extendMetaCtx binder info
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

abstractOverCtx :: TypingBoundCtx -> CheckedExpr -> CheckedExpr
abstractOverCtx ctx body = do
  let p = mempty
  let lamBinderForm (n, _, _) = BinderDisplayForm (OnlyName (fromMaybe "_" n)) True
  -- WARNING: in theory the type of this binder should be `t` but because these binders
  -- have temporary mutually recursive dependencies that are eliminated upon substitution
  -- then actualy using `t` here results in meta-substitution looping.
  let lam i@(_, _t, _) = Lam p (Binder p (lamBinderForm i) Explicit Relevant () (TypeUniverse p 0))
  foldr lam body (reverse ctx)

solveMeta :: MonadTypeChecker m => MetaID -> CheckedExpr -> DBLevel -> m ()
solveMeta m solution currentLevel = do
  MetaInfo p _ ctx <- getMetaInfo m
  let abstractedSolution = abstractOverCtx ctx solution
  gluedSolution <- glueNBE currentLevel abstractedSolution

  logDebug MaxDetail $
    "solved"
      <+> pretty m
      <+> "as"
      <+> prettyExternal (WithContext abstractedSolution (boundContextOf ctx))
  -- "as" <+> prettyFriendly (WithContext abstractedSolution (boundContextOf ctx))

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
            solvedMetaState = registerSolvedMeta m solvedMetaState,
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
    logDebug MaxDetail ("add-constraints " <> align (prettyExternal constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {unificationConstraints = unificationConstraints ++ constraints, ..}

addTypeClassConstraints :: MonadTypeChecker m => [WithContext TypeClassConstraint] -> m ()
addTypeClassConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyExternal constraints))

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
  (CheckedExpr, [CheckedArg]) ->
  CheckedType ->
  m GluedExpr
createFreshTypeClassConstraint boundCtx (fun, funArgs) tcExpr = do
  (tc, args) <- case tcExpr of
    BuiltinTypeClass _ tc args -> return (tc, NonEmpty.toList args)
    _ ->
      compilerDeveloperError $
        "Malformed type class constraint" <+> prettyVerbose tcExpr

  let ctxLevel = DBLevel $ length boundCtx
  nArgs <- traverse (traverse (whnfNBE ctxLevel)) args

  let p = provenanceOf fun
  (meta, metaExpr) <- freshTypeClassPlacementMeta p tcExpr boundCtx

  let origin = CheckingTypeClass fun funArgs
  let originProvenance = provenanceOf tcExpr
  let group = typeClassGroup tc
  let context = ConstraintContext originProvenance origin p unknownBlockingStatus boundCtx group
  let constraint = WithContext (Has meta tc nArgs) context

  addTypeClassConstraints [constraint]

  return metaExpr

instantiateArgForNonExplicitBinder ::
  MonadTypeChecker m =>
  TypingBoundCtx ->
  Provenance ->
  (CheckedExpr, [CheckedArg]) ->
  CheckedBinder ->
  m GluedArg
instantiateArgForNonExplicitBinder boundCtx p origin binder = do
  let binderType = typeOf binder
  checkedExpr <- case visibilityOf binder of
    Explicit {} -> compilerDeveloperError "Should not be instantiating Arg for explicit Binder"
    Implicit {} -> freshExprMeta p binderType boundCtx
    Instance {} -> createFreshTypeClassConstraint boundCtx origin binderType
  return $ Arg p (markInserted $ visibilityOf binder) (relevanceOf binder) checkedExpr

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
