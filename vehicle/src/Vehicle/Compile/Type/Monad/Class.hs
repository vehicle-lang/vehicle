{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}
module Vehicle.Compile.Type.Monad.Class
  ( MonadTypeChecker (..),
    TypeCheckerState (..),
    emptyTypeCheckerState,
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
    getMetaCtx,
    prettyMetas,
    prettyMeta,
    clearMetaCtx,
    getDeclType,
    getUnsolvedMetas,
    getBinderNameOrFreshName,
    glueNBE,
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
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin, prettyExternal, prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
  ( HasMetas (..),
    MetaInfo (..),
    extendMetaCtx,
    makeMetaExpr,
  )
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution
import Vehicle.Compile.Type.VariableContext
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
data TypeCheckerState builtin = TypeCheckerState
  { -- | The origin and type of each meta variable.
    -- NB: these are stored in *reverse* order from which they were created.
    metaInfo :: [MetaInfo builtin],
    currentSubstitution :: MetaSubstitution builtin,
    unificationConstraints :: [WithContext (UnificationConstraint builtin)],
    typeClassConstraints :: [WithContext (TypeClassConstraint builtin)],
    freshNameState :: FreshNameState,
    solvedMetaState :: SolvedMetaState
  }

emptyTypeCheckerState :: TypeCheckerState builtin
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
class (MonadCompile m, MonadNorm builtin m) => MonadTypeChecker builtin m where
  getDeclContext :: m (TypingDeclCtx builtin)
  addDeclContext :: TypedDecl builtin -> m a -> m a
  getMetaState :: m (TypeCheckerState builtin)
  modifyMetaCtx :: (TypeCheckerState builtin -> TypeCheckerState builtin) -> m ()
  getFreshName :: CheckedType builtin -> m Name
  clearFreshNames :: Proxy builtin -> m ()

instance (Monoid w, MonadTypeChecker builtin m) => MonadTypeChecker builtin (WriterT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapWriterT (addDeclContext d)
  getMetaState = lift getMetaState
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames

instance (Monoid w, MonadTypeChecker builtin m) => MonadTypeChecker builtin (ReaderT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapReaderT (addDeclContext d)
  getMetaState = lift getMetaState
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames

instance MonadTypeChecker builtin m => MonadTypeChecker builtin (StateT s m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapStateT (addDeclContext d)
  getMetaState = lift getMetaState
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames

--------------------------------------------------------------------------------
-- Operations

getsMetaCtx :: MonadTypeChecker builtin m => (TypeCheckerState builtin -> a) -> m a
getsMetaCtx f = f <$> getMetaState

getNumberOfMetasCreated :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> m Int
getNumberOfMetasCreated _ = getsMetaCtx @builtin (length . metaInfo)

-- | Track the metas solved while performing the provided computation.
-- Multiple calls can be nested arbitrarily deepily.
trackSolvedMetas :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> m () -> m MetaSet
trackSolvedMetas _ performComputation = do
  modifySolvedMetaState enterSolvedMetaTrackingRegion

  performComputation

  solvedMetas <- getsMetaCtx @builtin (getMostRecentlySolvedMetas . solvedMetaState)
  modifySolvedMetaState exitSolvedMetaTrackingRegion

  return solvedMetas
  where
    modifySolvedMetaState :: (SolvedMetaState -> SolvedMetaState) -> m ()
    modifySolvedMetaState f = modifyMetaCtx @builtin $ \TypeCheckerState {..} ->
      TypeCheckerState
        { solvedMetaState = f solvedMetaState,
          ..
        }

getUnsolvedMetas :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> m MetaSet
getUnsolvedMetas proxy = do
  metasSolved <- MetaMap.keys <$> getMetaSubstitution @builtin
  numberOfMetasCreated <- getNumberOfMetasCreated proxy
  let metasCreated = MetaSet.fromList $ fmap MetaID [0 .. numberOfMetasCreated - 1]
  return $ MetaSet.difference metasCreated metasSolved

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
  MonadTypeChecker builtin m =>
  Provenance ->
  CheckedType builtin ->
  TypingBoundCtx builtin ->
  m (MetaID, GluedExpr builtin)
freshMeta p metaType boundCtx = do
  -- Create a fresh id for the meta
  TypeCheckerState {..} <- getMetaState
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
removeMetaDependencies :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> MetaID -> m Bool
removeMetaDependencies _ m = do
  MetaInfo p t ctx <- getMetaInfo @builtin m
  if null ctx
    then return False
    else do
      newMeta <- freshExprMeta p t mempty
      solveMeta m (unnormalised newMeta) mempty
      return True

freshExprMeta ::
  MonadTypeChecker builtin m =>
  Provenance ->
  CheckedType builtin ->
  TypingBoundCtx builtin ->
  m (GluedExpr builtin)
freshExprMeta p t boundCtx = snd <$> freshMeta p t boundCtx

freshPolarityMeta :: MonadTypeChecker builtin m => Provenance -> m (GluedExpr builtin)
freshPolarityMeta p = snd <$> freshMeta p (PolarityUniverse p) mempty

freshLinearityMeta :: MonadTypeChecker builtin m => Provenance -> m (GluedExpr builtin)
freshLinearityMeta p = snd <$> freshMeta p (LinearityUniverse p) mempty

freshTypeClassPlacementMeta ::
  MonadTypeChecker builtin m =>
  Provenance ->
  CheckedType builtin ->
  TypingBoundCtx builtin ->
  m (MetaID, GluedExpr builtin)
freshTypeClassPlacementMeta = freshMeta

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaInfo :: MonadTypeChecker builtin m => MetaID -> m (MetaInfo builtin)
getMetaInfo m = do
  TypeCheckerState {..} <- getMetaState
  case metaInfo !!? getMetaIndex metaInfo m of
    Just info -> return info
    Nothing ->
      compilerDeveloperError $
        "Requesting info for unknown meta" <+> pretty m <+> "not in context"

getMetaIndex :: [MetaInfo builtin] -> MetaID -> Int
getMetaIndex metaInfo (MetaID m) = length metaInfo - m - 1

getMetaProvenance :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> MetaID -> m Provenance
getMetaProvenance _ m = metaProvenance <$> getMetaInfo @builtin m

getMetaType :: MonadTypeChecker builtin m => MetaID -> m (CheckedType builtin)
getMetaType m = metaType <$> getMetaInfo m

getSubstMetaType :: MonadTypeChecker builtin m => MetaID -> m (CheckedType builtin)
getSubstMetaType m = substMetas =<< getMetaType m

-- | Get the bound context the meta-variable was created in.
getMetaCtx :: MonadTypeChecker builtin m => MetaID -> m (TypingBoundCtx builtin)
getMetaCtx m = metaCtx <$> getMetaInfo m

extendBoundCtxOfMeta :: MonadTypeChecker builtin m => MetaID -> CheckedBinder builtin -> m ()
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

clearMetaSubstitution :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> m ()
clearMetaSubstitution _ = modifyMetaCtx @builtin $ \TypeCheckerState {..} ->
  TypeCheckerState {currentSubstitution = mempty, ..}

getUnsolvedAuxiliaryMetas :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> m MetaSet
getUnsolvedAuxiliaryMetas _ =
  filterMetasByTypes @builtin isAuxiliaryUniverse =<< getUnsolvedMetas (Proxy @builtin)

getSubstMetaTypes :: MonadTypeChecker builtin m => MetaSet -> m [(MetaID, CheckedType builtin)]
getSubstMetaTypes metas = traverse (\m -> (m,) <$> getSubstMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn ::
  forall builtin m.
  MonadTypeChecker builtin m =>
  [WithContext (Constraint builtin)] ->
  (CheckedType builtin -> Bool) ->
  CheckedType builtin ->
  m MetaSet
getMetasLinkedToMetasIn allConstraints typeFilter typeOfInterest = do
  let constraints = fmap objectIn allConstraints
  metasInType <- metasIn typeOfInterest
  directMetasInType <- filterMetasByTypes typeFilter metasInType
  loopOverConstraints constraints directMetasInType
  where
    loopOverConstraints :: [Constraint builtin] -> MetaSet -> m MetaSet
    loopOverConstraints constraints metas = do
      (unrelatedConstraints, newMetas) <- foldM processConstraint ([], metas) constraints
      if metas /= newMetas
        then loopOverConstraints unrelatedConstraints newMetas
        else return metas

    processConstraint ::
      ([Constraint builtin], MetaSet) ->
      Constraint builtin ->
      m ([Constraint builtin], MetaSet)
    processConstraint (nonRelatedConstraints, typeMetas) constraint = do
      allConstraintMetas <- metasIn constraint
      constraintMetas <- filterMetasByTypes typeFilter allConstraintMetas
      return $
        if MetaSet.disjoint constraintMetas typeMetas
          then (constraint : nonRelatedConstraints, typeMetas)
          else (nonRelatedConstraints, MetaSet.unions [constraintMetas, typeMetas])

filterMetasByTypes :: MonadTypeChecker builtin m => (CheckedType builtin -> Bool) -> MetaSet -> m MetaSet
filterMetasByTypes typeFilter metas = do
  typedMetas <- getSubstMetaTypes metas
  let filteredMetas = filter (typeFilter . snd) typedMetas
  return $ MetaSet.fromList (fmap fst filteredMetas)

abstractOverCtx :: TypingBoundCtx builtin -> CheckedExpr builtin -> CheckedExpr builtin
abstractOverCtx ctx body = do
  let p = mempty
  let lamBinderForm (n, _, _) = BinderDisplayForm (OnlyName (fromMaybe "_" n)) True
  -- WARNING: in theory the type of this binder should be `t` but because these binders
  -- have temporary mutually recursive dependencies that are eliminated upon substitution
  -- then actualy using `t` here results in meta-substitution looping.
  let lam i@(_, _t, _) = Lam p (Binder p (lamBinderForm i) Explicit Relevant () (TypeUniverse p 0))
  foldr lam body (reverse ctx)

solveMeta :: forall builtin m. MonadTypeChecker builtin m => MetaID -> CheckedExpr builtin -> TypingBoundCtx builtin -> m ()
solveMeta m solution solutionCtx = do
  MetaInfo p _ metaCtx <- getMetaInfo m
  let abstractedSolution = abstractOverCtx metaCtx solution
  gluedSolution <- glueNBE (DBLevel $ length solutionCtx) abstractedSolution

  logDebug MaxDetail $
    "solved"
      <+> pretty m
      <+> "as"
      <+> prettyExternal (WithContext abstractedSolution (boundContextOf solutionCtx))
  -- "as" <+> prettyFriendly (WithContext abstractedSolution (boundContextOf ctx))

  metaSubst <- getMetaSubstitution @builtin
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

prettyMetas :: forall builtin m a. MonadTypeChecker builtin m => Proxy builtin -> MetaSet -> m (Doc a)
prettyMetas _ metas = do
  typedMetaList <- getSubstMetaTypes @builtin metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: forall builtin m a. MonadTypeChecker builtin m => Proxy builtin -> MetaID -> m (Doc a)
prettyMeta _ meta = prettyMetaInternal meta <$> getMetaType @builtin meta

prettyMetaInternal :: PrintableBuiltin builtin => MetaID -> CheckedType builtin -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: forall builtin m. MonadTypeChecker builtin m => Proxy builtin -> m ()
clearMetaCtx _ = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyMetaCtx @builtin (const emptyTypeCheckerState)

getDeclType :: MonadTypeChecker builtin m => Provenance -> Identifier -> m (CheckedType builtin)
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

getActiveConstraints :: MonadTypeChecker builtin m => m [WithContext (Constraint builtin)]
getActiveConstraints = do
  us <- fmap (mapObject UnificationConstraint) <$> getActiveUnificationConstraints
  ts <- fmap (mapObject TypeClassConstraint) <$> getActiveTypeClassConstraints
  return $ us <> ts

getActiveUnificationConstraints :: MonadTypeChecker builtin m => m [WithContext (UnificationConstraint builtin)]
getActiveUnificationConstraints = getsMetaCtx unificationConstraints

getActiveTypeClassConstraints :: MonadTypeChecker builtin m => m [WithContext (TypeClassConstraint builtin)]
getActiveTypeClassConstraints = getsMetaCtx typeClassConstraints

setConstraints :: MonadTypeChecker builtin m => [WithContext (Constraint builtin)] -> m ()
setConstraints constraints = do
  let (us, ts) = separateConstraints constraints
  setUnificationConstraints us
  setTypeClassConstraints ts

setTypeClassConstraints :: MonadTypeChecker builtin m => [WithContext (TypeClassConstraint builtin)] -> m ()
setTypeClassConstraints newConstraints = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {typeClassConstraints = newConstraints, ..}

setUnificationConstraints :: MonadTypeChecker builtin m => [WithContext (UnificationConstraint builtin)] -> m ()
setUnificationConstraints newConstraints = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {unificationConstraints = newConstraints, ..}

addConstraints :: MonadTypeChecker builtin m => [WithContext (Constraint builtin)] -> m ()
addConstraints constraints = do
  let (us, ts) = separateConstraints constraints
  addUnificationConstraints us
  addTypeClassConstraints ts

addUnificationConstraints :: MonadTypeChecker builtin m => [WithContext (UnificationConstraint builtin)] -> m ()
addUnificationConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyExternal constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {unificationConstraints = unificationConstraints ++ constraints, ..}

addTypeClassConstraints :: MonadTypeChecker builtin m => [WithContext (TypeClassConstraint builtin)] -> m ()
addTypeClassConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyExternal constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {typeClassConstraints = typeClassConstraints ++ constraints, ..}

-- | Adds an entirely new unification constraint (as opposed to one
-- derived from another constraint).
createFreshUnificationConstraint ::
  MonadTypeChecker builtin m =>
  Provenance ->
  TypingBoundCtx builtin ->
  ConstraintOrigin builtin ->
  CheckedType builtin ->
  CheckedType builtin ->
  m ()
createFreshUnificationConstraint p ctx origin expectedType actualType = do
  let currentLevel = DBLevel $ length ctx
  normExpectedType <- whnf currentLevel expectedType
  normActualType <- whnf currentLevel actualType
  let context = ConstraintContext p origin p unknownBlockingStatus ctx
  let unification = Unify normExpectedType normActualType
  let constraint = WithContext unification context

  addUnificationConstraints [constraint]

-- | Adds an entirely new type-class constraint (as opposed to one
-- derived from another constraint).
createFreshTypeClassConstraint ::
  MonadTypeChecker builtin m =>
  TypingBoundCtx builtin ->
  (CheckedExpr builtin, [CheckedArg builtin]) ->
  CheckedType builtin ->
  m (GluedExpr builtin)
createFreshTypeClassConstraint boundCtx (fun, funArgs) tcExpr = do
  (tc, args) <- case tcExpr of
    BuiltinExpr _ tc args -> return (tc, NonEmpty.toList args)
    _ ->
      compilerDeveloperError $
        "Malformed type class constraint" <+> prettyVerbose tcExpr

  let ctxLevel = DBLevel $ length boundCtx
  nArgs <- traverse (traverse (whnf ctxLevel)) args

  let p = provenanceOf fun
  (meta, metaExpr) <- freshTypeClassPlacementMeta p tcExpr boundCtx

  let origin = CheckingTypeClass fun funArgs
  let originProvenance = provenanceOf tcExpr
  let context = ConstraintContext originProvenance origin p unknownBlockingStatus boundCtx
  let constraint = WithContext (Has meta tc nArgs) context

  addTypeClassConstraints [constraint]

  return metaExpr

instantiateArgForNonExplicitBinder ::
  MonadTypeChecker builtin m =>
  TypingBoundCtx builtin ->
  Provenance ->
  (CheckedExpr builtin, [CheckedArg builtin]) ->
  CheckedBinder builtin ->
  m (GluedArg builtin)
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

getBinderNameOrFreshName :: MonadTypeChecker builtin m => Maybe Name -> CheckedType builtin -> m Name
getBinderNameOrFreshName piName typ = case piName of
  Just x -> return x
  Nothing -> getFreshName typ

glueNBE :: MonadNorm builtin m => DBLevel -> CheckedExpr builtin -> m (GluedExpr builtin)
glueNBE boundCtxSize e = Glued e <$> whnf boundCtxSize e
