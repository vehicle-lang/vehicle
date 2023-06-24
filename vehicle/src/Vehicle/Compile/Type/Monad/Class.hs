module Vehicle.Compile.Type.Monad.Class where

import Control.Monad (foldM, unless)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyVerbose)
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
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable
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
data TypeCheckerState types = TypeCheckerState
  { -- | The origin and type of each meta variable.
    -- NB: these are stored in *reverse* order from which they were created.
    metaInfo :: [MetaInfo types],
    currentSubstitution :: MetaSubstitution types,
    unificationConstraints :: [WithContext (UnificationConstraint types)],
    typeClassConstraints :: [WithContext (TypeClassConstraint types)],
    freshNameState :: FreshNameState,
    solvedMetaState :: SolvedMetaState,
    nextConstraintID :: ConstraintID
  }

emptyTypeCheckerState :: TypeCheckerState types
emptyTypeCheckerState =
  TypeCheckerState
    { metaInfo = mempty,
      currentSubstitution = mempty,
      unificationConstraints = mempty,
      typeClassConstraints = mempty,
      freshNameState = 0,
      solvedMetaState = SolvedMetaState mempty,
      nextConstraintID = 0
    }

--------------------------------------------------------------------------------
-- The type-checking monad class

-- | The type-checking monad.
class (MonadCompile m, MonadNorm types m) => MonadTypeChecker types m where
  getDeclContext :: m (TypingDeclCtx types)
  addDeclContext :: GluedDecl types -> m a -> m a
  getMetaState :: m (TypeCheckerState types)
  modifyMetaCtx :: (TypeCheckerState types -> TypeCheckerState types) -> m ()
  getFreshName :: NormalisableType types -> m Name
  clearFreshNames :: Proxy types -> m ()

instance (Monoid w, MonadTypeChecker types m) => MonadTypeChecker types (WriterT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapWriterT (addDeclContext d)
  getMetaState = lift getMetaState
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames

instance (Monoid w, MonadTypeChecker types m) => MonadTypeChecker types (ReaderT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapReaderT (addDeclContext d)
  getMetaState = lift getMetaState
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames

instance (MonadTypeChecker types m) => MonadTypeChecker types (StateT s m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapStateT (addDeclContext d)
  getMetaState = lift getMetaState
  modifyMetaCtx = lift . modifyMetaCtx
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames

--------------------------------------------------------------------------------
-- Abstract interface for a type system.

-- | A class that provides an abstract interface for a set of builtins.
class (PrintableBuiltin types) => TypableBuiltin types where
  convertFromStandardTypes ::
    (MonadTypeChecker types m) =>
    Provenance ->
    StandardBuiltinType ->
    [NormalisableArg types] ->
    m (NormalisableExpr types)

  -- | Can meta-variables be dependent on their context?
  useDependentMetas :: Proxy types -> Bool

  -- | Construct a type for the builtin
  typeBuiltin ::
    Provenance -> NormalisableBuiltin types -> NormalisableType types

  restrictNetworkType ::
    (MonadTypeChecker types m) =>
    DeclProvenance ->
    GluedType types ->
    m (NormalisableType types)

  restrictDatasetType ::
    (MonadTypeChecker types m) =>
    DeclProvenance ->
    GluedType types ->
    m (NormalisableType types)

  restrictParameterType ::
    (MonadTypeChecker types m) =>
    ParameterSort ->
    DeclProvenance ->
    GluedType types ->
    m (NormalisableType types)

  restrictPropertyType ::
    (MonadTypeChecker types m) =>
    DeclProvenance ->
    GluedType types ->
    m ()

  typeClassRelevancy :: (MonadCompile m) => types -> m Relevance

  addAuxiliaryInputOutputConstraints ::
    (MonadTypeChecker types m) => NormalisableDecl types -> m (NormalisableDecl types)

  generateDefaultConstraint ::
    (MonadTypeChecker types m) =>
    Maybe (NormalisableDecl types) ->
    m Bool

  -- | Solves a type-class constraint
  solveInstance ::
    (MonadNorm types m, MonadTypeChecker types m) => WithContext (TypeClassConstraint types) -> m ()

  handleTypingError ::
    (MonadCompile m) => TypingError types -> m a

--------------------------------------------------------------------------------
-- Operations

getsMetaCtx :: (MonadTypeChecker types m) => (TypeCheckerState types -> a) -> m a
getsMetaCtx f = f <$> getMetaState

getNumberOfMetasCreated :: forall types m. (MonadTypeChecker types m) => Proxy types -> m Int
getNumberOfMetasCreated _ = getsMetaCtx @types (length . metaInfo)

-- | Track the metas solved while performing the provided computation.
-- Multiple calls can be nested arbitrarily deepily.
trackSolvedMetas :: forall types m. (MonadTypeChecker types m) => Proxy types -> m () -> m MetaSet
trackSolvedMetas _ performComputation = do
  modifySolvedMetaState enterSolvedMetaTrackingRegion

  performComputation

  solvedMetas <- getsMetaCtx @types (getMostRecentlySolvedMetas . solvedMetaState)
  modifySolvedMetaState exitSolvedMetaTrackingRegion

  return solvedMetas
  where
    modifySolvedMetaState :: (SolvedMetaState -> SolvedMetaState) -> m ()
    modifySolvedMetaState f = modifyMetaCtx @types $ \TypeCheckerState {..} ->
      TypeCheckerState
        { solvedMetaState = f solvedMetaState,
          ..
        }

getUnsolvedMetas :: forall types m. (MonadTypeChecker types m) => Proxy types -> m MetaSet
getUnsolvedMetas proxy = do
  metasSolved <- MetaMap.keys <$> getMetaSubstitution @types
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
  (MonadTypeChecker types m) =>
  Provenance ->
  NormalisableType types ->
  TypingBoundCtx types ->
  m (MetaID, GluedExpr types)
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

  logDebug MaxDetail $
    "fresh-meta"
      <+> prettyFriendly (WithContext (unnormalised metaExpr) (boundContextOf boundCtx))
      <+> ":"
      <+> prettyVerbose metaType
  return (metaID, metaExpr)

-- | Ensures the meta has no dependencies on the bound context. Returns true
-- if dependencies were removed to achieve this.
removeMetaDependencies :: forall types m. (MonadTypeChecker types m) => Proxy types -> MetaID -> m Bool
removeMetaDependencies _ m = do
  MetaInfo p t ctx <- getMetaInfo @types m
  if null ctx
    then return False
    else do
      newMeta <- snd <$> freshMeta p t mempty
      solveMeta m (unnormalised newMeta) mempty
      return True

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaInfo :: (MonadTypeChecker types m) => MetaID -> m (MetaInfo types)
getMetaInfo m = do
  TypeCheckerState {..} <- getMetaState
  case metaInfo !!? getMetaIndex metaInfo m of
    Just info -> return info
    Nothing ->
      compilerDeveloperError $
        "Requesting info for unknown meta" <+> pretty m <+> "not in context"

getMetaIndex :: [MetaInfo types] -> MetaID -> Int
getMetaIndex metaInfo (MetaID m) = length metaInfo - m - 1

getMetaProvenance :: forall types m. (MonadTypeChecker types m) => Proxy types -> MetaID -> m Provenance
getMetaProvenance _ m = metaProvenance <$> getMetaInfo @types m

getMetaType :: (MonadTypeChecker types m) => MetaID -> m (NormalisableType types)
getMetaType m = metaType <$> getMetaInfo m

getSubstMetaType :: (MonadTypeChecker types m) => MetaID -> m (NormalisableType types)
getSubstMetaType m = substMetas =<< getMetaType m

-- | Get the bound context the meta-variable was created in.
getMetaCtx :: (MonadTypeChecker types m) => MetaID -> m (TypingBoundCtx types)
getMetaCtx m = metaCtx <$> getMetaInfo m

extendBoundCtxOfMeta :: (MonadTypeChecker types m) => MetaID -> NormalisableBinder types -> m ()
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

clearMetaSubstitution :: forall types m. (MonadTypeChecker types m) => Proxy types -> m ()
clearMetaSubstitution _ = modifyMetaCtx @types $ \TypeCheckerState {..} ->
  TypeCheckerState {currentSubstitution = mempty, ..}

getSubstMetaTypes :: (MonadTypeChecker types m) => MetaSet -> m [(MetaID, NormalisableType types)]
getSubstMetaTypes metas = traverse (\m -> (m,) <$> getSubstMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn ::
  forall types m.
  (MonadTypeChecker types m) =>
  [WithContext (Constraint types)] ->
  NormalisableType types ->
  m MetaSet
getMetasLinkedToMetasIn allConstraints typeOfInterest = do
  let constraints = fmap objectIn allConstraints
  metasInType <- metasIn typeOfInterest
  loopOverConstraints constraints metasInType
  where
    loopOverConstraints :: [Constraint types] -> MetaSet -> m MetaSet
    loopOverConstraints constraints metas = do
      (unrelatedConstraints, newMetas) <- foldM processConstraint ([], metas) constraints
      if metas /= newMetas
        then loopOverConstraints unrelatedConstraints newMetas
        else return metas

    processConstraint ::
      ([Constraint types], MetaSet) ->
      Constraint types ->
      m ([Constraint types], MetaSet)
    processConstraint (nonRelatedConstraints, typeMetas) constraint = do
      constraintMetas <- metasIn constraint
      return $
        if MetaSet.disjoint constraintMetas typeMetas
          then (constraint : nonRelatedConstraints, typeMetas)
          else (nonRelatedConstraints, MetaSet.unions [constraintMetas, typeMetas])

abstractOverCtx :: TypingBoundCtx types -> NormalisableExpr types -> NormalisableExpr types
abstractOverCtx ctx body = do
  let p = mempty
  let lamBinderForm (n, _) = BinderDisplayForm (OnlyName (fromMaybe "_" n)) True
  -- WARNING: in theory the type of this binder should be `t` but because these binders
  -- have temporary mutually recursive dependencies that are eliminated upon substitution
  -- then actualy using `t` here results in meta-substitution looping.
  let lam i@(_, _t) = Lam p (Binder p (lamBinderForm i) Explicit Relevant (TypeUniverse p 0))
  foldr lam body (reverse ctx)

solveMeta :: forall types m. (MonadTypeChecker types m) => MetaID -> NormalisableExpr types -> TypingBoundCtx types -> m ()
solveMeta m solution solutionCtx = do
  MetaInfo p _ metaCtx <- getMetaInfo m
  let abstractedSolution = abstractOverCtx metaCtx solution
  let env = typingBoundContextToEnv metaCtx
  gluedSolution <- glueNBE env abstractedSolution

  logDebug MaxDetail $
    "solved"
      <+> pretty m
      <+> "as"
      <+> prettyExternal (WithContext abstractedSolution (boundContextOf solutionCtx))
  -- "as" <+> prettyFriendly (WithContext abstractedSolution (boundContextOf ctx))

  metaSubst <- getMetaSubstitution @types
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

prettyMetas :: forall types m a. (MonadTypeChecker types m) => Proxy types -> MetaSet -> m (Doc a)
prettyMetas _ metas = do
  typedMetaList <- getSubstMetaTypes @types metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: forall types m a. (MonadTypeChecker types m) => Proxy types -> MetaID -> m (Doc a)
prettyMeta _ meta = prettyMetaInternal meta <$> getMetaType @types meta

prettyMetaInternal :: (PrintableBuiltin types) => MetaID -> NormalisableType types -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: forall types m. (MonadTypeChecker types m) => Proxy types -> m ()
clearMetaCtx _ = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyMetaCtx @types (const emptyTypeCheckerState)

getDeclType :: (MonadTypeChecker types m) => Provenance -> Identifier -> m (NormalisableType types)
getDeclType p ident = do
  ctx <- getDeclContext
  case Map.lookup ident ctx of
    Just TypingDeclCtxEntry {..} ->
      return $ unnormalised declType
    -- This should have been caught during scope checking
    Nothing ->
      compilerDeveloperError $
        "Declaration"
          <+> quotePretty ident
          <+> "not found when"
          <+> "looking up variable in context"
          <+> pretty (Map.keys ctx)
          <+> "at"
          <+> pretty p

--------------------------------------------------------------------------------
-- Constraints

generateFreshConstraintID :: forall types m. (MonadTypeChecker types m) => Proxy types -> m ConstraintID
generateFreshConstraintID _ = do
  freshID <- getsMetaCtx @types nextConstraintID
  modifyMetaCtx @types $ \TypeCheckerState {..} ->
    TypeCheckerState {nextConstraintID = nextConstraintID + 1, ..}
  return freshID

getActiveConstraints :: (MonadTypeChecker types m) => m [WithContext (Constraint types)]
getActiveConstraints = do
  us <- fmap (mapObject UnificationConstraint) <$> getActiveUnificationConstraints
  ts <- fmap (mapObject TypeClassConstraint) <$> getActiveTypeClassConstraints
  return $ us <> ts

getActiveUnificationConstraints :: (MonadTypeChecker types m) => m [WithContext (UnificationConstraint types)]
getActiveUnificationConstraints = getsMetaCtx unificationConstraints

getActiveTypeClassConstraints :: (MonadTypeChecker types m) => m [WithContext (TypeClassConstraint types)]
getActiveTypeClassConstraints = getsMetaCtx typeClassConstraints

setConstraints :: (MonadTypeChecker types m) => [WithContext (Constraint types)] -> m ()
setConstraints constraints = do
  let (us, ts) = separateConstraints constraints
  setUnificationConstraints us
  setTypeClassConstraints ts

setTypeClassConstraints :: (MonadTypeChecker types m) => [WithContext (TypeClassConstraint types)] -> m ()
setTypeClassConstraints newConstraints = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {typeClassConstraints = newConstraints, ..}

setUnificationConstraints :: (MonadTypeChecker types m) => [WithContext (UnificationConstraint types)] -> m ()
setUnificationConstraints newConstraints = modifyMetaCtx $ \TypeCheckerState {..} ->
  TypeCheckerState {unificationConstraints = newConstraints, ..}

addConstraints :: (MonadTypeChecker types m) => [WithContext (Constraint types)] -> m ()
addConstraints constraints = do
  let (us, ts) = separateConstraints constraints
  addUnificationConstraints us
  addTypeClassConstraints ts

addUnificationConstraints :: (MonadTypeChecker types m) => [WithContext (UnificationConstraint types)] -> m ()
addUnificationConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyExternal constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {unificationConstraints = unificationConstraints ++ constraints, ..}

addTypeClassConstraints :: (MonadTypeChecker types m) => [WithContext (TypeClassConstraint types)] -> m ()
addTypeClassConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints " <> align (prettyExternal constraints))

  modifyMetaCtx $ \TypeCheckerState {..} ->
    TypeCheckerState {typeClassConstraints = typeClassConstraints ++ constraints, ..}

-- | Adds an entirely new unification constraint (as opposed to one
-- derived from another constraint).
createFreshUnificationConstraint ::
  forall types m.
  (MonadTypeChecker types m) =>
  Provenance ->
  TypingBoundCtx types ->
  ConstraintOrigin types ->
  NormalisableType types ->
  NormalisableType types ->
  m ()
createFreshUnificationConstraint p ctx origin expectedType actualType = do
  let env = typingBoundContextToEnv ctx
  normExpectedType <- whnf env expectedType
  normActualType <- whnf env actualType
  cid <- generateFreshConstraintID (Proxy @types)
  let context = ConstraintContext cid p origin p unknownBlockingStatus ctx
  let unification = Unify normExpectedType normActualType
  let constraint = WithContext unification context

  addUnificationConstraints [constraint]

-- | Create a new fresh copy of the context for a new constraint
copyContext :: forall types m. (MonadTypeChecker types m) => ConstraintContext types -> m (ConstraintContext types)
copyContext (ConstraintContext _cid originProv originalConstraint creationProv _blockingStatus ctx) = do
  freshID <- generateFreshConstraintID (Proxy @types)
  return $ ConstraintContext freshID originProv originalConstraint creationProv unknownBlockingStatus ctx

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

getBinderNameOrFreshName :: (MonadTypeChecker types m) => Maybe Name -> NormalisableType types -> m Name
getBinderNameOrFreshName piName typ = case piName of
  Just x -> return x
  Nothing -> getFreshName typ

glueNBE :: (MonadNorm types m) => Env types -> NormalisableExpr types -> m (GluedExpr types)
glueNBE env e = Glued e <$> whnf env e
