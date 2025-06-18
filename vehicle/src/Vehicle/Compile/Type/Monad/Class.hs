module Vehicle.Compile.Type.Monad.Class where

import Control.Monad (foldM, unless)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT (..))
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy (Proxy (..))
import Prettyprinter (fill)
import Vehicle.Compile.Context.Bound.Instance
import Vehicle.Compile.Context.Free.Class (MonadFreeContext)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
  ( HasMetas (..),
    MetaInfo (..),
    MetaVariableContext,
    findMetaInfo,
  )
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution as MetaSubstitution (MetaSubstitutable (..), RawMetaSubstitutable (..))
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Code.Value

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

type DeclIsUnused = Bool

-- | The meta-variables and constraints relating the variables currently in scope.
data TypeCheckerState builtin = TypeCheckerState
  { currentDecl :: Maybe (Decl builtin, DeclIsUnused),
    -- | The origin and type of each meta variable.
    -- NB: these are stored in *reverse* order from which they were created.
    metaVariableCtx :: MetaVariableContext builtin,
    applicationConstraints :: [WithContext (ApplicationConstraint builtin)],
    unificationConstraints :: [WithContext (UnificationConstraint builtin)],
    instanceConstraints :: [WithContext (InstanceConstraint builtin)],
    -- | Instance constraints not solvable by instance class resolution
    auxiliaryInstanceConstraints :: [WithContext (InstanceConstraint builtin)],
    freshNameState :: FreshNameState,
    solvedMetaState :: SolvedMetaState,
    nextConstraintID :: ConstraintID
  }

emptyTypeCheckerState :: TypeCheckerState builtin
emptyTypeCheckerState =
  TypeCheckerState
    { currentDecl = Nothing,
      metaVariableCtx = mempty,
      applicationConstraints = mempty,
      unificationConstraints = mempty,
      instanceConstraints = mempty,
      auxiliaryInstanceConstraints = mempty,
      freshNameState = 0,
      solvedMetaState = SolvedMetaState mempty,
      nextConstraintID = 0
    }

--------------------------------------------------------------------------------
-- The type-checking monad class

-- | The type-checking monad.
class (MonadCompile m, MonadFreeContext builtin m, NormalisableBuiltin builtin, Eq builtin, Hashable builtin, TypableBuiltin builtin) => MonadTypeChecker builtin m where
  getTypeCheckerState :: m (TypeCheckerState builtin)
  modifyTypeCheckerState :: (TypeCheckerState builtin -> TypeCheckerState builtin) -> m ()
  getFreshName :: Type builtin -> m Name
  clearFreshNames :: Proxy builtin -> m ()
  getInstanceCandidates :: m (InstanceDatabase builtin)

instance (Monoid w, MonadTypeChecker builtin m) => MonadTypeChecker builtin (WriterT w m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames
  getInstanceCandidates = lift getInstanceCandidates

instance (Monoid w, MonadTypeChecker builtin m) => MonadTypeChecker builtin (ReaderT w m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames
  getInstanceCandidates = lift getInstanceCandidates

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (StateT s m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames
  getInstanceCandidates = lift getInstanceCandidates

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (BoundContextT (Type builtin) m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames
  getInstanceCandidates = lift getInstanceCandidates

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (SupplyT a m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames
  getInstanceCandidates = lift getInstanceCandidates

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (MaybeT m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState
  getFreshName = lift . getFreshName
  clearFreshNames = lift . clearFreshNames
  getInstanceCandidates = lift getInstanceCandidates

--------------------------------------------------------------------------------
-- Operations

getsTypeCheckerState :: (MonadTypeChecker builtin m) => (TypeCheckerState builtin -> a) -> m a
getsTypeCheckerState f = f <$> getTypeCheckerState

getMetaVariableCtx :: (MonadTypeChecker builtin m) => m (MetaVariableContext builtin)
getMetaVariableCtx = getsTypeCheckerState metaVariableCtx

getNumberOfMetasCreated :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m Int
getNumberOfMetasCreated _ = getsTypeCheckerState @builtin (length . metaVariableCtx)

-- | Track the metas solved while performing the provided computation.
-- Multiple calls can be nested arbitrarily deepily.
trackSolvedMetas :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m () -> m MetaSet
trackSolvedMetas _ performComputation = do
  modifySolvedMetaState enterSolvedMetaTrackingRegion

  performComputation

  solvedMetas <- getsTypeCheckerState @builtin (getMostRecentlySolvedMetas . solvedMetaState)
  modifySolvedMetaState exitSolvedMetaTrackingRegion

  return solvedMetas
  where
    modifySolvedMetaState :: (SolvedMetaState -> SolvedMetaState) -> m ()
    modifySolvedMetaState f = modifyTypeCheckerState @builtin $ \TypeCheckerState {..} ->
      TypeCheckerState
        { solvedMetaState = f solvedMetaState,
          ..
        }

getIsUnblockedFn ::
  forall builtin m constraint.
  (MonadTypeChecker builtin m) =>
  m (Contextualised constraint (ConstraintContext builtin) -> Bool)
getIsUnblockedFn = do
  metasSolved <- getSolvedMetas (Proxy @builtin)
  let isUnblocked = not . constraintIsBlocked metasSolved
  return isUnblocked

substMetas ::
  forall builtin m a.
  (MonadTypeChecker builtin m, RawMetaSubstitutable m builtin a) =>
  a ->
  m a
substMetas x = do
  s <- getMetaVariableCtx
  MetaSubstitution.subst s x

substMetasAt ::
  forall builtin m a.
  (MonadTypeChecker builtin m, MetaSubstitutable m builtin a) =>
  Lv ->
  a ->
  m a
substMetasAt lv x = do
  s <- getMetaVariableCtx
  MetaSubstitution.substAt lv s x

getSolvedMetas :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m MetaSet
getSolvedMetas _proxy = do
  unsolvedMetas <- MetaMap.filter (isJust . metaSolution) <$> getMetaVariableCtx @builtin
  return $ MetaMap.keys unsolvedMetas

getUnsolvedMetas :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m MetaSet
getUnsolvedMetas _proxy = do
  unsolvedMetas <- MetaMap.filter (isNothing . metaSolution) <$> getMetaVariableCtx @builtin
  return $ MetaMap.keys unsolvedMetas

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
  (MonadTypeChecker builtin m) =>
  Provenance ->
  Type builtin ->
  BoundCtx (Type builtin) ->
  m (MetaID, Expr builtin)
freshMeta p metaType boundCtx = do
  -- Create a fresh id for the meta
  TypeCheckerState {..} <- getTypeCheckerState
  let nextMetaID = length metaVariableCtx
  let metaID = MetaID nextMetaID

  -- Construct the information about the meta-variable
  let info = MetaInfo p metaType boundCtx Nothing

  -- Update the meta context
  modifyTypeCheckerState $
    const $
      TypeCheckerState
        { metaVariableCtx = MetaMap.insert metaID info metaVariableCtx,
          ..
        }

  -- Create the expression
  metaExpr <- makeMetaExpr p metaID boundCtx

  logDebug MaxDetail $
    "fresh-meta"
      <+> prettyFriendly (WithContext metaExpr (toNamedBoundCtx boundCtx))
      <+> ":"
      <+> prettyVerbose metaType
  return (metaID, metaExpr)

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaInfo :: (MonadTypeChecker builtin m) => MetaID -> m (MetaInfo builtin)
getMetaInfo meta = do
  TypeCheckerState {..} <- getTypeCheckerState
  return $ findMetaInfo metaVariableCtx meta

getMetaIndex :: [MetaInfo builtin] -> MetaID -> Int
getMetaIndex metaInfo (MetaID m) = length metaInfo - m - 1

getMetaProvenance :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> MetaID -> m Provenance
getMetaProvenance _ m = metaProvenance <$> getMetaInfo @builtin m

getMetaType :: (MonadTypeChecker builtin m) => MetaID -> m (Type builtin)
getMetaType m = metaType <$> getMetaInfo m

-- | Get the bound context the meta-variable was created in.
getMetaCtx :: (MonadTypeChecker builtin m) => Proxy builtin -> MetaID -> m (BoundCtx (Type builtin))
getMetaCtx _ m = metaCtx <$> getMetaInfo m

getSubstMetaTypes :: (MonadTypeChecker builtin m) => MetaSet -> m [(MetaID, Type builtin)]
getSubstMetaTypes metas = traverse (\m -> (m,) <$> getSubstMetaType m) (MetaSet.toList metas)

-- | Computes the set of all metas that are related via constraints to the
-- metas in the provided expression as long as the types of those metas
-- satisfy the provided predicate.
getMetasLinkedToMetasIn ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  [WithContext (Constraint builtin)] ->
  Type builtin ->
  m MetaSet
getMetasLinkedToMetasIn allConstraints typeOfInterest = do
  let constraints = fmap objectIn allConstraints
  let metasInType = metasIn typeOfInterest
  loopOverConstraints constraints metasInType
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
      let constraintMetas = metasIn constraint
      return $
        if MetaSet.disjoint constraintMetas typeMetas
          then (constraint : nonRelatedConstraints, typeMetas)
          else (nonRelatedConstraints, MetaSet.unions [constraintMetas, typeMetas])

-- calculateMetaSolutions :: MetaVariableContext builtin -> MetaMap MetaSet
-- calculateMetaSolutions ctx = MetaMap.filter _ $ _

-- | Creates an expression that abstracts over all bound variables
makeMetaExpr ::
  (MonadCompile m) =>
  Provenance ->
  MetaID ->
  BoundCtx (Type builtin) ->
  m (Expr builtin)
makeMetaExpr p metaID boundCtx = do
  -- Create bound variables for everything in the context
  let dependencyLevels = [0 .. (length boundCtx - 1)]
  let unnormBoundEnv = [Arg p Explicit Relevant (BoundVar p $ Ix i) | i <- reverse dependencyLevels]

  -- Returns a meta applied to every bound variable in the context
  return $ normAppList (Meta p metaID) unnormBoundEnv

abstractOverCtx :: BoundCtx (Type builtin) -> Expr builtin -> Expr builtin
abstractOverCtx ctx body = do
  let p = mempty
  let lamBinderForm n = BinderDisplayForm (OnlyName (fromMaybe "_" n)) True
  -- WARNING: in theory the type of this binder should be `typeOf binder` but because these binders
  -- have temporary mutually recursive dependencies that are eliminated upon substitution
  -- then actualy using `t` here results in meta-substitution looping.
  let lam binder = Lam p (Binder p (lamBinderForm (nameOf binder)) Explicit (relevanceOf binder) (TypeUniverse p 0))
  foldr lam body (reverse ctx)

prettyMetas :: forall builtin m a. (MonadTypeChecker builtin m) => Proxy builtin -> MetaSet -> m (Doc a)
prettyMetas _ metas = do
  typedMetaList <- getSubstMetaTypes @builtin metas
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: forall builtin m a. (MonadTypeChecker builtin m) => Proxy builtin -> MetaID -> m (Doc a)
prettyMeta _ meta = fill 3 . prettyMetaInternal meta <$> getMetaType @builtin meta

prettyMetaInternal :: (PrintableBuiltin builtin) => MetaID -> Type builtin -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ()
clearMetaCtx _ = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyTypeCheckerState @builtin $ const emptyTypeCheckerState

getSubstMetaType :: forall builtin m. (MonadTypeChecker builtin m) => MetaID -> m (Type builtin)
getSubstMetaType m = do
  MetaInfo {..} <- getMetaInfo m
  substMetasAt (boundCtxLv metaCtx) metaType

updateMetaType :: forall builtin m. (MonadTypeChecker builtin m) => MetaID -> Type builtin -> m ()
updateMetaType m typ = do
  let updateInfo info = info {metaType = typ}
  modifyTypeCheckerState $ \state ->
    state
      { metaVariableCtx = MetaMap.adjust updateInfo m (metaVariableCtx state)
      }

--------------------------------------------------------------------------------
-- Constraints

generateFreshConstraintID :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ConstraintID
generateFreshConstraintID _ = do
  freshID <- getsTypeCheckerState @builtin nextConstraintID
  modifyTypeCheckerState @builtin $ \TypeCheckerState {..} ->
    TypeCheckerState {nextConstraintID = nextConstraintID + 1, ..}
  return freshID

createFreshConstraintCtx ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Provenance ->
  BoundCtx (Type builtin) ->
  m (ConstraintContext builtin)
createFreshConstraintCtx creationProvenance ctx = do
  cid <- generateFreshConstraintID (Proxy @builtin)
  return $ ConstraintContext cid creationProvenance unknownBlockingStatus ctx

getActiveConstraints :: (MonadTypeChecker builtin m) => m [WithContext (Constraint builtin)]
getActiveConstraints = do
  us <- fmap (mapObject UnificationConstraint) <$> getActiveUnificationConstraints
  as <- fmap (mapObject ApplicationConstraint) <$> getActiveApplicationConstraints
  ts <- fmap (mapObject InstanceConstraint) <$> getActiveInstanceConstraints
  xs <- fmap (mapObject InstanceConstraint) <$> getActiveAuxiliaryInstanceConstraints
  return $ us <> ts <> as <> xs

getActiveConstraintIDs :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m IntSet
getActiveConstraintIDs _ = do
  activeConstraints <- getActiveConstraints @builtin
  return $ IntSet.fromList $ fmap (unConstraintID . constraintID . contextOf) activeConstraints

getActiveUnificationConstraints :: (MonadTypeChecker builtin m) => m [WithContext (UnificationConstraint builtin)]
getActiveUnificationConstraints = getsTypeCheckerState unificationConstraints

getActiveApplicationConstraints :: (MonadTypeChecker builtin m) => m [WithContext (ApplicationConstraint builtin)]
getActiveApplicationConstraints = getsTypeCheckerState applicationConstraints

getActiveInstanceConstraints :: (MonadTypeChecker builtin m) => m [WithContext (InstanceConstraint builtin)]
getActiveInstanceConstraints = getsTypeCheckerState instanceConstraints

getActiveAuxiliaryInstanceConstraints :: (MonadTypeChecker builtin m) => m [WithContext (InstanceConstraint builtin)]
getActiveAuxiliaryInstanceConstraints = getsTypeCheckerState auxiliaryInstanceConstraints

setInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
setInstanceConstraints newConstraints = modifyTypeCheckerState $ \TypeCheckerState {..} ->
  TypeCheckerState {instanceConstraints = newConstraints, ..}

setApplicationConstraints :: (MonadTypeChecker builtin m) => [WithContext (ApplicationConstraint builtin)] -> m ()
setApplicationConstraints newConstraints = modifyTypeCheckerState $ \TypeCheckerState {..} ->
  TypeCheckerState {applicationConstraints = newConstraints, ..}

setUnificationConstraints :: (MonadTypeChecker builtin m) => [WithContext (UnificationConstraint builtin)] -> m ()
setUnificationConstraints newConstraints = modifyTypeCheckerState $ \TypeCheckerState {..} ->
  TypeCheckerState {unificationConstraints = newConstraints, ..}

setAuxiliaryInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
setAuxiliaryInstanceConstraints newConstraints = modifyTypeCheckerState $ \TypeCheckerState {..} ->
  TypeCheckerState {auxiliaryInstanceConstraints = newConstraints, ..}

addUnificationConstraints :: (MonadTypeChecker builtin m) => [WithContext (UnificationConstraint builtin)] -> m ()
addUnificationConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints:" <> line <> indent 2 (vcat (fmap prettyExternal constraints)))

  modifyTypeCheckerState $ \TypeCheckerState {..} ->
    TypeCheckerState {unificationConstraints = unificationConstraints ++ constraints, ..}

addInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
addInstanceConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints:" <> line <> indent 2 (vcat (fmap prettyExternal constraints)))

  modifyTypeCheckerState $ \TypeCheckerState {..} ->
    TypeCheckerState {instanceConstraints = instanceConstraints ++ constraints, ..}

addApplicationConstraint :: (MonadTypeChecker builtin m) => WithContext (ApplicationConstraint builtin) -> m ()
addApplicationConstraint constraint = do
  logDebug MaxDetail ("add-constraints:" <> line <> indent 2 (prettyExternal constraint))

  modifyTypeCheckerState $ \TypeCheckerState {..} ->
    TypeCheckerState {applicationConstraints = applicationConstraints ++ [constraint], ..}

addAuxiliaryInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
addAuxiliaryInstanceConstraints constraints = do
  logDebug MaxDetail ("add-constraints:" <> line <> indent 2 (vcat (fmap prettyExternal constraints)))

  modifyTypeCheckerState $ \TypeCheckerState {..} ->
    TypeCheckerState {auxiliaryInstanceConstraints = auxiliaryInstanceConstraints ++ constraints, ..}

removeInstanceConstraint ::
  (MonadTypeChecker builtin m) =>
  Proxy builtin ->
  ConstraintID ->
  m (WithContext (InstanceConstraint builtin))
removeInstanceConstraint _ idToFind = do
  TypeCheckerState {..} <- getTypeCheckerState
  let idOf = constraintID . contextOf
  let result1 = findAndDeleteElem (\c -> idOf c == idToFind) instanceConstraints
  case result1 of
    Just (constraint, remainder) -> do
      modifyTypeCheckerState (const $ TypeCheckerState {instanceConstraints = remainder, ..})
      return constraint
    Nothing -> do
      let result2 = findAndDeleteElem (\c -> idOf c == idToFind) auxiliaryInstanceConstraints
      case result2 of
        Just (constraint, remainder) -> do
          modifyTypeCheckerState (const $ TypeCheckerState {auxiliaryInstanceConstraints = remainder, ..})
          return constraint
        Nothing -> do
          developerError $ "Could not find instance constraint with ID" <+> pretty idToFind <+> "to delete"

-- | Create a new fresh copy of the context for a new constraint
copyContext ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  ConstraintContext builtin ->
  Maybe (BoundCtx (Type builtin)) ->
  m (ConstraintContext builtin)
copyContext (ConstraintContext _ creationProv _ ctx) maybeNewCtx = do
  newID <- generateFreshConstraintID (Proxy @builtin)
  let newCtx = fromMaybe ctx maybeNewCtx
  return $ ConstraintContext newID creationProv unknownBlockingStatus newCtx

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

setCurrentDecl :: forall builtin m. (MonadTypeChecker builtin m) => Maybe (Decl builtin, DeclIsUnused) -> m ()
setCurrentDecl maybeDecl = modifyTypeCheckerState $ \TypeCheckerState {..} ->
  TypeCheckerState {currentDecl = maybeDecl, ..}

getCurrentDeclAndUnused :: forall builtin m. (MonadTypeChecker builtin m) => m (Maybe (Decl builtin, DeclIsUnused))
getCurrentDeclAndUnused = do
  maybeDecl <- currentDecl <$> getTypeCheckerState @builtin
  case maybeDecl of
    Nothing -> return Nothing
    Just (decl, isUnused) -> do
      substDecl <- substMetas decl
      let result = Just (substDecl, isUnused)
      setCurrentDecl result
      return result

getCurrentDecl :: forall builtin m. (MonadTypeChecker builtin m) => m (Maybe (Decl builtin))
getCurrentDecl = (fst <$>) <$> getCurrentDeclAndUnused @builtin

glueNBE ::
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  BoundEnv builtin ->
  Expr builtin ->
  m (GluedExpr builtin)
glueNBE env e = Glued e <$> normaliseInEnv env e
