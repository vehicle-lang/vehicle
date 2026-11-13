module Vehicle.Compile.Type.Monad.Class where

import Control.Monad (foldM, unless)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer.Strict (WriterT (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Prettyprinter (fill)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
  ( MetaInfo (..),
    MetaVariableContext,
    findMetaInfo,
  )
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution (HasMetas, MetaSubstitutable (..), RawMetaSubstitutable (..))
import Vehicle.Compile.Type.Meta.Substitution qualified as MetaSubstitution
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.ForcedValue (ForcedValueWithMetas, ThunkWithMetas, UnforcedSpineWithMetas)
import Vehicle.Data.Code.ModuleInterface
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (addDeclToContext)
import Vehicle.Data.Variable.Free.Context.Class (MonadFreeContext)

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

data TypeCheckerDeclState builtin = TypeCheckerDeclState
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

emptyTypeCheckerDeclState ::
  (Ord builtin) =>
  TypeCheckerDeclState builtin
emptyTypeCheckerDeclState =
  TypeCheckerDeclState
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

-- | The meta-variables and constraints relating the variables currently in scope.
data TypeCheckerState builtin = TypeCheckerState
  { importedModules :: ImportedModuleContext builtin,
    currentModuleInterface :: ModuleTypingInterface builtin,
    declsByName :: Map Identifier (Decl builtin),
    currentFreeCtx :: FreeCtx builtin,
    currentDeclState :: TypeCheckerDeclState builtin
  }

emptyTypeCheckerState ::
  (Ord builtin) =>
  InstanceDatabase builtin ->
  ImportedModuleContext builtin ->
  TypeCheckerState builtin
emptyTypeCheckerState instanceDatabase importedModules = do
  let typedModules = fmap (\(_, interface, _) -> moduleDeclarations $ typedModule interface) importedModules
  let importedDeclsByName = mconcat $ fmap (fromMappedValueList identifierOf) typedModules
  TypeCheckerState
    { importedModules = importedModules,
      currentModuleInterface = emptyModuleTypingInterface {instanceDatabase = instanceDatabase},
      currentFreeCtx = mergeImportedFreeCtxs importedModules,
      declsByName = importedDeclsByName,
      currentDeclState = emptyTypeCheckerDeclState
    }

--------------------------------------------------------------------------------
-- The type-checking monad class

-- | The type-checking monad.
class (MonadCompile m, MonadFreeContext builtin m, NormalisableBuiltin builtin, Ord builtin) => MonadTypeChecker builtin m where
  getTypeCheckerState :: m (TypeCheckerState builtin)
  modifyTypeCheckerState :: (TypeCheckerState builtin -> TypeCheckerState builtin) -> m ()

instance (Monoid w, MonadTypeChecker builtin m) => MonadTypeChecker builtin (WriterT w m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

instance (Monoid w, MonadTypeChecker builtin m) => MonadTypeChecker builtin (ReaderT w m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (StateT s m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (BoundContextT (Type builtin) m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (SupplyT a m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (MaybeT m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

instance (MonadTypeChecker builtin m) => MonadTypeChecker builtin (NameBoundContextT m) where
  getTypeCheckerState = lift getTypeCheckerState
  modifyTypeCheckerState = lift . modifyTypeCheckerState

getsTypeCheckerState :: (MonadTypeChecker builtin m) => (TypeCheckerState builtin -> a) -> m a
getsTypeCheckerState f = f <$> getTypeCheckerState

getTypeCheckerDeclState :: (MonadTypeChecker builtin m) => m (TypeCheckerDeclState builtin)
getTypeCheckerDeclState = getsTypeCheckerState currentDeclState

getsTypeCheckerDeclState :: (MonadTypeChecker builtin m) => (TypeCheckerDeclState builtin -> a) -> m a
getsTypeCheckerDeclState f = getsTypeCheckerState (f . currentDeclState)

modifyTypeCheckerDeclState :: (MonadTypeChecker builtin m) => (TypeCheckerDeclState builtin -> TypeCheckerDeclState builtin) -> m ()
modifyTypeCheckerDeclState f = modifyTypeCheckerState $ \state ->
  state {currentDeclState = f (currentDeclState state)}

--------------------------------------------------------------------------------
-- Operations

getFreshName :: forall builtin m. (MonadTypeChecker builtin m) => Type builtin -> m Name
getFreshName _typ = do
  nameID <- getsTypeCheckerDeclState @builtin freshNameState
  modifyTypeCheckerDeclState @builtin (\state -> state {freshNameState = nameID + 1})
  return $ layoutAsText $ "_x" <> pretty nameID

clearFreshNames :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ()
clearFreshNames _proxy =
  modifyTypeCheckerDeclState @builtin (\state -> state {freshNameState = 0})

getMetaVariableCtx :: (MonadTypeChecker builtin m) => m (MetaVariableContext builtin)
getMetaVariableCtx = getsTypeCheckerDeclState metaVariableCtx

getNumberOfMetasCreated :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m Int
getNumberOfMetasCreated _ = getsTypeCheckerDeclState @builtin (length . metaVariableCtx)

getInstanceCandidatesFromFreeCtx ::
  (MonadTypeChecker builtin m) =>
  InstanceGoal builtin ->
  m [InstanceCandidate builtin]
getInstanceCandidatesFromFreeCtx goal = do
  imports <- getsTypeCheckerState importedModules
  current <- getsTypeCheckerState currentModuleInterface
  return $ concatInCombinedContext typingInterface (lookupInstances goal . instanceDatabase) current imports

-- | Gets the type from the database
getBuiltinTypeFromDatabase ::
  (MonadTypeChecker builtin m) =>
  builtin ->
  m (Type builtin)
getBuiltinTypeFromDatabase builtin = do
  imports <- getsTypeCheckerState importedModules
  current <- getsTypeCheckerState currentModuleInterface
  let maybeType = lookupInCombinedContext typingInterface (Map.lookup builtin . builtinDatabase) current imports
  case maybeType of
    Nothing -> developerError $ "unexpectedly no type found for builtin" <+> quotePretty builtin
    Just typ -> return typ

addBuiltinTypeToDatabase ::
  (MonadTypeChecker builtin m) =>
  builtin ->
  Type builtin ->
  m ()
addBuiltinTypeToDatabase builtin builtinType = do
  let err = developerError $ "Unexpected duplicate types for builtin" <+> quotePretty builtin
  modifyTypeCheckerState $ \state ->
    state
      { currentModuleInterface =
          (currentModuleInterface state)
            { builtinDatabase = Map.insertWithKey err builtin builtinType (builtinDatabase $ currentModuleInterface state)
            }
      }

-- | Track the metas solved while performing the provided computation.
-- Multiple calls can be nested arbitrarily deepily.
trackSolvedMetas :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m () -> m MetaSet
trackSolvedMetas _ performComputation = do
  modifySolvedMetaState enterSolvedMetaTrackingRegion

  performComputation

  solvedMetas <- getsTypeCheckerDeclState @builtin (getMostRecentlySolvedMetas . solvedMetaState)
  modifySolvedMetaState exitSolvedMetaTrackingRegion

  return solvedMetas
  where
    modifySolvedMetaState :: (SolvedMetaState -> SolvedMetaState) -> m ()
    modifySolvedMetaState f = modifyTypeCheckerDeclState @builtin $ \state ->
      state
        { solvedMetaState = f (solvedMetaState state)
        }

getIsUnblockedFn ::
  forall builtin m constraint.
  (MonadTypeChecker builtin m) =>
  m (Contextualised constraint (ConstraintContext builtin) -> Bool)
getIsUnblockedFn = do
  metasSolved <- getSolvedMetas (Proxy @builtin)
  let isUnblocked = not . constraintIsBlocked metasSolved
  return isUnblocked

metasIn ::
  forall builtin m a.
  (MonadTypeChecker builtin m, HasMetas a) =>
  Proxy builtin ->
  a ->
  m MetaSet
metasIn _ x = do
  s <- getMetaVariableCtx @builtin
  return $ MetaSubstitution.metasIn s x

substMetaVariables ::
  forall builtin m a.
  (MonadTypeChecker builtin m, NormalisableBuiltin builtin, RawMetaSubstitutable m builtin a) =>
  a ->
  m a
substMetaVariables x = do
  s <- getMetaVariableCtx
  MetaSubstitution.substMetas s x

substMetaVariablesAt ::
  forall builtin m a.
  (MonadTypeChecker builtin m, NormalisableBuiltin builtin, MetaSubstitutable m builtin a) =>
  NamedBoundCtx ->
  a ->
  m a
substMetaVariablesAt ctx x = do
  s <- getMetaVariableCtx
  MetaSubstitution.substMetasAt ctx s x

getSolvedMetas :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m MetaSet
getSolvedMetas _proxy = do
  unsolvedMetas <- MetaMap.filter (isJust . metaSolution) <$> getMetaVariableCtx @builtin
  return $ MetaMap.keys unsolvedMetas

getUnsolvedMetas :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m MetaSet
getUnsolvedMetas _proxy = do
  unsolvedMetas <- MetaMap.filter (isNothing . metaSolution) <$> getMetaVariableCtx @builtin
  return $ MetaMap.keys unsolvedMetas

forceThunkWithMetas ::
  (MonadTypeChecker builtin m) =>
  NamedBoundCtx ->
  ThunkWithMetas builtin ->
  m (ForcedValueWithMetas builtin, MetaSet)
forceThunkWithMetas ctx value = do
  metaCtx <- getMetaVariableCtx
  MetaSubstitution.forceThunkWithMetas ctx metaCtx value

forceApplicationWithMetas ::
  (MonadTypeChecker builtin m) =>
  NamedBoundCtx ->
  ThunkWithMetas builtin ->
  UnforcedSpineWithMetas builtin ->
  m (ForcedValueWithMetas builtin)
forceApplicationWithMetas ctx value spine = do
  metaCtx <- getMetaVariableCtx
  MetaSubstitution.forceApplicationWithMetas ctx metaCtx value spine

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
  Relevance ->
  BoundCtx (Type builtin) ->
  m (MetaID, Expr builtin)
freshMeta p metaType relevance boundCtx = do
  -- Create a fresh id for the meta
  TypeCheckerDeclState {..} <- getTypeCheckerDeclState
  let nextMetaID = length metaVariableCtx
  let metaID = MetaID nextMetaID

  -- Construct the information about the meta-variable
  let info = MetaInfo p metaType relevance boundCtx Nothing

  -- Update the meta context
  modifyTypeCheckerDeclState $
    const $
      TypeCheckerDeclState
        { metaVariableCtx = MetaMap.insert metaID info metaVariableCtx,
          ..
        }

  -- Create the expression
  metaExpr <- makeMetaExpr p metaID boundCtx
  logDebug MaxDetail $
    "fresh-meta"
      <+> prettyFriendly (WithContext metaExpr (toNamedBoundCtx boundCtx))
      <+> ":"
      <+> prettyFriendly (WithContext metaType (toNamedBoundCtx boundCtx))
  return (metaID, metaExpr)

--------------------------------------------------------------------------------
-- Meta information retrieval

getMetaInfo :: (MonadTypeChecker builtin m) => MetaID -> m (MetaInfo builtin)
getMetaInfo meta = do
  state <- getTypeCheckerDeclState
  return $ findMetaInfo (metaVariableCtx state) meta

getMetaIndex :: [MetaInfo builtin] -> MetaID -> Int
getMetaIndex metaInfo (MetaID m) = length metaInfo - m - 1

getMetaProvenance :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> MetaID -> m Provenance
getMetaProvenance _ m = metaProvenance <$> getMetaInfo @builtin m

getMetaType :: (MonadTypeChecker builtin m) => MetaID -> m (Type builtin)
getMetaType m = metaType <$> getMetaInfo m

-- | Get the bound context the meta-variable was created in.
getMetaCtx :: (MonadTypeChecker builtin m) => Proxy builtin -> MetaID -> m (BoundCtx (Type builtin))
getMetaCtx _ m = metaCtx <$> getMetaInfo m

getDecl ::
  forall builtin m.
  (MonadTypeChecker builtin m, HasCallStack) =>
  Proxy builtin ->
  Identifier ->
  m (Decl builtin)
getDecl _proxy ident = do
  declsByName <- getsTypeCheckerState @builtin declsByName
  return $ lookupInFreeCtx ident declsByName

getRecordDefinition ::
  (MonadTypeChecker builtin m) =>
  Proxy builtin ->
  Identifier ->
  m (Telescope builtin, RecordFields builtin)
getRecordDefinition proxy ident = do
  decl <- getDecl proxy ident
  case decl of
    DefRecord _ _ _ telescope fields _ ->
      return (telescope, fields)
    _ ->
      developerError $
        pretty ident <+> "is unexpectedly not a record"

getDeclType ::
  (MonadTypeChecker builtin m, HasCallStack) =>
  Proxy builtin ->
  Identifier ->
  m (Type builtin)
getDeclType proxy ident = do
  decl <- getDecl proxy ident
  return $ case decl of
    DefAbstract _ _ _ t -> t
    DefFunction _ _ _ t _ -> t
    DefRecord p _ _ telescope _ _ -> foldr (Pi p) (Universe p 0) telescope

addTypedDeclToContext ::
  (MonadTypeChecker builtin m) =>
  Decl builtin ->
  m a ->
  m a
addTypedDeclToContext decl cont = do
  modifyTypeCheckerState $ \state ->
    state
      { declsByName = Map.insert (identifierOf decl) decl (declsByName state)
      }
  addDeclToContext decl cont

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
  metasInType <- metasIn (Proxy @builtin) typeOfInterest
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
      constraintMetas <- metasIn (Proxy @builtin) constraint
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
  let unnormBoundEnv = [Arg Explicit Relevant (BoundVar p $ Ix i) | i <- reverse dependencyLevels]

  -- Returns a meta applied to every bound variable in the context
  return $ normAppList (Meta p metaID) unnormBoundEnv

abstractOverCtx :: BoundCtx (Type builtin) -> Expr builtin -> Expr builtin
abstractOverCtx ctx body = do
  let p = mempty
  let lamBinderForm n = BinderDisplayForm (OnlyName (fromMaybe "_" n) p) True
  -- WARNING: in theory the type of this binder should be `typeOf binder` but because these binders
  -- have temporary mutually recursive dependencies that are eliminated upon substitution
  -- then actualy using `t` here results in meta-substitution looping.
  let lam binder = Lam p (Binder (lamBinderForm (nameOf binder)) Explicit (relevanceOf binder) (TypeUniverse p 0))
  foldr lam body (reverse ctx)

prettyMetas :: forall builtin m a. (MonadTypeChecker builtin m, NormalisableBuiltin builtin, PrintableBuiltin builtin) => Proxy builtin -> MetaSet -> m (Doc a)
prettyMetas _ metas = do
  typedMetaList <- traverse (\m -> (m,) <$> getSubstMetaType @builtin m) (MetaSet.toList metas)
  let docs = fmap (uncurry prettyMetaInternal) typedMetaList
  return $ prettySetLike docs

prettyMeta :: forall builtin m a. (MonadTypeChecker builtin m) => Proxy builtin -> MetaID -> m (Doc a)
prettyMeta _ meta = fill 3 . prettyMetaInternal meta <$> getMetaType @builtin meta

prettyMetaInternal :: (PrintableBuiltin builtin) => MetaID -> Type builtin -> Doc a
prettyMetaInternal m t = pretty m <+> ":" <+> prettyVerbose t

clearMetaCtx :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ()
clearMetaCtx _ = do
  logDebug MaxDetail "Clearing meta-variable context"
  modifyTypeCheckerState @builtin $ \state -> do
    state
      { currentDeclState = emptyTypeCheckerDeclState
      }

getSubstMetaType :: forall builtin m. (MonadTypeChecker builtin m, NormalisableBuiltin builtin) => MetaID -> m (Type builtin)
getSubstMetaType m = do
  MetaInfo {..} <- getMetaInfo m
  substMetaVariablesAt (toNamedBoundCtx metaCtx) metaType

updateMetaType :: forall builtin m. (MonadTypeChecker builtin m) => MetaID -> Type builtin -> m ()
updateMetaType m typ = do
  let updateInfo info = info {metaType = typ}
  modifyTypeCheckerDeclState $ \state ->
    state
      { metaVariableCtx = MetaMap.adjust updateInfo m (metaVariableCtx state)
      }

--------------------------------------------------------------------------------
-- Constraints

generateFreshConstraintID :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ConstraintID
generateFreshConstraintID _ = do
  freshID <- getsTypeCheckerDeclState @builtin nextConstraintID
  modifyTypeCheckerDeclState @builtin $ \state ->
    state {nextConstraintID = nextConstraintID state + 1}
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
getActiveUnificationConstraints = getsTypeCheckerDeclState unificationConstraints

getActiveApplicationConstraints :: (MonadTypeChecker builtin m) => m [WithContext (ApplicationConstraint builtin)]
getActiveApplicationConstraints = getsTypeCheckerDeclState applicationConstraints

getActiveInstanceConstraints :: (MonadTypeChecker builtin m) => m [WithContext (InstanceConstraint builtin)]
getActiveInstanceConstraints = getsTypeCheckerDeclState instanceConstraints

getActiveAuxiliaryInstanceConstraints :: (MonadTypeChecker builtin m) => m [WithContext (InstanceConstraint builtin)]
getActiveAuxiliaryInstanceConstraints = getsTypeCheckerDeclState auxiliaryInstanceConstraints

setInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
setInstanceConstraints newConstraints = modifyTypeCheckerDeclState $ \state ->
  state {instanceConstraints = newConstraints}

setApplicationConstraints :: (MonadTypeChecker builtin m) => [WithContext (ApplicationConstraint builtin)] -> m ()
setApplicationConstraints newConstraints = modifyTypeCheckerDeclState $ \state ->
  state {applicationConstraints = newConstraints}

setUnificationConstraints :: (MonadTypeChecker builtin m) => [WithContext (UnificationConstraint builtin)] -> m ()
setUnificationConstraints newConstraints = modifyTypeCheckerDeclState $ \state ->
  state {unificationConstraints = newConstraints}

setAuxiliaryInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
setAuxiliaryInstanceConstraints newConstraints = modifyTypeCheckerDeclState $ \state ->
  state {auxiliaryInstanceConstraints = newConstraints}

addUnificationConstraints :: (MonadTypeChecker builtin m) => [WithContext (UnificationConstraint builtin)] -> m ()
addUnificationConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints:" <> lineIndent (vcat (fmap prettyExternal constraints)))

  modifyTypeCheckerDeclState $ \state ->
    state {unificationConstraints = unificationConstraints state ++ constraints}

addInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
addInstanceConstraints constraints = do
  unless (null constraints) $ do
    logDebug MaxDetail ("add-constraints:" <> lineIndent (vcat (fmap prettyExternal constraints)))

  modifyTypeCheckerDeclState $ \state ->
    state {instanceConstraints = instanceConstraints state ++ constraints}

addApplicationConstraint :: (MonadTypeChecker builtin m) => WithContext (ApplicationConstraint builtin) -> m ()
addApplicationConstraint constraint = do
  logDebug MaxDetail ("add-constraints:" <> lineIndent (prettyExternal constraint))

  modifyTypeCheckerDeclState $ \state ->
    state {applicationConstraints = applicationConstraints state ++ [constraint]}

addAuxiliaryInstanceConstraints :: (MonadTypeChecker builtin m) => [WithContext (InstanceConstraint builtin)] -> m ()
addAuxiliaryInstanceConstraints constraints = do
  logDebug MaxDetail ("add-constraints:" <> lineIndent (vcat (fmap prettyExternal constraints)))

  modifyTypeCheckerDeclState $ \state ->
    state {auxiliaryInstanceConstraints = auxiliaryInstanceConstraints state ++ constraints}

removeInstanceConstraint ::
  (MonadTypeChecker builtin m) =>
  Proxy builtin ->
  ConstraintID ->
  m (WithContext (InstanceConstraint builtin))
removeInstanceConstraint _ idToFind = do
  TypeCheckerDeclState {..} <- getTypeCheckerDeclState
  let idOf = constraintID . contextOf
  let result1 = findAndDeleteElem (\c -> idOf c == idToFind) instanceConstraints
  case result1 of
    Just (constraint, remainder) -> do
      modifyTypeCheckerDeclState (const $ TypeCheckerDeclState {instanceConstraints = remainder, ..})
      return constraint
    Nothing -> do
      let result2 = findAndDeleteElem (\c -> idOf c == idToFind) auxiliaryInstanceConstraints
      case result2 of
        Just (constraint, remainder) -> do
          modifyTypeCheckerDeclState (const $ TypeCheckerDeclState {auxiliaryInstanceConstraints = remainder, ..})
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
setCurrentDecl maybeDecl = modifyTypeCheckerDeclState $ \state ->
  state {currentDecl = maybeDecl}

getCurrentDeclAndUnused :: forall builtin m. (MonadTypeChecker builtin m, NormalisableBuiltin builtin) => m (Maybe (Decl builtin, DeclIsUnused))
getCurrentDeclAndUnused = do
  maybeDecl <- currentDecl <$> getTypeCheckerDeclState @builtin
  case maybeDecl of
    Nothing -> return Nothing
    Just (decl, isUnused) -> do
      substDecl <- substMetaVariables decl
      let result = Just (substDecl, isUnused)
      setCurrentDecl result
      return result

getCurrentDecl :: forall builtin m. (MonadTypeChecker builtin m, NormalisableBuiltin builtin) => m (Maybe (Decl builtin))
getCurrentDecl = (fst <$>) <$> getCurrentDeclAndUnused @builtin

--------------------------------------------------------------------------------
-- Other
--------------------------------------------------------------------------------

-- | Every free variable in the expression which is missing implicit arguments
-- gets them inserted automatically.
prependMissingFreeVarImplicitArgs ::
  forall m builtin.
  (MonadTypeChecker builtin m) =>
  Decl builtin ->
  m (Decl builtin)
prependMissingFreeVarImplicitArgs = traverse (traverseFreeVarsM (const id) processFreeVar)
  where
    processFreeVar :: FreeVarUpdate m builtin
    processFreeVar f p ident args = do
      declType <- getDeclType (Proxy @builtin) ident
      args' <- traverseArgs f args
      finalArgs <- insertNewArgs args' declType
      return $ normAppList (FreeVar p ident) finalArgs

    -- For each leading auto-generalised implicit Pi binder, consume the
    -- matching implicit from the spine.
    insertNewArgs :: [Arg builtin] -> Type builtin -> m [Arg builtin]
    insertNewArgs as = \case
      Pi _ binder result | wasInsertedByCompiler binder && isImplicit binder ->
        case as of
          a : rest -> (a :) <$> insertNewArgs rest result
          [] -> return as
      _ -> return as
