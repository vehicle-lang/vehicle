{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitutable (..),
    RawMetaSubstitutable (..),
    HasMetas,
    metasIn,
    MetaSubstitution,
    forceThunkWithMetas,
    forceApplicationWithMetas,
  )
where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..), execWriter)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Ordered qualified as OMap
import Data.Traversable (for)
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet, findUltimateUnsolvedMeta)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Variable (MetaVariableContext, findMetaInfo, metaCtx, metaSolution, metaType)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context

-----------------------------------------------------------------------------
-- Meta-variable forcing

forceThunkWithMetas ::
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  NamedBoundCtx ->
  MetaVariableContext builtin ->
  ThunkWithMetas builtin ->
  m (ForcedValueWithMetas builtin, MetaSet)
forceThunkWithMetas ctx metaCtx thunk = do
  runForceT ctx metaCtx (forceThunk thunk)

forceApplicationWithMetas ::
  forall builtin m.
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  NamedBoundCtx ->
  MetaVariableContext builtin ->
  ThunkWithMetas builtin ->
  UnforcedSpineWithMetas builtin ->
  m (ForcedValueWithMetas builtin)
forceApplicationWithMetas ctx metaCtx fun spine = do
  fst <$> runForceT ctx metaCtx (forceApplication fun spine)

runForceT ::
  (MonadFreeContext builtin m) =>
  NamedBoundCtx ->
  MetaVariableContext builtin ->
  ReaderT (MetaID -> Maybe (GluedExprWithMetas builtin)) (WriterT MetaSet (NameBoundContextT m)) a ->
  m (a, MetaSet)
runForceT ctx metaCtx action =
  runNameBoundContextT ctx $ runWriterT $ runReaderT action (\m -> metaSolution $ findMetaInfo metaCtx m)

--------------------------------------------------------------------------------
-- Objects which have meta variables in.

metasIn :: (HasMetas a) => MetaSubstitution builtin -> a -> MetaSet
metasIn s e = execWriter (findMetas s e)

class HasMetas a where
  findMetas :: (MonadWriter MetaSet m) => MetaSubstitution builtin -> a -> m ()

processMeta :: (MonadWriter MetaSet m) => MetaSubstitution builtin -> MetaID -> m ()
processMeta s m = case metaSolution $ findMetaInfo s m of
  Nothing -> tell (MetaSet.singleton m)
  Just sol -> findMetas s $ normalised sol

instance HasMetas (Expr builtin) where
  findMetas s expr = case expr of
    Meta _ m -> processMeta s m
    Universe {} -> return ()
    Hole {} -> return ()
    Builtin {} -> return ()
    BoundVar {} -> return ()
    FreeVar {} -> return ()
    Pi _ binder result -> do findMetas s binder; findMetas s result
    Let _ bound binder body -> do findMetas s bound; findMetas s binder; findMetas s body
    Lam _ binder body -> do findMetas s binder; findMetas s body
    App fun args -> do findMetas s fun; findMetas s args
    Record _ _ fields -> findMetas s $ fmap snd fields
    RecordProj _ recordType record _ -> do findMetas s recordType; findMetas s record

instance HasMetas (ForcedValueWithMetas builtin) where
  findMetas s expr = case expr of
    VMeta m spine -> do
      processMeta s m
      findMetas s spine
    VUniverse {} -> return ()
    VBuiltin _ spine -> findMetas s spine
    VFreeVar _ spine -> findMetas s spine
    VBoundVar _ spine -> findMetas s spine
    VPi binder closure -> do findMetas s binder; findMetas s closure
    VLam binder closure -> do findMetas s binder; findMetas s closure
    VRecord _ fields -> findMetas s (snd <$> OMap.assocs fields)
    VRecordAcc recordType record _ spine -> do
      findMetas s recordType
      findMetas s record
      findMetas s spine

instance HasMetas (ThunkWithMetas builtin) where
  findMetas s = \case
    Forced value -> findMetas s value
    Unforced env expr -> do
      traverseEnv_ (findMetas s) env
      findMetas s expr

instance HasMetas (ClosureWithMetas builtin) where
  findMetas s (Closure env expr) = do
    traverseEnv_ (findMetas s) env
    findMetas s expr

instance (HasMetas expr) => HasMetas (GenericArg expr) where
  findMetas s = mapM_ $ findMetas s

instance (HasMetas expr) => HasMetas (GenericBinder expr) where
  findMetas s = mapM_ $ findMetas s

instance (HasMetas a) => HasMetas [a] where
  findMetas s = mapM_ $ findMetas s

instance (HasMetas a) => HasMetas (NonEmpty a) where
  findMetas s = mapM_ $ findMetas s

instance HasMetas (InstanceConstraint builtin) where
  findMetas s (Resolve _ m _ _ goal) = do
    processMeta s m
    findMetas s goal

instance HasMetas (InstanceGoal builtin) where
  findMetas s (InstanceGoal _ _ spine) = findMetas s spine

instance HasMetas (UnificationConstraint builtin) where
  findMetas s (Unify _ e1 e2) = do findMetas s e1; findMetas s e2

instance HasMetas (ArgInsertionProblem builtin) where
  findMetas s ArgInsertionProblem {..} = do
    findMetas s originalFun
    findMetas s checkedArgs
    findMetas s uncheckedArgs

instance HasMetas (ApplicationConstraint builtin) where
  findMetas s (InferArgs _ _ insertionProblem) = findMetas s insertionProblem

instance HasMetas (Constraint builtin) where
  findMetas s = \case
    UnificationConstraint c -> findMetas s c
    InstanceConstraint c -> findMetas s c
    ApplicationConstraint c -> findMetas s c

--------------------------------------------------------------------------------
-- Substitution type

type MetaSubstitution builtin = MetaVariableContext builtin

--------------------------------------------------------------------------------
-- Substitution operation at level

class MetaSubstitutable m builtin a | a -> builtin where
  -- | Substitutes meta-variables through the provided object, returning the
  -- updated object and the set of meta-variables within the object for which
  -- no subsitution was provided.
  substMetasAt ::
    (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
    NamedBoundCtx ->
    MetaSubstitution builtin ->
    a ->
    m a

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (Maybe a) where
  substMetasAt ctx s = traverse (substMetasAt ctx s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin [a] where
  substMetasAt ctx s = traverse (substMetasAt ctx s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (NonEmpty a) where
  substMetasAt ctx s = traverse (substMetasAt ctx s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (GenericArg a) where
  substMetasAt ctx s = traverse (substMetasAt ctx s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (GenericBinder a) where
  substMetasAt ctx s = traverse (substMetasAt ctx s)

instance MetaSubstitutable m builtin (Expr builtin) where
  substMetasAt ctx s expr =
    -- logCompilerPass MaxDetail (prettyVerbose ex) $
    case expr of
      Meta p m -> substMeta ctx s (p, m, [])
      App (Meta p m) args -> substMeta ctx s (p, m, NonEmpty.toList args)
      App fun args -> App <$> substMetasAt ctx s fun <*> substMetasAt ctx s args
      Universe {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      FreeVar {} -> return expr
      BoundVar {} -> return expr
      Record p ident fields -> Record p ident <$> traverseRecordFields (substMetasAt ctx s) fields
      RecordProj p recordType record field -> RecordProj p <$> substMetasAt ctx s recordType <*> substMetasAt ctx s record <*> pure field
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi p binder res -> Pi p <$> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s res
      Let p e1 binder e2 -> Let p <$> substMetasAt ctx s e1 <*> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s e2
      Lam p binder e -> Lam p <$> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s e

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substMeta ::
  forall builtin m.
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  NamedBoundCtx ->
  MetaSubstitution builtin ->
  (Provenance, MetaID, [Arg builtin]) ->
  m (Expr builtin)
substMeta ctx s (p, m, mArgs) = do
  let metaInfo = findMetaInfo s m
  case metaSolution metaInfo of
    Nothing -> normAppList (Meta p m) <$> substMetasAt ctx s mArgs
    Just value -> do
      let shiftLv = boundCtxLv ctx - boundCtxLv (metaCtx metaInfo)
      let liftedValue = liftDBIndices shiftLv (unnormalised value)
      substMetasAt ctx s $ substArgs liftedValue mArgs

instance MetaSubstitutable m builtin (ThunkWithMetas builtin) where
  substMetasAt ctx s = \case
    Forced value -> Forced <$> substMetasAt ctx s value
    Unforced env expr -> Unforced <$> traverseEnv (substMetasAt ctx s) env <*> substMetasAt ctx s expr

instance MetaSubstitutable m builtin (ForcedValueWithMetas builtin) where
  substMetasAt ctx s expr = case expr of
    VMeta m spine -> do
      let metaInfo = findMetaInfo s m
      case metaSolution metaInfo of
        Nothing -> VMeta m <$> substMetasAt ctx s spine
        Just value -> do
          substValue <- forceApplicationWithMetas ctx s (normalised value) spine
          substMetasAt ctx s substValue
    VUniverse {} -> return expr
    VFreeVar v spine -> VFreeVar v <$> traverse (substMetasAt ctx s) spine
    VBoundVar v spine -> VBoundVar v <$> traverse (substMetasAt ctx s) spine
    VRecord ident fields -> VRecord ident <$> traverse (substMetasAt ctx s) fields
    VRecordAcc recordType record field spine -> do
      recordType' <- substMetasAt ctx s recordType
      record' <- substMetasAt ctx s record
      spine' <- traverse (substMetasAt ctx s) spine
      return $ VRecordAcc recordType' record' field spine'
    VBuiltin b spine -> do
      spine' <- traverse (substMetasAt ctx s) spine
      return $ VBuiltin b spine'

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam binder body -> VLam <$> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s body
    VPi binder body -> VPi <$> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s body

instance MetaSubstitutable m builtin (ClosureWithMetas builtin) where
  substMetasAt ctx s (Closure env body) = Closure <$> traverseEnv (substMetasAt ctx s) env <*> substMetasAt ctx s body

instance MetaSubstitutable m builtin (GluedExprWithMetas builtin) where
  substMetasAt ctx s (Glued a b) = Glued <$> substMetasAt ctx s a <*> substMetasAt ctx s b

instance MetaSubstitutable m builtin (InstanceConstraint builtin) where
  substMetasAt ctx s (Resolve origin m r c g) = do
    Resolve <$> substMetasAt ctx s origin <*> findUltimateUnsolvedMeta s m <*> pure r <*> pure c <*> substMetasAt ctx s g

instance MetaSubstitutable m builtin (InstanceGoal builtin) where
  substMetasAt ctx s (InstanceGoal t h spine) =
    InstanceGoal t h <$> substMetasAt ctx s spine

instance MetaSubstitutable m builtin (InstanceArgOrigin builtin) where
  substMetasAt ctx s (ArgOrigin tcOp tcOpArgs tcOpType tc) =
    ArgOrigin <$> substMetasAt ctx s tcOp <*> substMetasAt ctx s tcOpArgs <*> substMetasAt ctx s tcOpType <*> substMetasAt ctx s tc

instance MetaSubstitutable m builtin (InstanceTypeRestrictionOrigin builtin) where
  substMetasAt ctx s (TypeRestrictionOrigin env n sort t) =
    TypeRestrictionOrigin env n sort <$> substMetasAt ctx s t

instance MetaSubstitutable m builtin (InstanceConstraintOrigin builtin) where
  substMetasAt ctx s = \case
    InstanceTypeRestrictionOrigin t -> InstanceTypeRestrictionOrigin <$> substMetasAt ctx s t
    InstanceArgOrigin t -> InstanceArgOrigin <$> substMetasAt ctx s t

--------------------------------------------------------------------------------
-- Substitution operation

class RawMetaSubstitutable m builtin a | a -> builtin where
  -- | Substitutes meta-variables through the provided object, returning the
  -- updated object and the set of meta-variables within the object for which
  -- no subsitution was provided.
  substMetas ::
    (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
    MetaSubstitution builtin ->
    a ->
    m a

instance (MetaSubstitutable m builtin expr) => RawMetaSubstitutable m builtin (GenericDecl expr) where
  substMetas s = traverse (substMetasAt mempty s)

instance (MetaSubstitutable m builtin constraint) => RawMetaSubstitutable m builtin (Contextualised constraint (ConstraintContext builtin)) where
  substMetas s (WithContext constraint ctx) = WithContext <$> substMetasAt (namedBoundCtxOf ctx) s constraint <*> pure ctx

instance (RawMetaSubstitutable m builtin a) => RawMetaSubstitutable m builtin [a] where
  substMetas s = traverse (substMetas s)

instance RawMetaSubstitutable m builtin (MetaVariableContext builtin) where
  substMetas s ctx = for ctx $ \entry -> do
    let namedCtx = toNamedBoundCtx $ metaCtx entry
    newType <- substMetasAt namedCtx s (metaType entry)
    newSolution <- substMetasAt namedCtx s (metaSolution entry)
    return $
      entry
        { metaSolution = newSolution,
          metaType = newType
        }
