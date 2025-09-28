module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitutable (..),
    RawMetaSubstitutable (..),
    MetaSubstitution,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (findUltimateUnsolvedMeta)
import Vehicle.Compile.Type.Meta.Variable (MetaVariableContext, findMetaInfo, metaCtx, metaSolution, metaType)
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Free.Context

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
      RecordAcc p record field -> RecordAcc p <$> substMetasAt ctx s record <*> pure field
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

instance MetaSubstitutable m builtin (Value builtin) where
  substMetasAt ctx s expr = case expr of
    VMeta m args -> do
      let metaInfo = findMetaInfo s m
      case metaSolution metaInfo of
        -- TODO do we need to substitute through the args here?
        Nothing -> VMeta m <$> substMetasAt ctx s args
        Just value -> do
          substValue <- substMetasAt ctx s $ normalised value
          case args of
            [] -> return substValue
            (a : as) -> normaliseApp ctx substValue (a : as)
    VUniverse {} -> return expr
    VFreeVar v spine -> VFreeVar v <$> traverse (substMetasAt ctx s) spine
    VBoundVar v spine -> VBoundVar v <$> traverse (substMetasAt ctx s) spine
    VRecord ident fields -> VRecord ident <$> traverse (substMetasAt ctx s) fields
    VRecordAcc record field -> VRecordAcc <$> substMetasAt ctx s record <*> pure field
    VBuiltin b spine -> do
      spine' <- traverse (substMetasAt ctx s) spine
      normaliseBuiltin ctx b spine'

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam binder body -> VLam <$> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s body
    VPi binder body -> VPi <$> substMetasAt ctx s binder <*> substMetasAt (nameOf binder : ctx) s body

instance MetaSubstitutable m builtin (Closure builtin) where
  substMetasAt ctx s (Closure env body) = Closure <$> traverseEnv (substMetasAt ctx s) env <*> substMetasAt ctx s body

instance MetaSubstitutable m builtin (GluedExpr builtin) where
  substMetasAt ctx s (Glued a b) = Glued <$> substMetasAt ctx s a <*> substMetasAt ctx s b

instance MetaSubstitutable m builtin (InstanceConstraint builtin) where
  substMetasAt ctx s (Resolve origin m r g) = do
    Resolve <$> substMetasAt ctx s origin <*> findUltimateUnsolvedMeta s m <*> pure r <*> substMetasAt ctx s g

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

instance (MetaSubstitutable m builtin expr) => RawMetaSubstitutable m builtin (GenericProg expr) where
  substMetas s (Main ds) = Main <$> traverse (substMetas s) ds

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
