module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitutable (..),
    RawMetaSubstitutable (..),
    MetaSubstitution,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (findUltimateUnsolvedMeta)
import Vehicle.Compile.Type.Meta.Variable (MetaVariableContext, findMetaInfo, metaCtx, metaSolution, metaType)
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Substitution type

type MetaSubstitution builtin = MetaVariableContext builtin

--------------------------------------------------------------------------------
-- Substitution operation at level

class MetaSubstitutable m builtin a | a -> builtin where
  -- | Substitutes meta-variables through the provided object, returning the
  -- updated object and the set of meta-variables within the object for which
  -- no subsitution was provided.
  substAt ::
    (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
    Lv ->
    MetaSubstitution builtin ->
    a ->
    m a

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (Maybe a) where
  substAt lv s = traverse (substAt lv s)

instance (MetaSubstitutable m builtin b) => MetaSubstitutable m builtin (a, b) where
  substAt lv s (x, y) = do
    y' <- substAt lv s y
    return (x, y')

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin [a] where
  substAt lv s = traverse (substAt lv s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (NonEmpty a) where
  substAt lv s = traverse (substAt lv s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (GenericArg a) where
  substAt lv s = traverse (substAt lv s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (GenericBinder a) where
  substAt lv s = traverse (substAt lv s)

instance MetaSubstitutable m builtin (Expr builtin) where
  substAt lv s expr =
    -- logCompilerPass MaxDetail (prettyVerbose ex) $
    case expr of
      Meta p m -> substMeta lv s (p, m, [])
      App (Meta p m) args -> substMeta lv s (p, m, NonEmpty.toList args)
      App fun args -> App <$> substAt lv s fun <*> substAt lv s args
      Universe {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      FreeVar {} -> return expr
      BoundVar {} -> return expr
      Record p ident fields -> Record p ident <$> traverseRecordFields (substAt lv s) fields
      RecordAcc p record field -> RecordAcc p <$> substAt lv s record <*> pure field
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi p binder res -> Pi p <$> substAt lv s binder <*> substAt (lv + 1) s res
      Let p e1 binder e2 -> Let p <$> substAt lv s e1 <*> substAt lv s binder <*> substAt (lv + 1) s e2
      Lam p binder e -> Lam p <$> substAt lv s binder <*> substAt (lv + 1) s e

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substMeta ::
  forall builtin m.
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  Lv ->
  MetaSubstitution builtin ->
  (Provenance, MetaID, [Arg builtin]) ->
  m (Expr builtin)
substMeta lv s (p, m, mArgs) = do
  let metaInfo = findMetaInfo s m
  case metaSolution metaInfo of
    Nothing -> normAppList (Meta p m) <$> substAt lv s mArgs
    Just value -> do
      let shiftLv = lv - boundCtxLv (metaCtx metaInfo)
      let liftedValue = liftDBIndices shiftLv (unnormalised value)
      substAt lv s $ substArgs liftedValue mArgs

instance MetaSubstitutable m builtin (Value builtin) where
  substAt lv s expr = case expr of
    VMeta m args -> do
      let metaInfo = findMetaInfo s m
      case metaSolution metaInfo of
        -- TODO do we need to substitute through the args here?
        Nothing -> VMeta m <$> substAt lv s args
        Just value -> do
          substValue <- substAt lv s $ normalised value
          case args of
            [] -> return substValue
            (a : as) -> normaliseApp substValue (a : as)
    VUniverse {} -> return expr
    VFreeVar v spine -> VFreeVar v <$> traverse (substAt lv s) spine
    VBoundVar v spine -> VBoundVar v <$> traverse (substAt lv s) spine
    VRecord ident fields -> VRecord ident <$> traverse (substAt lv s) fields
    VRecordAcc record field -> VRecordAcc <$> substAt lv s record <*> pure field
    VBuiltin b spine -> do
      spine' <- traverse (substAt lv s) spine
      normaliseBuiltin b spine'

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam binder body -> VLam <$> substAt lv s binder <*> substAt (lv + 1) s body
    VPi binder body -> VPi <$> substAt lv s binder <*> substAt (lv + 1) s body

instance MetaSubstitutable m builtin (Closure builtin) where
  substAt lv s (Closure env body) = Closure <$> substAt lv s env <*> substAt lv s body

instance MetaSubstitutable m builtin (GluedExpr builtin) where
  substAt lv s (Glued a b) = Glued <$> substAt lv s a <*> substAt lv s b

instance MetaSubstitutable m builtin (InstanceConstraint builtin) where
  substAt lv s (Resolve origin m r g) = do
    Resolve <$> substAt lv s origin <*> findUltimateUnsolvedMeta s m <*> pure r <*> substAt lv s g

instance MetaSubstitutable m builtin (InstanceGoal builtin) where
  substAt lv s (InstanceGoal t h spine) =
    InstanceGoal t h <$> substAt lv s spine

instance MetaSubstitutable m builtin (InstanceArgOrigin builtin) where
  substAt lv s (ArgOrigin tcOp tcOpArgs tcOpType tc) =
    ArgOrigin <$> substAt lv s tcOp <*> substAt lv s tcOpArgs <*> substAt lv s tcOpType <*> substAt lv s tc

instance MetaSubstitutable m builtin (InstanceTypeRestrictionOrigin builtin) where
  substAt lv s (TypeRestrictionOrigin env n sort t) =
    TypeRestrictionOrigin env n sort <$> substAt lv s t

instance MetaSubstitutable m builtin (InstanceConstraintOrigin builtin) where
  substAt lv s = \case
    InstanceTypeRestrictionOrigin t -> InstanceTypeRestrictionOrigin <$> substAt lv s t
    InstanceArgOrigin t -> InstanceArgOrigin <$> substAt lv s t

--------------------------------------------------------------------------------
-- Substitution operation

class RawMetaSubstitutable m builtin a | a -> builtin where
  -- | Substitutes meta-variables through the provided object, returning the
  -- updated object and the set of meta-variables within the object for which
  -- no subsitution was provided.
  subst ::
    (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
    MetaSubstitution builtin ->
    a ->
    m a

instance (MetaSubstitutable m builtin expr) => RawMetaSubstitutable m builtin (GenericDecl expr) where
  subst s = traverse (substAt 0 s)

instance (MetaSubstitutable m builtin expr) => RawMetaSubstitutable m builtin (GenericProg expr) where
  subst s (Main ds) = Main <$> traverse (subst s) ds

instance (MetaSubstitutable m builtin constraint) => RawMetaSubstitutable m builtin (Contextualised constraint (ConstraintContext builtin)) where
  subst s (WithContext constraint ctx) = WithContext <$> substAt (boundCtxLv $ boundContextOf ctx) s constraint <*> pure ctx

instance (RawMetaSubstitutable m builtin a) => RawMetaSubstitutable m builtin [a] where
  subst s = traverse (subst s)

instance RawMetaSubstitutable m builtin (MetaVariableContext builtin) where
  subst s ctx = for ctx $ \entry -> do
    let lv = boundCtxLv (metaCtx entry)
    newType <- substAt lv s (metaType entry)
    newSolution <- substAt lv s (metaSolution entry)
    return $
      entry
        { metaSolution = newSolution,
          metaType = newType
        }
