module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitutable,
    subst,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalBuiltin)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Variable (MetaInfo (..))
import Vehicle.Data.BuiltinInterface (HasStandardData)
import Vehicle.Data.NormalisedExpr

--------------------------------------------------------------------------------
-- Substitution operation

class MetaSubstitutable m builtin a | a -> builtin where
  -- | Substitutes meta-variables through the provided object, returning the
  -- updated object and the set of meta-variables within the object for which
  -- no subsitution was provided.
  subst :: (MonadCompile m, MonadFreeContext builtin m, HasStandardData builtin) => MetaSubstitution builtin -> a -> m a

instance (MetaSubstitutable m builtin b) => MetaSubstitutable m builtin (a, b) where
  subst s (x, y) = do
    y' <- subst s y
    return (x, y')

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin [a] where
  subst s = traverse (subst s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (NonEmpty a) where
  subst s = traverse (subst s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (GenericArg a) where
  subst s = traverse (subst s)

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (GenericBinder a) where
  subst s = traverse (subst s)

instance MetaSubstitutable m builtin (Expr Ix builtin) where
  subst s expr =
    -- logCompilerPass MaxDetail (prettyVerbose ex) $
    case expr of
      e@(Meta p _) -> substApp s p (e, [])
      e@(App p _ _) -> substApp s p (toHead e)
      Universe {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      FreeVar {} -> return expr
      BoundVar {} -> return expr
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi p binder res -> Pi p <$> subst s binder <*> subst s res
      Let p e1 binder e2 -> Let p <$> subst s e1 <*> subst s binder <*> subst s e2
      Lam p binder e -> Lam p <$> subst s binder <*> subst s e

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substApp ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  MetaSubstitution builtin ->
  Provenance ->
  (Expr Ix builtin, [Arg Ix builtin]) ->
  m (Expr Ix builtin)
substApp s p (fun@(Meta _ m), mArgs) = do
  case MetaMap.lookup m s of
    Nothing -> normAppList p fun <$> subst s mArgs
    Just value -> subst s $ substArgs p (unnormalised value) mArgs
substApp s p (fun, args) = normAppList p <$> subst s fun <*> subst s args

instance MetaSubstitutable m builtin (WHNFValue builtin) where
  subst s expr = case expr of
    VMeta m args -> do
      case MetaMap.lookup m s of
        -- TODO do we need to substitute through the args here?
        Nothing -> VMeta m <$> subst s args
        Just value -> do
          substValue <- subst s $ normalised value
          case args of
            [] -> return substValue
            (a : as) -> evalApp defaultNBEOptions substValue (a : as)
    VUniverse {} -> return expr
    VFreeVar v spine -> VFreeVar v <$> traverse (subst s) spine
    VBoundVar v spine -> VBoundVar v <$> traverse (subst s) spine
    VBuiltin b spine -> do
      spine' <- traverse (subst s) spine
      evalBuiltin (evalApp defaultNBEOptions) b spine'

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam binder body -> VLam <$> subst s binder <*> subst s body
    VPi binder body -> VPi <$> subst s binder <*> subst s body

instance MetaSubstitutable m builtin (Body 'WHNF builtin) where
  subst s (WHNFBody env body) = WHNFBody <$> subst s env <*> subst s body

instance MetaSubstitutable m builtin (GluedExpr builtin) where
  subst s (Glued a b) = Glued <$> subst s a <*> subst s b

instance (MetaSubstitutable m builtin expr) => MetaSubstitutable m builtin (GenericDecl expr) where
  subst s = traverse (subst s)

instance (MetaSubstitutable m builtin expr) => MetaSubstitutable m builtin (GenericProg expr) where
  subst s (Main ds) = Main <$> traverse (subst s) ds

instance MetaSubstitutable m builtin (UnificationConstraint builtin) where
  subst s (Unify origin e1 e2) = Unify <$> subst s origin <*> subst s e1 <*> subst s e2

instance MetaSubstitutable m builtin (InstanceConstraint builtin) where
  subst s (Resolve origin m r e) = do
    Resolve origin <$> substMetaID s m <*> pure r <*> subst s e

-- This is a massive hack, and only works because we only have instance resolution
-- for types in the loss typing subsystem which doesn't use dependently types.
substMetaID :: (MonadCompile m) => MetaSubstitution builtin -> MetaID -> m MetaID
substMetaID metaSubst m =
  case MetaMap.lookup m metaSubst of
    Nothing -> return m
    Just other -> case normalised other of
      VMeta m2 [] -> substMetaID metaSubst m2
      _ -> return m

instance MetaSubstitutable m builtin (Constraint builtin) where
  subst s = \case
    UnificationConstraint c -> UnificationConstraint <$> subst s c
    InstanceConstraint c -> InstanceConstraint <$> subst s c

instance (MetaSubstitutable m builtin constraint) => MetaSubstitutable m builtin (Contextualised constraint (ConstraintContext builtin)) where
  subst s (WithContext constraint context) = do
    newConstraint <- subst s constraint
    return $ WithContext newConstraint context

instance MetaSubstitutable m builtin (InstanceConstraintOrigin builtin) where
  subst s (InstanceConstraintOrigin tcOp tcOpArgs tcOpType tc) =
    InstanceConstraintOrigin <$> subst s tcOp <*> subst s tcOpArgs <*> subst s tcOpType <*> subst s tc

instance MetaSubstitutable m builtin (UnificationConstraintOrigin builtin) where
  subst s = \case
    CheckingExprType c -> CheckingExprType <$> subst s c
    CheckingBinderType c -> CheckingBinderType <$> subst s c
    CheckingInstanceType c -> CheckingInstanceType <$> subst s c
    CheckingAuxiliary -> return CheckingAuxiliary

instance MetaSubstitutable m builtin (CheckingExprType builtin) where
  subst s (CheckingExpr e t1 t2) = CheckingExpr <$> subst s e <*> subst s t1 <*> subst s t2

instance MetaSubstitutable m builtin (CheckingBinderType builtin) where
  subst s (CheckingBinder n t1 t2) = CheckingBinder n <$> subst s t1 <*> subst s t2

instance (MetaSubstitutable m builtin a) => MetaSubstitutable m builtin (MetaMap a) where
  subst s (MetaMap t) = MetaMap <$> traverse (subst s) t

instance MetaSubstitutable m builtin (MetaInfo builtin) where
  subst s (MetaInfo p t ctx) = MetaInfo p <$> subst s t <*> pure ctx
