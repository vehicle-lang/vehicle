module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitutable,
    substMetas,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Variable (MetaInfo (..))
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (GluedExpr (..), NormExpr (..))

-- | Substitutes meta-variables through the provided object, returning the
-- updated object and the set of meta-variables within the object for which
-- no subsitution was provided.
substMetas :: (MonadCompile m, MetaSubstitutable m a) => a -> m a
substMetas = subst

--------------------------------------------------------------------------------
-- Substitution operation

class MetaSubstitutable m a where
  subst :: MonadCompile m => a -> m a

instance MetaSubstitutable m a => MetaSubstitutable m (a, a) where
  subst (e1, e2) = do
    e1' <- subst e1
    e2' <- subst e2
    return (e1', e2')

instance MetaSubstitutable m a => MetaSubstitutable m [a] where
  subst = traverse subst

instance MetaSubstitutable m a => MetaSubstitutable m (NonEmpty a) where
  subst = traverse subst

instance MetaSubstitutable m a => MetaSubstitutable m (GenericArg a) where
  subst = traverse subst

instance MetaSubstitutable m a => MetaSubstitutable m (GenericBinder value a) where
  subst = traverse subst

instance MonadNorm builtin m => MetaSubstitutable m (DBExpr builtin) where
  subst expr =
    -- logCompilerPass MaxDetail (prettyVerbose ex) $
    case expr of
      e@(Meta p _) -> substApp p (e, [])
      e@(App p _ _) -> substApp p (toHead e)
      Universe {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Literal {} -> return expr
      Var {} -> return expr
      LVec p es -> LVec p <$> traverse subst es
      Ann p term typ -> Ann p <$> subst term <*> subst typ
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi p binder res -> Pi p <$> subst binder <*> subst res
      Let p e1 binder e2 -> Let p <$> subst e1 <*> subst binder <*> subst e2
      Lam p binder e -> Lam p <$> subst binder <*> subst e

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substApp ::
  forall builtin m.
  MonadNorm builtin m =>
  Provenance ->
  (DBExpr builtin, [DBArg builtin]) ->
  m (DBExpr builtin)
substApp p (fun@(Meta _ m), mArgs) = do
  metaSubst <- getMetaSubstitution
  case MetaMap.lookup m metaSubst of
    Just value -> subst =<< substArgs (unnormalised value) mArgs
    Nothing -> normAppList p fun <$> subst mArgs
  where
    substArgs :: DBExpr builtin -> [DBArg builtin] -> m (DBExpr builtin)
    substArgs (Lam _ _ body) (arg : args) = do
      substArgs (argExpr arg `substDBInto` body) args
    substArgs e args = return $ normAppList p e args
substApp p (fun, args) = normAppList p <$> subst fun <*> subst args

instance MonadNorm builtin m => MetaSubstitutable m (NormExpr builtin) where
  subst expr = case expr of
    VMeta m args -> do
      metaSubst <- getMetaSubstitution
      case MetaMap.lookup m metaSubst of
        -- TODO do we need to subst through the args here?
        Nothing -> VMeta m <$> subst args
        Just value -> do
          substValue <- subst $ normalised value
          case args of
            [] -> return substValue
            (a : as) -> evalApp substValue (a : as)
    -- logDebug MaxDetail $ prettyVerbose substValue -- <+> prettyVerbose (fmap argExpr (a : as))

    VUniverse {} -> return expr
    VLiteral {} -> return expr
    VFreeVar v spine -> VFreeVar v <$> traverse subst spine
    VBoundVar v spine -> VBoundVar v <$> traverse subst spine
    VLVec xs spine -> VLVec <$> traverse subst xs <*> traverse subst spine
    VBuiltin b spine -> do
      spine' <- traverse subst spine
      evalBuiltin b spine'

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam binder env body -> VLam <$> subst binder <*> subst env <*> subst body
    VPi binder body -> VPi <$> subst binder <*> subst body

instance MonadNorm builtin m => MetaSubstitutable m (GluedExpr builtin) where
  subst (Glued a b) = Glued <$> subst a <*> subst b

instance MetaSubstitutable m expr => MetaSubstitutable m (GenericDecl expr) where
  subst = traverse subst

instance MetaSubstitutable m expr => MetaSubstitutable m (GenericProg expr) where
  subst (Main ds) = Main <$> traverse subst ds

instance MonadNorm builtin m => MetaSubstitutable m (UnificationConstraint builtin) where
  subst (Unify e1 e2) = Unify <$> subst e1 <*> subst e2

instance MonadNorm builtin m => MetaSubstitutable m (TypeClassConstraint builtin) where
  subst (Has m tc es) = Has m tc <$> subst es

instance MonadNorm builtin m => MetaSubstitutable m (Constraint builtin) where
  subst = \case
    UnificationConstraint c -> UnificationConstraint <$> subst c
    TypeClassConstraint c -> TypeClassConstraint <$> subst c

instance MetaSubstitutable m constraint => MetaSubstitutable m (Contextualised constraint (ConstraintContext builtin)) where
  subst (WithContext constraint context) = do
    newConstraint <- subst constraint
    return $ WithContext newConstraint context

instance MetaSubstitutable m a => MetaSubstitutable m (MetaMap a) where
  subst (MetaMap t) = MetaMap <$> traverse subst t

instance MonadNorm builtin m => MetaSubstitutable m (MetaInfo builtin) where
  subst (MetaInfo p t ctx) = MetaInfo p <$> subst t <*> pure ctx
