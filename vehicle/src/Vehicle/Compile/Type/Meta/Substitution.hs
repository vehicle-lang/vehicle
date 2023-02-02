module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitution,
    MetaSubstitutable,
    substituteMetas,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalApp, evalBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Variable (MetaInfo (..))
import Vehicle.Compile.Type.VariableContext (MetaSubstitution)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (BasicNormExpr, GluedExpr (..), NormExpr (..))

-- | Substitutes meta-variables through the provided object, returning the
-- updated object and the set of meta-variables within the object for which
-- no subsitution was provided.
substituteMetas ::
  (MonadCompile m, MetaSubstitutable a) =>
  DeclCtx BasicNormExpr ->
  MetaSubstitution ->
  a ->
  m a
substituteMetas declCtx sub e =
  runReaderT (subst e) (sub, declCtx)

--------------------------------------------------------------------------------
-- Substitution operation

type MonadSubst m =
  ( MonadCompile m,
    MonadReader (MetaSubstitution, DeclCtx BasicNormExpr) m
  )

class MetaSubstitutable a where
  subst :: MonadSubst m => a -> m a

instance MetaSubstitutable a => MetaSubstitutable (a, a) where
  subst (e1, e2) = do
    e1' <- subst e1
    e2' <- subst e2
    return (e1', e2')

instance MetaSubstitutable a => MetaSubstitutable [a] where
  subst = traverse subst

instance MetaSubstitutable a => MetaSubstitutable (NonEmpty a) where
  subst = traverse subst

instance MetaSubstitutable a => MetaSubstitutable (GenericArg a) where
  subst = traverse subst

instance MetaSubstitutable a => MetaSubstitutable (GenericBinder value a) where
  subst = traverse subst

instance MetaSubstitutable CheckedExpr where
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
  forall m.
  MonadSubst m =>
  Provenance ->
  (CheckedExpr, [CheckedArg]) ->
  m CheckedExpr
substApp p (fun@(Meta _ m), mArgs) = do
  (metaSubst, _declCtx) <- ask
  case MetaMap.lookup m metaSubst of
    Just value -> subst =<< substArgs (unnormalised value) mArgs
    Nothing -> normAppList p fun <$> subst mArgs
  where
    substArgs :: CheckedExpr -> [CheckedArg] -> m CheckedExpr
    substArgs (Lam _ _ body) (arg : args) = do
      substArgs (argExpr arg `substDBInto` body) args
    substArgs e args = return $ normAppList p e args
substApp p (fun, args) = normAppList p <$> subst fun <*> subst args

instance MetaSubstitutable BasicNormExpr where
  subst expr = case expr of
    VMeta m args -> do
      (metaSubst, declCtx) <- ask
      case MetaMap.lookup m metaSubst of
        -- TODO do we need to subst through the args here?
        Nothing -> VMeta m <$> subst args
        Just value -> do
          substValue <- subst $ normalised value
          case args of
            [] -> return substValue
            (a : as) -> do
              -- logDebug MaxDetail $ prettyVerbose substValue -- <+> prettyVerbose (fmap argExpr (a : as))
              runReaderT (evalApp substValue (a : as)) (declCtx, metaSubst)
    VUniverse {} -> return expr
    VLiteral {} -> return expr
    VFreeVar v spine -> VFreeVar v <$> traverse subst spine
    VBoundVar v spine -> VBoundVar v <$> traverse subst spine
    VLVec xs spine -> VLVec <$> traverse subst xs <*> traverse subst spine
    VBuiltin b spine -> do
      (metaSubst, declCtx) <- ask
      spine' <- traverse subst spine
      runReaderT (evalBuiltin b spine') (declCtx, metaSubst)

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam binder env body -> VLam <$> subst binder <*> subst env <*> subst body
    VPi binder body -> VPi <$> subst binder <*> subst body

instance MetaSubstitutable GluedExpr where
  subst (Glued a b) = Glued <$> subst a <*> subst b

instance MetaSubstitutable expr => MetaSubstitutable (GenericDecl expr) where
  subst = traverse subst

instance MetaSubstitutable expr => MetaSubstitutable (GenericProg expr) where
  subst (Main ds) = Main <$> traverse subst ds

instance MetaSubstitutable UnificationConstraint where
  subst (Unify e1 e2) = Unify <$> subst e1 <*> subst e2

instance MetaSubstitutable TypeClassConstraint where
  subst (Has m tc es) = Has m tc <$> subst es

instance MetaSubstitutable Constraint where
  subst = \case
    UnificationConstraint c -> UnificationConstraint <$> subst c
    TypeClassConstraint c -> TypeClassConstraint <$> subst c

instance MetaSubstitutable constraint => MetaSubstitutable (Contextualised constraint ConstraintContext) where
  subst (WithContext constraint context) = do
    newConstraint <- subst constraint
    return $ WithContext newConstraint context

instance MetaSubstitutable a => MetaSubstitutable (MetaMap a) where
  subst (MetaMap t) = MetaMap <$> traverse subst t

instance MetaSubstitutable MetaInfo where
  subst (MetaInfo p t ctx) = MetaInfo p <$> subst t <*> pure ctx
