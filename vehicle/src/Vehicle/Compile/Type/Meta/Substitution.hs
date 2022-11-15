module Vehicle.Compile.Type.Meta.Substitution
  ( MetaSubstitution
  , MetaSubstitutable
  , substituteMetas
  ) where

import Control.Monad.Reader (MonadReader (..), ReaderT(..))
import Data.List.NonEmpty (NonEmpty ((:|)))

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude hiding (subst)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Variable (MetaInfo (..))
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Normalise.NormExpr (NormExpr(..), GluedExpr (..))
import Vehicle.Compile.Normalise.NBE (evalApp, evalBuiltin)

type MetaSubstitution = MetaMap GluedExpr

substituteMetas :: (MonadCompile m, MetaSubstitutable a)
                => DeclCtx NormExpr
                -> MetaSubstitution
                -> a
                -> m a
substituteMetas declCtx sub e = runReaderT (subst e) (sub, declCtx)

--------------------------------------------------------------------------------
-- Substitution operation

type MonadSubst m =
  ( MonadCompile m
  , MonadReader (MetaSubstitution, DeclCtx NormExpr) m
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
    --logCompilerPass MaxDetail (prettyVerbose ex) $
    case expr of
      e@(Meta ann _)  -> substApp ann (e, [])
      e@(App ann _ _) -> substApp ann (toHead e)

      Universe{} -> return expr
      Hole{}     -> return expr
      Builtin{}  -> return expr
      Literal{}  -> return expr
      Var{}      -> return expr

      LVec ann es       -> LVec ann <$> traverse subst es
      Ann  ann term typ -> Ann  ann <$> subst term   <*> subst typ
      -- NOTE: no need to lift the substitutions here as we're passing under the binders
      -- because by construction every meta-variable solution is a closed term.
      Pi  ann binder res   -> Pi       ann <$> subst binder <*> subst res
      Let ann e1 binder e2 -> Let      ann <$> subst e1     <*> subst binder <*> subst e2
      Lam ann binder e     -> Lam      ann <$> subst binder <*> subst e

-- | We really don't want un-normalised lambda applications from solved meta-variables
-- clogging up our program so this function detects meta applications and normalises
-- them as it substitutes the meta in.
substApp :: forall m . MonadSubst m
          => Provenance
          -> (CheckedExpr, [CheckedArg])
          -> m CheckedExpr
substApp ann (fun@(Meta _ m), mArgs) = do
  (metaSubst, _declCtx) <- ask
  case MetaMap.lookup m metaSubst of
    Just value -> subst =<< substArgs (unnormalised value) mArgs
    Nothing    -> normAppList ann fun <$> subst mArgs
  where
    substArgs :: CheckedExpr -> [CheckedArg] -> m CheckedExpr
    substArgs (Lam _ _ body) (arg : args) = do
      substArgs (argExpr arg `substInto` body) args
    substArgs Lam{}          []           = compilerDeveloperError $
      "Meta variable" <+> pretty m <+> "does not appear to be applied to" <+>
      "every variable in the context"
    substArgs e              args         = return $ normAppList ann e args
substApp ann (fun, args) = normAppList ann <$> subst fun <*> subst args

instance MetaSubstitutable NormExpr where
  subst expr = case expr of
    VMeta p m args -> do
      (metaSubst, declCtx) <- ask
      case MetaMap.lookup m metaSubst of
        -- TODO do we need to subst through the args here?
        Nothing    -> VMeta p m <$> subst args
        Just value -> do
          -- logDebug MaxDetail $ pretty m <+> pretty (show (normalised value))
          substValue <- subst $ normalised value
          case args of
            []       -> return substValue
            (a : as) -> do
              -- logDebug MaxDetail $ prettyVerbose substValue -- <+> prettyVerbose (fmap argExpr (a : as))
              runReaderT (evalApp substValue (a :| as)) declCtx

    VUniverse{} -> return expr
    VLiteral{}  -> return expr

    VVar     p v  spine -> VVar     p v <$> traverse subst spine
    VLVec    p xs spine -> VLVec    p   <$> traverse subst xs <*> traverse subst spine

    VBuiltin p b  spine -> do
      (_metaSubst, declCtx) <- ask
      spine' <- traverse subst spine
      runReaderT (evalBuiltin p b spine') declCtx

    -- NOTE: no need to lift the substitutions here as we're passing under the binders
    -- because by construction every meta-variable solution is a closed term.
    VLam  p binder env body -> VLam p <$> subst binder <*> subst env <*> subst body
    VPi   p binder body     -> VPi  p <$> subst binder <*> subst body

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
    TypeClassConstraint   c -> TypeClassConstraint   <$> subst c

instance MetaSubstitutable a => MetaSubstitutable (MetaMap a) where
  subst (MetaMap t) = MetaMap <$> traverse subst t

instance MetaSubstitutable MetaInfo where
  subst (MetaInfo p t ctx) = MetaInfo p <$> subst t <*> pure ctx
