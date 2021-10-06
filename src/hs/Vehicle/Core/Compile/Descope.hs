
module Vehicle.Core.Compile.Descope
  ( runDescope
  , runDescopeWithCtx
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import GHC.Stack (HasCallStack)

import Vehicle.Prelude
import Vehicle.Core.AST

runDescope :: Descope a b => a -> b
runDescope = runDescopeWithCtx mempty

runDescopeWithCtx :: Descope a b => [Name] -> a -> b
runDescopeWithCtx ctx e = runReader (descope e) (Ctx ctx)

newtype Ctx = Ctx [Name]

addBinderToCtx :: OutputBinder -> Ctx -> Ctx
addBinderToCtx (Binder _ _ name _) (Ctx ctx) = Ctx (name : ctx)

type MonadDescope m = MonadReader Ctx m

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: HasCallStack => Index -> Int -> Provenance -> a
indexOutOfBounds index ctxSize p = developerError $
  "DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize <+>
  "at" <+> pretty p

lookupVar :: (HasCallStack, MonadDescope m) => Provenance -> CheckedVar -> m OutputVar
lookupVar p = \case
  Free (Identifier name) -> return $ User name
  Bound i -> do
    Ctx ctx <- ask
    case ctx !!? i of
      Nothing -> indexOutOfBounds i (length ctx) p
      Just x  -> return x


class Descope a b where
  descope :: MonadDescope m => a -> m b

instance Descope CheckedBinder OutputBinder where
  descope (Binder p v n e) = Binder p v n <$> descope e

instance Descope CheckedArg OutputArg where
  descope (Arg p v e) = Arg p v <$> descope e

showScopeEntry :: CheckedExpr -> CheckedExpr
showScopeEntry e = {-trace ("descope-entry " <> showCore e)-} e

showScopeExit :: MonadDescope m => m OutputExpr -> m OutputExpr
showScopeExit m = do
  e <- m
  {-trace ("descope-exit  " <> showCore e)-}
  return e

instance Descope CheckedExpr OutputExpr where
  descope e = showScopeExit $ case showScopeEntry e of
    Type     l                     -> return (Type l)
    Hole     p name                -> return (Hole p name)
    Builtin  ann op                -> return (Builtin ann op)
    Literal  ann l                 -> return (Literal ann l)
    Var      ann v                 -> Var ann <$> lookupVar (prov ann) v
    Ann      ann e1 t              -> Ann ann <$> descope e1 <*> descope t
    App      ann fun args          -> App ann <$> descope fun <*> traverse descope args
    Seq      ann es                -> Seq ann <$> traverse descope es
    PrimDict tc                    -> PrimDict <$> descope tc

    Let ann bound binder body -> do
      bound'  <- descope bound
      binder' <- descope binder
      body'   <- local (addBinderToCtx binder') (descope body)
      return $ Let ann bound' binder' body'

    Lam ann binder body -> do
      binder' <- descope binder
      body'   <- local (addBinderToCtx binder') (descope body)
      return $ Lam ann binder' body'

    Pi ann binder body -> do
      binder' <- descope binder
      body'   <- local (addBinderToCtx binder') (descope body)
      return $ Pi ann binder' body'

    Meta _ann _i -> developerError "Unsolved metas should have been caught before descoping"

-- No need to add the declaration identifiers to the ctx, as they
-- are untouched during conversion back from de Bruijn indice's.
-- Therefore the following are not in the state monad.

instance Descope CheckedDecl OutputDecl where
  descope = \case
    DeclNetw p n t   -> DeclNetw p n <$> descope t
    DeclData p n t   -> DeclData p n <$> descope t
    DefFun   p n t e -> DefFun   p n <$> descope t <*> descope e

instance Descope CheckedProg OutputProg where
  descope (Main ds) = Main <$> traverse descope ds