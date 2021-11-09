
module Vehicle.Language.Descope
  ( Descope
  , runDescope
  , runDescopeProg
  , runNaiveDescope
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST

runDescope :: (IsBoundCtx ctx, Descope a b) => ctx -> a -> b
runDescope ctx e = runReader (descope e) (Ctx (ctxNames ctx), True)

runDescopeProg :: DeBruijnProg ann -> NamedProg ann
runDescopeProg = runDescope (mempty :: [Name])

-- | Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
-- Useful for debugging
runNaiveDescope :: Descope a b => a -> b
runNaiveDescope e = runReader (descope e) (Ctx mempty, False)

newtype Ctx = Ctx [Name]

addBinderToCtx :: NamedBinder ann -> (Ctx, Bool) -> (Ctx, Bool)
addBinderToCtx binder (Ctx ctx, r) = (Ctx (nameOf binder : ctx), r)

type MonadDescope m = MonadReader (Ctx, Bool) m

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: Index -> Int -> a
indexOutOfBounds index ctxSize = developerError $
  "DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize

lookupVar :: MonadDescope m => CheckedVar -> m OutputVar
lookupVar = \case
  Free (Identifier name) -> return $ User name
  Bound i -> do
    (Ctx ctx, translateDeBruijn) <- ask
    if not translateDeBruijn
      then return $ User (pack ("i" <> show i))
      else case ctx !!? i of
        Nothing -> indexOutOfBounds i (length ctx)
        Just x  -> return x


class Descope a b where
  descope :: MonadDescope m => a -> m b

instance Descope (DeBruijnBinder ann) (NamedBinder ann) where
  descope = traverseBinderType descope

instance Descope (DeBruijnArg ann) (NamedArg ann) where
  descope = traverseArgExpr descope

showScopeEntry :: DeBruijnExpr ann -> DeBruijnExpr ann
showScopeEntry e = {-trace ("descope-entry " <> showCore e)-} e

showScopeExit :: MonadDescope m => m (NamedExpr ann) -> m (NamedExpr ann)
showScopeExit m = do
  e <- m
  {-trace ("descope-exit  " <> showCore e)-}
  return e

instance Descope (DeBruijnExpr ann) (NamedExpr ann) where
  descope e = showScopeExit $ case showScopeEntry e of
    Type     l                     -> return (Type l)
    Hole     p name                -> return (Hole p name)
    Builtin  ann op                -> return (Builtin ann op)
    Literal  ann l                 -> return (Literal ann l)
    Var      ann v                 -> Var ann <$> lookupVar v
    Ann      ann e1 t              -> Ann ann <$> descope e1 <*> descope t
    App      ann fun args          -> App ann <$> descope fun <*> traverse descope args
    Seq      ann es                -> Seq ann <$> traverse descope es
    PrimDict tc                    -> PrimDict <$> descope tc
    Meta     ann i                 -> return $ Meta ann i

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

-- No need to add the declaration identifiers to the ctx, as they
-- are untouched during conversion back from de Bruijn indice's.
-- Therefore the following are not in the state monad.

instance Descope (DeBruijnDecl ann) (NamedDecl ann) where
  descope = \case
    DeclNetw p n t   -> DeclNetw p n <$> descope t
    DeclData p n t   -> DeclData p n <$> descope t
    DefFun   p n t e -> DefFun   p n <$> descope t <*> descope e

instance Descope (DeBruijnProg ann) (NamedProg ann) where
  descope (Main ds) = Main <$> traverse descope ds