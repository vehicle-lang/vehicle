
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

--------------------------------------------------------------------------------
-- Public interface

-- |Converts DeBruijn variables back into named variables according to the
-- provided context.
runDescope :: Descope t
           => [Symbol]
           -> t Symbol LocallyNamelessVar ann
           -> t Symbol Symbol             ann
runDescope ctx = performDescoping ctx True

-- |Converts DeBruijn variables back into named variables with no context.
runDescopeProg :: Prog Symbol LocallyNamelessVar ann
               -> Prog Symbol Symbol             ann
runDescopeProg = performDescoping mempty True

-- |Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
-- Useful for debugging
runNaiveDescope :: Descope t
                => t Symbol LocallyNamelessVar ann
                -> t Symbol Symbol             ann
runNaiveDescope = performDescoping mempty False

--------------------------------------------------------------------------------
-- Core operation

performDescoping :: Descope t
                 => [Symbol]
                 -> Bool
                 -> t Symbol LocallyNamelessVar ann
                 -> t Symbol Symbol             ann
performDescoping ctx translateDeBruijn e =
  runReader (descope e) (Ctx ctx, translateDeBruijn)

newtype Ctx = Ctx [Symbol]

addBinderToCtx :: Binder Symbol Symbol ann -> (Ctx, Bool) -> (Ctx, Bool)
addBinderToCtx binder (Ctx ctx, r) = (Ctx (nameOf binder : ctx), r)

type MonadDescope m = MonadReader (Ctx, Bool) m

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: Index -> Int -> a
indexOutOfBounds index ctxSize = developerError $
  "DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize

lookupVar :: MonadDescope m => CheckedVar -> m OutputVar
lookupVar = \case
  Free (Identifier name) -> return name
  Bound i -> do
    (Ctx ctx, translateDeBruijn) <- ask
    if not translateDeBruijn
      then return $ pack ("i" <> show i)
      else case ctx !!? i of
        Nothing -> indexOutOfBounds i (length ctx)
        Just x  -> return x

class Descope t where
  descope :: MonadDescope m
          => t Symbol LocallyNamelessVar ann
          -> m (t Symbol Symbol ann)

instance Descope Binder where
  descope = traverseBinderType descope

instance Descope Arg where
  descope = traverseArgExpr descope

showScopeEntry :: Expr Symbol LocallyNamelessVar ann -> Expr Symbol LocallyNamelessVar ann
showScopeEntry e = {-trace ("descope-entry " <> showCore e)-} e

showScopeExit :: MonadDescope m => m (Expr Symbol Symbol ann) -> m (Expr Symbol Symbol ann)
showScopeExit m = do
  e <- m
  {-trace ("descope-exit  " <> showCore e)-}
  return e

instance Descope Expr where
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
      bound'      <- descope bound
      binder'     <- descope binder
      body'       <- local (addBinderToCtx binder') (descope body)
      return $ Let ann bound' binder' body'

    Lam ann binder body -> do
      binder'     <- descope binder
      body'       <- local (addBinderToCtx binder') (descope body)
      return $ Lam ann binder' body'

    Pi ann binder body -> do
      binder'     <- descope binder
      body'       <- local (addBinderToCtx binder') (descope body)
      return $ Pi ann binder' body'

-- No need to add the declaration identifiers to the ctx, as they
-- are untouched during conversion back from de Bruijn indice's.
-- Therefore the following are not in the state monad.

instance Descope Decl where
  descope = \case
    DeclNetw p n t   -> DeclNetw p n <$> descope t
    DeclData p n t   -> DeclData p n <$> descope t
    DefFun   p n t e -> DefFun   p n <$> descope t <*> descope e

instance Descope Prog where
  descope (Main ds) = Main <$> traverse descope ds