
module Vehicle.Core.Compile.Descope
  ( descopeProg
  , descopeWithCtx
  ) where

import Control.Monad.Supply (MonadSupply, demand, runSupply, withSupply)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Text (pack)
import Prettyprinter ((<+>), Pretty(pretty))

import Vehicle.Prelude
import Vehicle.Core.AST hiding (lift)

descopeProg :: CheckedProg -> OutputProg
descopeProg = descopeWithCtx emptyCtx

descopeWithCtx :: Descope a b => Ctx -> a -> b
descopeWithCtx ctx e = runSupply (withSupply (\i -> "v" <> pack (show i)) (runReaderT (descope e) ctx)) (+ 1) (0 :: Integer)

newtype Ctx = Ctx [Symbol]

emptyCtx :: Ctx
emptyCtx = Ctx mempty

addToCtx :: OutputBinder -> Ctx -> Ctx
addToCtx (Binder _ _ name _) (Ctx ctx) = Ctx (name : ctx)

type MonadDescope m = (MonadSupply Symbol Identity m, MonadReader Ctx m)

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: Index -> Int -> Provenance -> a
indexOutOfBounds index ctxSize p = developerError $
  "DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize <+>
  "at" <+> pretty p

lookupVar :: MonadDescope m => Provenance -> CheckedVar -> m OutputVar
lookupVar p = \case
  Free (Identifier name) -> return name
  Bound i -> do
    Ctx ctx <- ask
    case ctx !!? i of
      Nothing -> indexOutOfBounds i (length ctx) p
      Just x  -> return x


class Descope a b where
  descope :: MonadDescope m => a -> m b

instance Descope CheckedAnn OutputAnn where
  descope (RecAnn e p) = RecAnn <$> descope e <*> pure p

instance Descope CheckedBind OutputBind where
  descope (User name) = return name
  descope Machine     = demand

instance Descope CheckedBinder OutputBinder where
  descope (Binder p v n e) = Binder p v <$> descope n <*> descope e

instance Descope CheckedArg OutputArg where
  descope (Arg p v e) = Arg p v <$> descope e

instance Descope CheckedExpr OutputExpr where
  descope = \case
    Type     l                     -> return (Type l)
    Constraint                     -> return Constraint
    Meta     p i                   -> return (Meta p i)
    Hole     ann name              -> Hole    <$> descope ann <*> pure name
    Var      ann v                 -> Var     <$> descope ann <*> lookupVar (prov ann) v
    Ann      ann e t               -> Ann     <$> descope ann <*> descope e <*> descope t
    App      ann fun arg           -> App     <$> descope ann <*> descope fun <*> descope arg
    Builtin  ann op                -> Builtin <$> descope ann <*> pure op
    Literal  ann l                 -> Literal <$> descope ann <*> pure l
    Seq      ann es                -> Seq     <$> descope ann <*> traverse descope es


    Let ann binder bound body -> do
      ann'    <- descope ann
      bound'  <- descope bound
      binder' <- descope binder
      body'   <- local (addToCtx binder') (descope body)
      return $ Let ann' binder' bound' body'

    Lam ann binder body -> do
      ann'    <- descope ann
      binder' <- descope binder
      body'   <- local (addToCtx binder') (descope body)
      return $ Lam ann' binder' body'

    Pi ann binder body -> do
      ann'    <- descope ann
      binder' <- descope binder
      body'   <- local (addToCtx binder') (descope body)
      return $ Pi ann' binder' body'

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