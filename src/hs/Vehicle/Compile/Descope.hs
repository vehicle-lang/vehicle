
module Vehicle.Compile.Descope
  ( Descope
  , runDescope
  , runDescopeProg
  , runNaiveDBDescope
  , runNaiveCoDBDescope

  ) where

import Control.Monad.Reader (MonadReader(..), Reader, runReader)
import Data.Text (pack)
import Data.Map (Map)

import Vehicle.Compile.Prelude
import Data.Coerce (coerce)

--------------------------------------------------------------------------------
-- Public interface

-- |Converts DeBruijn variables back into named variables according to the
-- provided context.
runDescope :: Descope t
           => NamedBoundCtx
           -> t NamedBinding DBVar
           -> t NamedBinding Symbol
runDescope ctx = performDescoping ctx descopeDBVar

-- |Converts DeBruijn variables back into named variables with no context.
runDescopeProg :: Prog Symbol DBVar
               -> Prog Symbol Symbol
runDescopeProg = performDescoping mempty descopeDBVar

-- |Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
-- Useful for debugging
runNaiveDBDescope :: Descope t
                  => t Symbol DBVar
                  -> t Symbol Symbol
runNaiveDBDescope = performDescoping mempty descopeDBVarNaive

-- |Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
-- Useful for debugging
runNaiveCoDBDescope :: (Descope t, ExtractPositionTrees t)
                    => t (CoDBBinding Symbol) CoDBVar
                    -> (t Symbol Symbol, Map Symbol (Maybe PositionTree))
runNaiveCoDBDescope e1 =
  let (e2, pts) = extractPTs e1 in
  let e3 = performDescoping mempty descopeCoDBVarNaive e2 in
  (e3, pts)

--------------------------------------------------------------------------------
-- Core operation

-- Can get rid of this newtype and just use NamedBoundCtx instead?
newtype Ctx = Ctx [NamedBinding]

addBinderToCtx :: NamedBinder -> Ctx -> Ctx
addBinderToCtx binder (Ctx ctx) = Ctx (nameOf binder : ctx)

performDescoping :: (Descope t, Show var)
                 => [NamedBinding]
                 -> (Provenance -> var -> Reader Ctx NamedVar)
                 -> t NamedBinding var
                 -> t NamedBinding NamedVar
performDescoping ctx convertVar e =
  runReader (descope convertVar e) (Ctx ctx)

type MonadDescope m = MonadReader Ctx m

class Descope t where
  descope :: (MonadDescope m, Show var)
          => (Provenance -> var -> m NamedVar)
          -> t NamedBinding var
          -> m (t NamedBinding NamedVar)

instance Descope Prog where
  descope f (Main ds) = Main <$> traverse (descope f) ds

instance Descope Decl where
  descope f = traverseDeclExprs (descope f)

instance Descope Expr where
  descope f e = showScopeExit $ case showScopeEntry e of
    Universe  ann l        -> return $ Universe ann l
    Hole     ann name     -> return $ Hole    ann name
    Builtin  ann op       -> return $ Builtin ann op
    Literal  ann l        -> return $ Literal ann l
    Meta     ann i        -> return $ Meta ann i
    Var      ann v        -> Var ann <$> f ann v
    Ann      ann e1 t     -> Ann ann <$> descope f e1 <*> descope f t
    App      ann fun args -> App ann <$> descope f fun <*> traverse (descopeArg f) args
    LVec     ann es       -> LVec ann <$> traverse (descope f) es

    Let ann bound binder body -> do
      bound'      <- descope f bound
      binder'     <- descopeBinder f binder
      body'       <- local (addBinderToCtx binder') (descope f body)
      return $ Let ann bound' binder' body'

    Lam ann binder body -> do
      binder'     <- descopeBinder f binder
      body'       <- local (addBinderToCtx binder') (descope f body)
      return $ Lam ann binder' body'

    Pi ann binder body -> do
      binder'     <- descopeBinder f binder
      body'       <- local (addBinderToCtx binder') (descope f body)
      return $ Pi ann binder' body'

descopeBinder :: (MonadReader Ctx f, Show var)
              => (Provenance -> var -> f NamedVar)
              -> GenericBinder NamedBinding (Expr NamedBinding var)
              -> f (GenericBinder NamedBinding (Expr NamedBinding NamedVar))
descopeBinder f = traverse (descope f)

descopeArg :: (MonadReader Ctx f, Show var)
           => (Provenance -> var -> f NamedVar)
           -> GenericArg (Expr NamedBinding var)
           -> f (GenericArg (Expr NamedBinding NamedVar))
descopeArg f = traverse (descope f)

instance Descope Arg' where
  descope f a = coerce <$> descopeArg f (coerce a)

instance Descope Binder' where
  descope f a = coerce <$> descopeBinder f (coerce a)

descopeDBVar :: MonadDescope m => Provenance -> DBVar -> m NamedVar
descopeDBVar _ (Free (Identifier name)) = return name
descopeDBVar p (Bound i) = do
  Ctx ctx <- ask
  case ctx !!? i of
    Nothing -> indexOutOfBounds p i (length ctx)
    Just x  -> return x

descopeDBVarNaive :: MonadDescope m => Provenance -> DBVar -> m NamedVar
descopeDBVarNaive _ = \case
  Free  i -> return $ nameOf i
  Bound i -> return $ pack ("i" <> show i)

descopeCoDBVarNaive :: MonadDescope m => Provenance -> CoDBVar -> m NamedVar
descopeCoDBVarNaive _ = \case
  CoDBFree i -> return $ nameOf i
  CoDBBound  -> return "CoDBVar"

--------------------------------------------------------------------------------
-- Logging and errors

showScopeEntry :: Show var => Expr NamedBinding var -> Expr NamedBinding var
showScopeEntry e =
  e

showScopeExit :: MonadDescope m => m NamedExpr -> m NamedExpr
showScopeExit m = do
  e <- m
  return e

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: MonadDescope m => Provenance -> DBIndex -> Int -> m a
indexOutOfBounds p index ctxSize = developerError $
  "During descoping found DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize <+> parens (pretty p)
