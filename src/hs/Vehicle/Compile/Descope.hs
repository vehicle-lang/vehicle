
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

--------------------------------------------------------------------------------
-- Public interface

-- |Converts DeBruijn variables back into named variables according to the
-- provided context.
runDescope :: Descope t
           => [Symbol]
           -> t Symbol DBVar
           -> t Symbol Symbol
runDescope ctx = performDescoping ctx convertDBVar

-- |Converts DeBruijn variables back into named variables with no context.
runDescopeProg :: Prog Symbol DBVar
               -> Prog Symbol Symbol
runDescopeProg = performDescoping mempty convertDBVar

-- |Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
-- Useful for debugging
runNaiveDBDescope :: Descope t
                  => t Symbol DBVar
                  -> t Symbol Symbol
runNaiveDBDescope = performDescoping mempty convertDBVarNaive

-- |Converts DeBruijn indices into names naively, e.g. 0 becomes "i0".
-- Useful for debugging
runNaiveCoDBDescope :: (Descope t, ExtractPositionTrees t)
                    => t (Symbol, Maybe PositionTree) CoDBVar
                    -> (t Symbol Symbol, Map Symbol (Maybe PositionTree))
runNaiveCoDBDescope e1 =
  let (e2, pts) = extractPTs e1 in
  let e3 = performDescoping mempty convertCoDBVarNaive e2 in
  (e3, pts)

--------------------------------------------------------------------------------
-- Core operation

performDescoping :: (Descope t, Show var)
                 => [NamedBinding]
                 -> (Provenance -> var -> Reader Ctx NamedVar)
                 -> t NamedBinding var
                 -> t NamedBinding NamedVar
performDescoping ctx convertVar e =
  runReader (descope convertVar e) (Ctx ctx)

newtype Ctx = Ctx [NamedBinding]

addBinderToCtx :: NamedBinder -> Ctx -> Ctx
addBinderToCtx binder (Ctx ctx) = Ctx (nameOf binder : ctx)

type MonadDescope m = MonadReader Ctx m

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: MonadDescope m => Provenance -> DBIndex -> Int -> m a
indexOutOfBounds p index ctxSize = developerError $
  "During descoping found DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize <+> parens (pretty p)

convertDBVar :: MonadDescope m => Provenance -> DBVar -> m NamedVar
convertDBVar _ (Free (Identifier name)) = return name
convertDBVar p (Bound i) = do
  Ctx ctx <- ask
  case ctx !!? i of
    Nothing -> indexOutOfBounds p i (length ctx)
    Just x  -> return x

convertDBVarNaive :: MonadDescope m => Provenance -> DBVar -> m NamedVar
convertDBVarNaive _ (Free (Identifier name)) = return name
convertDBVarNaive _ (Bound i)                = return $ pack ("i" <> show i)

convertCoDBVarNaive :: MonadDescope m => Provenance -> CoDBVar -> m NamedVar
convertCoDBVarNaive _ (CoDBFree (Identifier name)) = return name
convertCoDBVarNaive _ CoDBBound                    = return "CoDBVar"

class Descope t where
  descope :: (MonadDescope m, Show var)
          => (Provenance -> var -> m NamedVar)
          -> t NamedBinding var
          -> m (t NamedBinding NamedVar)

instance Descope Binder where
  descope f = traverseBinderType (descope f)

instance Descope Arg where
  descope f = traverseArgExpr (descope f)

showScopeEntry :: Show var => Expr NamedBinding var -> Expr NamedBinding var
showScopeEntry e =
  e

showScopeExit :: MonadDescope m => m NamedExpr -> m NamedExpr
showScopeExit m = do
  e <- m
  return e

instance Descope Expr where
  descope f e = showScopeExit $ case showScopeEntry e of
    Universe  ann l        -> return $ Universe ann l
    Hole     ann name     -> return $ Hole    ann name
    Builtin  ann op       -> return $ Builtin ann op
    Literal  ann l        -> return $ Literal ann l
    Meta     ann i        -> return $ Meta ann i
    Var      ann v        -> Var ann <$> f ann v
    Ann      ann e1 t     -> Ann ann <$> descope f e1 <*> descope f t
    App      ann fun args -> App ann <$> descope f fun <*> traverse (descope f) args
    LSeq     ann es       -> LSeq ann <$> traverse (descope f) es
    PrimDict ann tc       -> PrimDict ann <$> descope f tc

    Let ann bound binder body -> do
      bound'      <- descope f bound
      binder'     <- descope f binder
      body'       <- local (addBinderToCtx binder') (descope f body)
      return $ Let ann bound' binder' body'

    Lam ann binder body -> do
      binder'     <- descope f binder
      body'       <- local (addBinderToCtx binder') (descope f body)
      return $ Lam ann binder' body'

    Pi ann binder body -> do
      binder'     <- descope f binder
      body'       <- local (addBinderToCtx binder') (descope f body)
      return $ Pi ann binder' body'

-- No need to add the declaration identifiers to the ctx, as they
-- are untouched during conversion back from de Bruijn indice's.
-- Therefore the following are not in the state monad.

instance Descope Decl where
  descope f = traverseDeclExprs (descope f)

instance Descope Prog where
  descope f (Main ds) = Main <$> traverse (descope f) ds