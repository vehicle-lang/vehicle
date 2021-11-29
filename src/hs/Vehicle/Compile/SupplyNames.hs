
module Vehicle.Compile.SupplyNames
  ( SupplyNames
  , runSupplyNames
  , runSupplyNamesWithCtx
  ) where

import Vehicle.Prelude
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Public interface

runSupplyNames :: forall t var ann . SupplyNames t
               => t (Maybe Symbol) var ann
               -> t Symbol         var ann
runSupplyNames e = snd (runSupplyNamesWithCtx (mempty, e))

runSupplyNamesWithCtx :: forall t var ann . SupplyNames t
                      => ([Maybe Symbol], t (Maybe Symbol) var ann)
                      -> ([Symbol],       t Symbol         var ann)
runSupplyNamesWithCtx (ctx, e) = runSupply supplyAction freshNames
  where
    supplyAction = do
      ctx' <- supplyNamesToCtx ctx
      e'   <- supplyNames e
      return (ctx', e')

--------------------------------------------------------------------------------
-- Core operation

getName :: MonadSupply Symbol m => Maybe Symbol -> m Symbol
getName = maybe demand return

supplyNamesToCtx :: MonadSupply Symbol m => [Maybe Symbol] -> m [Symbol]
supplyNamesToCtx = mapM getName

type MonadSupplyNames m = MonadSupply Symbol m

class SupplyNames t where
  supplyNames :: MonadSupplyNames m
              => t (Maybe Symbol) var ann
              -> m (t Symbol var ann)

instance SupplyNames Binder where
  supplyNames (Binder ann v n e) = do
    n' <- getName n
    e' <- supplyNames e
    return $ Binder ann v n' e'

instance SupplyNames Arg where
  supplyNames = traverseArgExpr supplyNames

instance SupplyNames Expr where
  supplyNames e = case e of
    Type     l            -> return (Type l)
    Hole     p name       -> return (Hole p name)
    Builtin  ann op       -> return (Builtin ann op)
    Literal  ann l        -> return (Literal ann l)
    Var      ann v        -> return $ Var ann v
    Ann      ann e1 t     -> Ann ann <$> supplyNames e1 <*> supplyNames t
    App      ann fun args -> App ann <$> supplyNames fun <*> traverse supplyNames args
    Seq      ann es       -> Seq ann <$> traverse supplyNames es
    PrimDict ann tc       -> PrimDict ann <$> supplyNames tc
    Meta     ann i        -> return $ Meta ann i

    Let ann bound binder body -> Let ann <$> supplyNames bound <*> supplyNames binder <*> supplyNames body
    Lam ann binder body       -> Lam ann <$> supplyNames binder <*> supplyNames body
    Pi  ann binder body       -> Pi  ann <$> supplyNames binder <*> supplyNames body

instance SupplyNames Decl where
  supplyNames = \case
    DeclNetw p n t   -> DeclNetw p n <$> supplyNames t
    DeclData p n t   -> DeclData p n <$> supplyNames t
    DefFun   p n t e -> DefFun   p n <$> supplyNames t <*> supplyNames e

instance SupplyNames Prog where
  supplyNames (Main ds) = Main <$> traverse supplyNames ds