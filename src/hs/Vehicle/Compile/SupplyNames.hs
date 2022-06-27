
module Vehicle.Compile.SupplyNames
  ( SupplyNames
  , supplyDBNames
  , supplyDBNamesWithCtx
  , supplyCoDBNames
  , supplyCoDBNamesWithCtx
  ) where

import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Public interface

supplyDBNames :: SupplyNames t
              => t DBBinding    var
              -> t NamedBinding var
supplyDBNames e = snd (supplyDBNamesWithCtx (mempty, e))

supplyDBNamesWithCtx :: SupplyNames t
                     => ([DBBinding],    t DBBinding    var)
                     -> ([NamedBinding], t NamedBinding var)
supplyDBNamesWithCtx = supplyNamesWithCtx' getName

supplyCoDBNames :: SupplyNames t
                => t CoDBBinding var
                -> t (Symbol, Maybe PositionTree) var
supplyCoDBNames e = snd (supplyCoDBNamesWithCtx (mempty, e))

supplyCoDBNamesWithCtx :: SupplyNames t
                       => ([DBBinding], t CoDBBinding var)
                       -> ([NamedBinding], t (Symbol, Maybe PositionTree) var)
supplyCoDBNamesWithCtx = supplyNamesWithCtx' convertBinding
  where
    convertBinding :: CoDBBinding -> Supply Symbol (Symbol, Maybe PositionTree)
    convertBinding (CoDBBinding n pt) = do n' <- getName n; return (n', pt)

--------------------------------------------------------------------------------
-- Core operation

supplyNamesWithCtx' :: (SupplyNames t)
             => (binding1 -> Supply Symbol binding2)
             -> ([DBBinding],    t binding1 var)
             -> ([NamedBinding], t binding2 var)
supplyNamesWithCtx' f v = runSupply (supplyNamesWithCtx f v) freshNames

supplyNamesToCtx :: MonadSupply Symbol m => [DBBinding] -> m [NamedBinding]
supplyNamesToCtx = mapM getName

getName :: MonadSupply Symbol m => DBBinding -> m Symbol
getName = maybe demand return

type MonadSupplyNames m = MonadSupply Symbol m

class SupplyNames t where
  supplyNames :: MonadSupplyNames m
              => (binding1 -> m binding2)
              -> t binding1 var
              -> m (t binding2 var)

  supplyNamesWithCtx :: MonadSupplyNames m
                     => (binding1 -> m binding2)
                     -> ([DBBinding], t binding1 var)
                     -> m ([Symbol], t binding2 var)
  supplyNamesWithCtx f (ctx, e) = do
    ctx' <- supplyNamesToCtx ctx
    e'   <- supplyNames f e
    return (ctx', e')

instance SupplyNames Prog where
  supplyNames f (Main ds) = Main <$> traverse (supplyNames f) ds

instance SupplyNames Decl where
  supplyNames f = traverseDeclExprs (supplyNames f)

instance SupplyNames Expr where
  supplyNames f e = case e of
    Universe ann l        -> return (Universe ann l)
    Hole     ann name     -> return (Hole ann name)
    Builtin  ann op       -> return (Builtin ann op)
    Literal  ann l        -> return (Literal ann l)
    Var      ann v        -> return $ Var ann v
    Ann      ann e1 t     -> Ann ann <$> supplyNames f e1 <*> supplyNames f t
    App      ann fun args -> App ann <$> supplyNames f fun <*> traverse (supplyNames f) args
    LSeq     ann es       -> LSeq ann <$> traverse (supplyNames f) es
    PrimDict ann tc       -> PrimDict ann <$> supplyNames f tc
    Meta     ann i        -> return $ Meta ann i

    Let ann bound binder body -> Let ann <$> supplyNames f bound <*> supplyNames f binder <*> supplyNames f body
    Lam ann binder body       -> Lam ann <$> supplyNames f binder <*> supplyNames f body
    Pi  ann binder body       -> Pi  ann <$> supplyNames f binder <*> supplyNames f body

instance SupplyNames Binder where
  supplyNames f (Binder ann v n e) = do
    n' <- f n
    e' <- supplyNames f e
    return $ Binder ann v n' e'

instance SupplyNames Arg where
  supplyNames f = traverseArgExpr (supplyNames f)

