
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
                => t (CoDBBinding DBBinding)    var
                -> t (CoDBBinding NamedBinding) var
supplyCoDBNames e = snd (supplyCoDBNamesWithCtx (mempty, e))

supplyCoDBNamesWithCtx :: SupplyNames t
                       => ([DBBinding],    t (CoDBBinding DBBinding) var)
                       -> ([NamedBinding], t (CoDBBinding NamedBinding) var)
supplyCoDBNamesWithCtx = supplyNamesWithCtx' convertBinding
  where
    convertBinding :: CoDBBinding DBBinding -> Supply Symbol (CoDBBinding NamedBinding)
    convertBinding (CoDBBinding n pt) = do
      n' <- getName n
      return (CoDBBinding n' pt)

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
    App      ann fun args -> App ann <$> supplyNames f fun <*> traverse (supplyNamesArg f) args
    LVec     ann es       -> LVec ann <$> traverse (supplyNames f) es
    Meta     ann i        -> return $ Meta ann i

    Let ann bound binder body -> Let ann <$> supplyNames f bound <*> supplyNamesBinder f binder <*> supplyNames f body
    Lam ann binder body       -> Lam ann <$> supplyNamesBinder f binder <*> supplyNames f body
    Pi  ann binder body       -> Pi  ann <$> supplyNamesBinder f binder <*> supplyNames f body

instance SupplyNames Arg' where
  supplyNames f e = WrapArg <$> supplyNamesArg f (unwrapArg e)

instance SupplyNames Binder' where
  supplyNames f e = WrapBinder <$> supplyNamesBinder f (unwrapBinder e)

supplyNamesBinder ::  (SupplyNames t, MonadSupply Symbol m)
                  => (binding1 -> m binder)
                  -> GenericBinder binding1 (t binding1 var)
                  -> m (GenericBinder binder (t binder var))
supplyNamesBinder f (Binder p v r n e) = do
    n' <- f n
    e' <- supplyNames f e
    return $ Binder p v r n' e'

supplyNamesArg :: (Traversable t1, SupplyNames t2, MonadSupply Symbol f)
               => (binding1 -> f binding2)
               -> t1 (t2 binding1 var) -> f (t1 (t2 binding2 var))
supplyNamesArg f = traverse (supplyNames f)

