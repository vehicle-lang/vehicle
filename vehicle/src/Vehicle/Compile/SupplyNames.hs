module Vehicle.Compile.SupplyNames
  ( SupplyNames (..),
  )
where

import Data.Text qualified as Text (pack)
import Vehicle.Compile.Prelude
-- import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.DeBruijn

--------------------------------------------------------------------------------
-- Public interface

class SupplyNames a b | a -> b where
  supplyNames :: a -> b

instance SupplyNames (Prog DBBinding var) (Prog NamedBinding var) where
  supplyNames = fmap supplyNames

instance SupplyNames (Decl DBBinding var) (Decl NamedBinding var) where
  supplyNames = fmap supplyNames

instance SupplyNames (Expr DBBinding var) (Expr NamedBinding var) where
  supplyNames e = runSupplyNames (supplyNames2 getName e)

instance SupplyNames (Arg DBBinding var) (Arg NamedBinding var) where
  supplyNames = fmap supplyNames

instance SupplyNames (Binder DBBinding var) (Binder NamedBinding var) where
  supplyNames b = runSupplyNames (supplyNamesBinder getName b)

instance SupplyNames (Contextualised (Expr DBBinding var) [DBBinding]) (Contextualised (Expr NamedBinding var) [NamedBinding]) where
  supplyNames e = runSupplyNames (supplyNamesWithCtx getName e)

{-

supplyCoDBNames ::
  SupplyNames2 t =>
  t (CoDBBinding DBBinding) var ->
  t (CoDBBinding NamedBinding) var
supplyCoDBNames e = snd (supplyCoDBNamesWithCtx (mempty, e))

supplyCoDBNamesWithCtx ::
  SupplyNames2 t =>
  ([DBBinding], t (CoDBBinding DBBinding) var) ->
  ([NamedBinding], t (CoDBBinding NamedBinding) var)
supplyCoDBNamesWithCtx = supplyNamesWithCtx' convertBinding
  where
    convertBinding :: CoDBBinding DBBinding -> Supply Name (CoDBBinding NamedBinding)
    convertBinding (CoDBBinding n pt) = do
      n' <- getName n
      return (CoDBBinding n' pt)
-}
--------------------------------------------------------------------------------
-- Core operation

runSupplyNames :: Supply Name a -> a
runSupplyNames x =
  runSupply
    x
    ["_x" <> Text.pack (show i) | i <- [0 :: Int ..]]

type MonadSupplyNames m = MonadSupply Name m

supplyNamesToCtx :: MonadSupply Name m => [DBBinding] -> m [NamedBinding]
supplyNamesToCtx = mapM getName

getName :: MonadSupply Name m => DBBinding -> m Name
getName = maybe demand return

class SupplyNames2 t where
  supplyNames2 ::
    MonadSupplyNames m =>
    (binding1 -> m binding2) ->
    t binding1 var ->
    m (t binding2 var)

  supplyNamesWithCtx ::
    MonadSupplyNames m =>
    (binding1 -> m binding2) ->
    Contextualised (t binding1 var) [DBBinding] ->
    m (Contextualised (t binding2 var) [Name])
  supplyNamesWithCtx f (WithContext e ctx) = do
    ctx' <- supplyNamesToCtx ctx
    e' <- supplyNames2 f e
    return (WithContext e' ctx')

instance SupplyNames2 Expr where
  supplyNames2 f e = case e of
    Universe ann l -> return (Universe ann l)
    Hole ann name -> return (Hole ann name)
    Builtin ann op -> return (Builtin ann op)
    Literal ann l -> return (Literal ann l)
    Var ann v -> return $ Var ann v
    Ann ann e1 t -> Ann ann <$> supplyNames2 f e1 <*> supplyNames2 f t
    App ann fun args -> App ann <$> supplyNames2 f fun <*> traverse (supplyNamesArg f) args
    LVec ann es -> LVec ann <$> traverse (supplyNames2 f) es
    Meta ann i -> return $ Meta ann i
    Let ann bound binder body -> Let ann <$> supplyNames2 f bound <*> supplyNamesBinder f binder <*> supplyNames2 f body
    Lam ann binder body -> Lam ann <$> supplyNamesBinder f binder <*> supplyNames2 f body
    Pi ann binder body -> Pi ann <$> supplyNamesBinder f binder <*> supplyNames2 f body

supplyNamesBinder ::
  (SupplyNames2 t, MonadSupply Name m) =>
  (binding1 -> m binder) ->
  GenericBinder binding1 (t binding1 var) ->
  m (GenericBinder binder (t binder var))
supplyNamesBinder f (Binder p u v r n e) = do
  n' <- f n
  e' <- supplyNames2 f e
  return $ Binder p u v r n' e'

supplyNamesArg ::
  (Traversable t1, SupplyNames2 t2, MonadSupply Name f) =>
  (binding1 -> f binding2) ->
  t1 (t2 binding1 var) ->
  f (t1 (t2 binding2 var))
supplyNamesArg f = traverse (supplyNames2 f)
