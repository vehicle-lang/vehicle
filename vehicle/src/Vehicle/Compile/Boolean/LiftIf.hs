module Vehicle.Compile.Boolean.LiftIf
  ( liftIf,
    liftIfArg,
    liftIfSpine,
    unfoldIf,
  )
where

import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Value

--------------------------------------------------------------------------------
-- If lifting

liftIf ::
  (Monad m) =>
  (WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
liftIf k (IIf t cond e1 e2) = IIf t cond <$> liftIf k e1 <*> liftIf k e2
liftIf k e = k e

liftIfArg ::
  (Monad m) =>
  (WHNFArg Builtin -> m (WHNFValue Builtin)) ->
  WHNFArg Builtin ->
  m (WHNFValue Builtin)
liftIfArg k (Arg p v r e) = liftIf (k . Arg p v r) e

liftIfSpine ::
  (Monad m) =>
  WHNFSpine Builtin ->
  (WHNFSpine Builtin -> m (WHNFValue Builtin)) ->
  m (WHNFValue Builtin)
liftIfSpine [] k = k []
liftIfSpine (x : xs) k = liftIfArg (\a -> liftIfSpine xs (\as -> k (a : as))) x

unfoldIf ::
  (Monad m, MonadFreeContext Builtin m) =>
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unfoldIf c x y = do
  let mkArgs = fmap (Arg mempty Explicit Relevant)
  cAndX <- normaliseBuiltin (BuiltinFunction And) (mkArgs [c, x])
  notC <- normaliseBuiltin (BuiltinFunction Not) (mkArgs [c])
  notCAndY <- normaliseBuiltin (BuiltinFunction And) (mkArgs [notC, y])
  normaliseBuiltin (BuiltinFunction Or) (mkArgs [cAndX, notCAndY])
