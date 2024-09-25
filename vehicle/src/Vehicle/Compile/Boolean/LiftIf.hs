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
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- If lifting

liftIf ::
  (Monad m) =>
  WHNFValue Builtin ->
  (WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  m (WHNFValue Builtin)
liftIf (IIf t cond e1 e2) k = IIf t cond <$> liftIf e1 k <*> liftIf e2 k
liftIf e k = k e

liftIfArg ::
  (Monad m) =>
  WHNFArg Builtin ->
  (WHNFArg Builtin -> m (WHNFValue Builtin)) ->
  m (WHNFValue Builtin)
liftIfArg (Arg p v r e) k = liftIf e (k . Arg p v r)

liftIfSpine ::
  (Monad m) =>
  WHNFSpine Builtin ->
  (WHNFSpine Builtin -> m (WHNFValue Builtin)) ->
  m (WHNFValue Builtin)
liftIfSpine [] k = k []
liftIfSpine (x : xs) k = liftIfArg x (\a -> liftIfSpine xs (\as -> k (a : as)))

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
