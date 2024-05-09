module Vehicle.Backend.Queries.UserVariableElimination.EliminateIf
  ( liftIf,
    liftIfArg,
    liftIfSpine,
    unfoldIf,
  )
where

import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised

--------------------------------------------------------------------------------
-- If lifting

liftIf ::
  (MonadCompile m) =>
  (WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
liftIf k (IIf t cond e1 e2) = IIf t cond <$> liftIf k e1 <*> liftIf k e2
liftIf k e = k e

liftIfArg ::
  (MonadCompile m) =>
  (WHNFArg Builtin -> m (WHNFValue Builtin)) ->
  WHNFArg Builtin ->
  m (WHNFValue Builtin)
liftIfArg k (Arg p v r e) = liftIf (k . Arg p v r) e

liftIfSpine ::
  (MonadUnblock m) =>
  WHNFSpine Builtin ->
  (WHNFSpine Builtin -> m (WHNFValue Builtin)) ->
  m (WHNFValue Builtin)
liftIfSpine [] k = k []
liftIfSpine (x : xs) k = liftIfArg (\a -> liftIfSpine xs (\as -> k (a : as))) x

{-
-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: WHNFValue Builtin -> WHNFValue Builtin
elimIf (IIf _ cond e1 e2) = unfoldIf cond (elimIf e1) (elimIf e2)
elimIf e = e
-}
unfoldIf ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
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
