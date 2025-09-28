module Vehicle.Compile.LiftIf
  ( liftIf,
    liftIfValues,
    unfoldIf,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Name (MonadNameContext, getNameContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

--------------------------------------------------------------------------------
-- If lifting

pattern IIf :: VArg Builtin -> VArg Builtin -> Value Builtin -> Value Builtin -> Value Builtin
pattern IIf t c x y <- VBuiltin (BuiltinFunction If) [t, c, argExpr -> x, argExpr -> y]
  where
    IIf t c x y = VBuiltin (BuiltinFunction If) [t, c, explicit x, explicit y]

liftIf ::
  (Monad m) =>
  Value Builtin ->
  (Value Builtin -> m (Value Builtin)) ->
  m (Value Builtin)
liftIf (IIf t cond e1 e2) k = IIf t cond <$> liftIf e1 k <*> liftIf e2 k
liftIf e k = k e

liftIfValues ::
  (Monad m) =>
  [Value Builtin] ->
  ([Value Builtin] -> m (Value Builtin)) ->
  m (Value Builtin)
liftIfValues [] k = k []
liftIfValues (x : xs) k = liftIf x (\a -> liftIfValues xs (\as -> k (a : as)))

unfoldIf ::
  (Monad m, MonadNameContext m, MonadFreeContext Builtin m) =>
  IfArgs (Value Builtin) ->
  m (Value Builtin)
unfoldIf (IfArgs _ c x y) = do
  let dims = implicitIrrelevant (mkDims [])
  cAndX <- evalAnd (TensorOp2Args dims c x)
  notC <- evalNot (TensorOp1Args dims c)
  notCAndY <- evalAnd (TensorOp2Args dims notC y)
  result <- evalOr (TensorOp2Args dims cAndX notCAndY)
  logDebugM MaxDetail $ do
    nameCtx <- getNameContext
    return $ "unfold-if" <+> prettyFriendly (WithContext result nameCtx)
  return result
