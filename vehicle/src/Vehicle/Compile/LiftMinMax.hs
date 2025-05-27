module Vehicle.Compile.LiftMinMax
  ( liftMinMax,
    unfoldIf,
  )
where

import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Context.Name (MonadNameContext, getNameContext)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView (RatTensorValue (..), fromRatTensorValue, toRatTensorValue)
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- If lifting

liftMinMax ::
  (Monad m) =>
  Value Builtin ->
  (Value Builtin -> m (Value Builtin)) ->
  m (Value Builtin)
liftMinMax (toRatTensorValue -> VMinRatTensor args) k =
  fromRatTensorValue . VMinRatTensor <$> traverseTensorOp2Args k args
liftMinMax e k = k e

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
    return $ "elim-if" <+> prettyFriendly (WithContext result nameCtx)
  return result
