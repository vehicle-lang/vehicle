module Vehicle.Compile.LiftIf
  ( unfoldIf,
  )
where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Force ()
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

--------------------------------------------------------------------------------
-- If lifting

unfoldIf ::
  (MonadLogger m, MonadReadableNameContext m, MonadFreeContext Builtin m) =>
  IfArgs (Thunk Builtin) ->
  m (Thunk Builtin)
unfoldIf (IfArgs _ c x y) = do
  let dims = Forced $ mkDims []
  cAndX <- forceEvaluation accessAndTensor evalAnd (TensorOp2Args dims c x)
  notC <- forceEvaluation accessNotTensor evalNot (TensorOp1Args dims c)
  notCAndY <- forceEvaluation accessAndTensor evalAnd (TensorOp2Args dims notC y)
  result <- forceEvaluation accessOrTensor evalOr (TensorOp2Args dims cAndX notCAndY)
  logDebugM MaxDetail $ do
    nameCtx <- getNameContext
    return $ "unfold-if" <+> prettyFriendly (WithContext result nameCtx)
  return result
