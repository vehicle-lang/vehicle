module Vehicle.Data.Builtin.Interface.TensorFusion
  ( fuseReduceAndTensor,
  )
where

import Data.Maybe (fromMaybe)
import Vehicle.Compile.Context.Free (MonadFreeContext, getFreeEnv)
import Vehicle.Compile.Context.Name (MonadNameContext, getNameContext)
import Vehicle.Compile.Normalise.NBE (eval, evalApp)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value

-----------------------------------------------------------------------------
-- Main entry point

type MonadFuse m =
  ( MonadNameContext m,
    MonadFreeContext Builtin m
  )

fuseReduceAndTensor ::
  (MonadFuse m) =>
  TensorOp2Args (Value Builtin) ->
  m (Value Builtin)
fuseReduceAndTensor args@(TensorOp2Args _ e@(IBoolLiteral True) tensor) = do
  freeEnv <- getFreeEnv
  nameCtx <- getNameContext
  result <- fuseForeachTensor freeEnv nameCtx tensor
  case result of
    Nothing -> return $ mkExpr accessReduceAnd args
    Just (newDims, fusedTensor) ->
      evalReduceAndTensor (TensorOp2Args newDims e fusedTensor)
fuseReduceAndTensor args = return $ mkExpr accessReduceAnd args

-- | An optimised evaluation procedure for `Foreach` that attempts to minimise the
-- amount of work needed by lifting operations to higher-tensor levels.
-- For example `foreach i . xs ! i + ys ! i` becomes `xs + ys`.
fuseForeachTensor ::
  (MonadLogger m) =>
  FreeEnv Builtin ->
  NamedBoundCtx ->
  Value Builtin ->
  m (Maybe (VArg Builtin, Value Builtin))
fuseForeachTensor freeEnv ctx value = do
  fusionEnter ctx value
  fusionExit ctx =<< case getExpr accessForeachTensor value of
    Just (ForeachTensorArgs typ d _ (VLam binder (Closure env body))) -> do
      let lv = boundCtxLv ctx
      let newEnv = extendEnvWithBound lv binder env
      let newCtx = nameOf binder : ctx
      body' <- eval freeEnv newCtx newEnv body
      case getExpr accessReduceAnd body' of
        Just (TensorOp2Args tensorDims (IBoolLiteral True) tensor) -> do
          (newDims, newTensor) <- fromMaybe (tensorDims, tensor) <$> fuseForeachTensor freeEnv newCtx tensor
          let newTensor' = quote mempty (lv + 1) newTensor
          let newLam = VLam binder (Closure (namedBoundContextToEnv ctx) newTensor')
          let newForeachArgs = ForeachTensorArgs typ d newDims newLam
          newBody' <- evalForeachTensor newCtx (evalApp freeEnv) (eval freeEnv) newForeachArgs
          return $ Just (implicit (ICons (implicit INatType) d (argExpr newDims)), newBody')
        _ -> return Nothing
    _ -> return Nothing

fusionEnter :: (MonadLogger m) => NamedBoundCtx -> Value Builtin -> m ()
fusionEnter ctx value = do
  logDebug MaxDetail $ "fusion-enter" <+> prettyFriendly (WithContext value ctx)
  incrCallDepth

fusionExit :: (MonadLogger m) => NamedBoundCtx -> Maybe (VArg Builtin, Value Builtin) -> m (Maybe (VArg Builtin, Value Builtin))
fusionExit ctx result = do
  decrCallDepth
  logDebug MaxDetail $
    "fusion-exit" <+> case result of
      Nothing -> ""
      Just (dims, value) -> prettyFriendly (WithContext value ctx) <+> parens (prettyFriendly (WithContext (argExpr dims) ctx))
  return result
