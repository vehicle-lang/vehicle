module Vehicle.Backend.LossFunction.LossCompilation
  ( convertValue,
    convertExpr,
    MonadLossCtx,
    runMonadLogicT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Backend.LossFunction.Core
import Vehicle.Backend.LossFunction.Core qualified as L
import Vehicle.Backend.LossFunction.Domain (Domain (..), extractSearchDomain)
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval, evalApp, traverseClosure)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Data.Builtin.Core (Builtin (..))
import Vehicle.Data.Builtin.Core qualified as S
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.Interface (TensorOp1Args (..), mkDims, pattern INatLiteral, pattern INatType)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value (BoundEnv, Closure (..), Spine, VArg, VBinder, Value (..), boundContextToEnv, extendEnvWithBound, traverseSpine)
import Vehicle.Data.Tensor (Tensor, foldMapTensor)

--------------------------------------------------------------------------------
-- Monad

type MonadLossCtx =
  ( DeclProvenance,
    CompiledDifferentiableLogic
  )

type MonadLogic m =
  ( MonadCompile m,
    MonadNameContext m,
    MonadReader MonadLossCtx m
  )

runMonadLogicT ::
  (MonadCompile m, MonadNameContext m) =>
  CompiledDifferentiableLogic ->
  DeclProvenance ->
  ReaderT MonadLossCtx m a ->
  m a
runMonadLogicT logic origin =
  flip runReaderT (origin, logic)

getLogic :: (MonadLogic m) => m DifferentiableLogicImplementation
getLogic = do
  (_, (_, logic)) <- ask
  return logic

getDeclProvenance :: (MonadLogic m) => m DeclProvenance
getDeclProvenance = do
  (prov, _) <- ask
  return prov

getLogicField :: (MonadLogic m) => TensorDifferentiableLogicField -> m (Value LossBuiltin)
getLogicField field = do
  logic <- getLogic
  lookupLogicField field logic

lookupLogicField :: (MonadCompile m, Ord field, Pretty field) => field -> Map field value -> m value
lookupLogicField field logic = do
  case Map.lookup field logic of
    Nothing -> compilerDeveloperError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
    Just value -> return value

--------------------------------------------------------------------------------
-- Conversion

convertExpr ::
  (MonadLogic m) =>
  BoundEnv Builtin ->
  Expr Builtin ->
  m (Value LossBuiltin)
convertExpr env expr = convertValue =<< eval mempty env expr

convertValue ::
  forall m.
  (MonadLogic m) =>
  Value Builtin ->
  m (Value LossBuiltin)
convertValue e = do
  showEntry e
  result <- case e of
    VMeta {} ->
      unexpectedExprError currentPass "VMeta"
    VUniverse l ->
      return $ VUniverse l
    VFreeVar v spine -> do
      args' <- traverseArgs convertValue spine
      return $ VFreeVar v args'
    VBoundVar v spine -> do
      VBoundVar v <$> traverseArgs convertValue spine
    VBuiltin b spine -> do
      convertBuiltinToLoss b spine
    VPi binder closure -> do
      binder' <- traverse convertValue binder
      closure' <- traverseClosure convertValue mempty binder closure
      return $ VPi binder' closure'
    VLam binder closure -> do
      binder' <- traverse convertValue binder
      closure' <- traverseClosure convertValue mempty binder closure
      return $ VLam binder' closure'
  showExit result
  return result

convertBuiltinToLoss ::
  forall m.
  (MonadLogic m) =>
  Builtin ->
  Spine Builtin ->
  m (Value LossBuiltin)
convertBuiltinToLoss b spine = case b of
  S.TypeClassOp {} -> unexpectedExprError currentPass (pretty b)
  S.TypeClass {} -> unexpectedExprError currentPass (pretty b)
  S.NatInDomainConstraint -> unexpectedExprError currentPass (pretty b)
  S.BuiltinConstructor c -> case c of
    S.BoolTensorLiteral t -> translateConstant t
    S.Nil -> unchangedConstructor Nil
    S.Cons -> unchangedConstructor Nil
    S.UnitLiteral -> unchangedConstructor UnitLiteral
    S.IndexLiteral x -> unchangedConstructor $ IndexLiteral x
    S.NatLiteral x -> unchangedConstructor $ NatLiteral x
    S.NatTensorLiteral x -> unchangedConstructor $ NatTensorLiteral x
    S.RatTensorLiteral x -> unchangedConstructor $ RatTensorLiteral x
    S.VectorLiteral -> unsupportedBuiltin
  S.BuiltinType t -> case t of
    S.BoolType -> return $ VBuiltin (LossBuiltinType RatType) []
    S.RatType -> unchangedType RatType
    S.UnitType -> unchangedType UnitType
    S.IndexType -> unchangedType IndexType
    S.NatType -> unchangedType NatType
    S.ListType -> unchangedType ListType
    S.VectorType -> developerError "Vector not yet supported"
    S.TensorType -> unchangedType TensorType
  S.BuiltinFunction f -> case f of
    --------------
    -- Booleans --
    --------------
    S.Not -> changedBuiltin L.PointwiseNegation
    S.And -> changedBuiltin L.PointwiseConjunction
    S.Or -> changedBuiltin L.PointwiseDisjunction
    S.CompareRatTensorPointwise Eq -> changedBuiltin L.PointwiseEq
    S.CompareRatTensorPointwise Ne -> changedBuiltin L.PointwiseNe
    S.CompareRatTensorPointwise Lt -> changedBuiltin L.PointwiseLt
    S.CompareRatTensorPointwise Le -> changedBuiltin L.PointwiseLe
    S.CompareRatTensorPointwise Gt -> changedBuiltin L.PointwiseGt
    S.CompareRatTensorPointwise Ge -> changedBuiltin L.PointwiseGe
    S.ReduceAndTensor -> changedBuiltin L.ReduceConjunction
    S.ReduceOrTensor -> changedBuiltin L.ReduceDisjunction
    S.QuantifyRatTensor q -> translateQuantifier q spine
    --------------
    -- Unsupported --
    --------------
    S.Implies -> unexpectedExprError currentPass (pretty b)
    S.If -> unsupportedBuiltin
    -----------
    -- Other --
    -----------
    S.Neg dom -> unchangedFunction (Neg dom)
    S.Add dom -> unchangedFunction (Add dom)
    S.Sub dom -> unchangedFunction (Sub dom)
    S.Mul dom -> unchangedFunction (Mul dom)
    S.Div dom -> unchangedFunction (Div dom)
    S.Min dom -> unchangedFunction (Min dom)
    S.Max dom -> unchangedFunction (Max dom)
    S.PowRat -> unchangedFunction PowRat
    S.ReduceAddRatTensor -> unchangedFunction ReduceAddRatTensor
    S.ReduceMulRatTensor -> unchangedFunction ReduceMulRatTensor
    S.ReduceMinRatTensor -> unchangedFunction ReduceMinRatTensor
    S.ReduceMaxRatTensor -> unchangedFunction ReduceMaxRatTensor
    S.AtTensor -> unchangedFunction At
    S.StackTensor -> unchangedFunction StackTensor
    S.ConstTensor -> unchangedFunction ConstTensor
    S.ForeachVector -> developerError "Conversion of `foreach` not yet supported"
    S.ForeachTensor -> developerError "Conversion of `foreach` not yet supported"
    -----------------
    -- Unsupported --
    -----------------
    S.CompareNat {} -> unsupportedBuiltin
    S.CompareIndex {} -> unsupportedBuiltin
    S.FoldList -> unsupportedBuiltin
    S.MapList -> unsupportedBuiltin
    S.Iterate -> unsupportedBuiltin
    S.AtVector -> unsupportedBuiltin
  S.BuiltinCast {} -> unsupportedBuiltin
  S.DerivedFunction {} -> unsupportedBuiltin
  ----------------------
  -- Other operations --
  ----------------------
  where
    changedBuiltin :: TensorDifferentiableLogicField -> m (Value LossBuiltin)
    changedBuiltin field = substField field =<< traverseSpine convertValue spine

    unchangedBuiltin :: LossBuiltin -> m (Value LossBuiltin)
    unchangedBuiltin op = VBuiltin op <$> traverseSpine convertValue spine

    unchangedFunction :: LossBuiltinFunction -> m (Value LossBuiltin)
    unchangedFunction op = unchangedBuiltin (LossBuiltinFunction op)

    unchangedConstructor :: LossBuiltinConstructor -> m (Value LossBuiltin)
    unchangedConstructor op = unchangedBuiltin (LossBuiltinConstructor op)

    unchangedType :: LossBuiltinType -> m (Value LossBuiltin)
    unchangedType op = unchangedBuiltin (LossBuiltinType op)

    unsupportedBuiltin :: m a
    unsupportedBuiltin = do
      (declProv, _) <- ask
      throwError $ UnsupportedLossOperation declProv mempty (pretty S.If)

substField :: (MonadLogic m) => TensorDifferentiableLogicField -> Spine LossBuiltin -> m (Value LossBuiltin)
substField field spine = do
  fn <- getLogicField field
  logDebug MaxDetail $ "subst-field" <+> pretty field <> ":" <+> prettyFriendlyEmptyCtx fn
  evalApp mempty fn spine

translateConstant :: (MonadLogic m) => Tensor Bool -> m (Value LossBuiltin)
translateConstant tensor = do
  trueExpr <- getLogicField L.TruthityElement
  falseExpr <- getLogicField L.FalsityElement

  let convertBool b = if b then trueExpr else falseExpr
  let foldLayer shape elems = do
        let dim = length elems
        let dims = implicitIrrelevant (mkDims shape)
        let args = implicit (INatLiteral dim) : dims : implicit INatType : fmap explicit elems
        VBuiltin (LossBuiltinFunction StackTensor) args
  return $ foldMapTensor convertBool foldLayer tensor

translateQuantifier :: (MonadLogic m) => Quantifier -> Spine Builtin -> m (Value LossBuiltin)
translateQuantifier q = \case
  [dims, argExpr -> VLam binder (Closure env body)] -> do
    -- Normalise the body
    lv <- getBinderDepth
    let newEnv = extendEnvWithBound lv binder env
    bodyValue <- eval mempty newEnv body

    -- Translate the dimensions
    lossDims <- traverse convertValue dims

    case q of
      Forall -> translateForall dims lossDims binder bodyValue
      Exists -> translateExists lossDims binder bodyValue
  spine -> unexpectedExprError currentPass ("quantifier spine:" <> prettyVerbose spine)

translateForall ::
  (MonadLogic m) =>
  VArg Builtin ->
  VArg LossBuiltin ->
  VBinder Builtin ->
  Value Builtin ->
  m (Value LossBuiltin)
translateForall dims lossDims binder body = do
  let newBody = fromBoolValue $ VNot $ TensorOp1Args dims body
  result <- translateExists lossDims binder newBody
  substField L.PointwiseNegation [lossDims, explicit result]

translateExists ::
  (MonadLogic m) =>
  VArg LossBuiltin ->
  VBinder Builtin ->
  Value Builtin ->
  m (Value LossBuiltin)
translateExists lossDims binder bodyValue = logCompilerSection MaxDetail "convert-exists" $ do
  boundCtx <- getBinderContext
  let lv = boundCtxLv boundCtx

  -- Convert the binder and the dimensions.
  lossBinder <- traverse convertValue binder

  -- Generate the operation for doing the reduction
  genericReductionOp <- getLogicField ReduceDisjunction
  reductionOp <- evalApp mempty genericReductionOp [lossDims]

  -- Extract the domain for the search
  declProv <- getDeclProvenance
  (Domain {..}, normTensorBody) <- extractSearchDomain declProv binder lv bodyValue

  -- Convert the domain to a loss value.
  lossLowerBounds <- convertValue lowerBound
  lossUpperBounds <- convertValue upperBound

  normLossBody <- addNameToContext binder $ convertValue normTensorBody

  -- Reform the closure
  let lossBody = quote mempty (lv + 1) normLossBody
  let finalEnv = boundContextToEnv boundCtx
  let lossPredicate = VLam lossBinder (Closure finalEnv lossBody)

  let newArgs = lossDims : (explicit <$> [reductionOp, lossLowerBounds, lossUpperBounds, lossPredicate])

  return $ VBuiltin (LossBuiltinFunction SearchRatTensor) newArgs

--------------------------------------------------------------------------------
-- Utils

currentPass :: CompilerPass
currentPass = "logic translation"

showEntry :: (MonadLogger m, MonadNameContext m) => Value Builtin -> m ()
showEntry e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ doc <+> ":" <+> prettyVerbose e
  logDebug MaxDetail $ "enter-loss" <+> ":" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLogger m, MonadNameContext m) => Value LossBuiltin -> m ()
showExit e = do
  ctx <- getNameContext
  decrCallDepth
  logDebug MaxDetail $ "exit-loss" <+> ": " <+> prettyFriendly (WithContext e ctx)
