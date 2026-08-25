module Vehicle.Backend.Loss.LossCompilation
  ( convertThunk,
    convertBoolTensorLiteral,
    convertQuantifierlessExprToLoss,
    convertBooleanOp,
    orLossValue,
    andLossValue,
    notLossValue,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Backend.Loss.Core hiding (currentPass)
import Vehicle.Compile.Error (CompileError (UnsupportedLossOperation))
import Vehicle.Compile.Normalise.Force (forceApplication, forceFreeVar, forceThunk)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.Data.Builtin.Standard qualified as S
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Tensor (Tensor, foldMapTensor, shapeOf)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level (findSliceIndices)

type QuantifierHandling m = Maybe ((Quantifier, QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) -> m (Thunk LossBuiltin))

convertQuantifierlessExprToLoss :: (MonadLogic m) => Thunk Builtin -> m (Thunk LossBuiltin)
convertQuantifierlessExprToLoss = convertThunk Nothing

convertThunk ::
  forall m.
  (MonadLogic m) =>
  QuantifierHandling m ->
  Thunk Builtin ->
  m (Thunk LossBuiltin)
convertThunk quantifiers = go
  where
    go :: Thunk Builtin -> m (Thunk LossBuiltin)
    go value = logConversion2 value $ do
      forcedValue <- forceAndRewriteTensor value
      case forcedValue of
        VPi binder body -> convertPi binder body
        VLam binder body -> convertLam binder body
        VFreeVar ident spine -> convertFreeVar ident spine
        VBoundVar v spine -> convertBoundVar v spine
        VBuiltin b spine -> convertBuiltin b spine
        VRecordAcc typ record field args -> Forced <$> (VRecordAcc <$> go typ <*> go record <*> pure field <*> traverseArgs go args)
        VRecord typ fields -> Forced <$> (VRecord <$> go typ <*> traverse go fields)
        VUniverse l -> return $ Forced $ VUniverse l

    convertBuiltin ::
      (MonadLogic m) =>
      Builtin ->
      UnforcedSpine Builtin ->
      m (Thunk LossBuiltin)
    convertBuiltin b args = case b of
      BuiltinType t -> case t of
        S.UnitType -> mkType UnitType
        S.BoolType -> return $ Forced IRatType
        S.IndexType -> mkType IndexType
        S.NatType -> mkType NatType
        S.RatType -> mkType RatType
        S.ListType -> mkType ListType
        S.VectorType -> mkType VectorType
        S.TensorType -> mkType TensorType
      BuiltinConstructor c -> case c of
        S.Nil -> mkConstructor Nil
        S.Cons -> mkConstructor Cons
        S.UnitLiteral -> mkConstructor UnitLiteral
        S.IndexLiteral n -> mkConstructor $ IndexLiteral n
        S.NatLiteral n -> mkConstructor $ NatLiteral n
        S.VectorLiteral -> mkConstructor VectorLiteral
        S.BoolTensorLiteral t -> Forced <$> convertBoolTensorLiteral t
        S.NatTensorLiteral t -> mkConstructor $ NatTensorLiteral t
        S.RatTensorLiteral t -> mkConstructor $ RatTensorLiteral t
      BuiltinFunction f -> case f of
        S.Not -> convertBoolOp PointwiseNegation
        S.And -> convertBoolOp PointwiseConjunction
        S.Or -> convertBoolOp PointwiseDisjunction
        S.QuantifyRatTensor q -> convertQuantifiers quantifiers q args
        S.QuantifyRecord {} -> unexpectedOperation "quantifier"
        S.CompareIndex {} -> unsupportedOperation (pretty b)
        S.CompareNat {} -> unsupportedOperation (pretty b)
        S.CompareRatTensor op -> convertRatTensorComparison op args
        S.ReduceAndTensor -> convertBoolOp ReduceConjunction
        S.ReduceOrTensor -> convertBoolOp ReduceDisjunction
        S.Add dom -> mkFunction $ Add dom
        S.Mul dom -> mkFunction $ Mul dom
        S.Neg dom -> mkFunction $ Neg dom
        S.Sub dom -> mkFunction $ Sub dom
        S.Div dom -> mkFunction $ Div dom
        S.Min dom -> mkFunction $ Min dom
        S.Max dom -> mkFunction $ Max dom
        S.Pow dom -> mkFunction $ Pow dom
        S.Log dom -> mkFunction $ Log dom
        S.Exp dom -> mkFunction $ Exp dom
        S.ReduceAddRatTensor -> mkFunction ReduceAddRatTensor
        S.ReduceMulRatTensor -> mkFunction ReduceMulRatTensor
        S.ReduceMinRatTensor -> mkFunction ReduceMinRatTensor
        S.ReduceMaxRatTensor -> mkFunction ReduceMaxRatTensor
        S.AtTensor -> mkFunction AtTensor
        S.StackTensor -> mkFunction StackTensor
        S.ConstTensor -> mkFunction ConstTensor
        S.Transpose -> mkFunction Transpose
        S.ForeachTensor -> mkFunction ForeachTensor
        S.AtVector -> mkFunction AtVector
        S.ForeachVector -> mkFunction ForeachVector
        S.FoldList -> mkFunction FoldList
        S.MapList -> mkFunction MapList
        S.ReverseList -> mkFunction ReverseList
        S.AppendList -> mkFunction AppendList
        S.SearchRatTensor -> mkFunction SearchRatTensor
        S.If -> unsupportedOperation "if"
        S.Implies -> unexpected
        S.Iterate -> unexpected
      BuiltinCast {} -> unexpected
      DerivedFunction f -> convertDerived f args
      TypeClass {} -> unexpected
      TypeClassOp {} -> unexpected
      NatInDomainConstraint {} -> unexpected
      where
        unexpected = unexpectedExprError currentPass (pretty b)
        mkType v = Forced . VBuiltin (LossBuiltinType v) <$> traverseArgs go args
        mkConstructor v = Forced . VBuiltin (LossBuiltinConstructor v) <$> traverseArgs go args
        mkFunction v = Forced . VBuiltin (LossBuiltinFunction v) <$> traverseArgs go args
        convertBoolOp op = convertBooleanOp op =<< traverseArgs go args

    convertPi :: UnforcedBinder Builtin -> Closure Builtin -> m (Thunk LossBuiltin)
    convertPi binder closure = do
      binder' <- traverse go binder
      closure' <- convertClosure go binder closure
      return $ Forced $ VPi binder' closure'

    convertLam :: UnforcedBinder Builtin -> Closure Builtin -> m (Thunk LossBuiltin)
    convertLam binder closure = do
      binder' <- traverse go binder
      closure' <- convertClosure go binder closure
      return $ Forced $ VLam binder' closure'
    {-
        -- \| We want to get rid of as many `foreach`'s as possible as they don't translate well.
        -- This is a cludge and when we have a better story about optimisation we can get rid of this.
        convertForeach :: UnforcedSpine Builtin -> m (Thunk LossBuiltin)
        convertForeach args = case getExpr accessSpine args of
          Nothing -> Forced . VBuiltin (LossBuiltinFunction ForeachTensor) <$> traverseArgs go args
          Just foreachArgs -> do
            nameCtx <- fmap Just <$> getCompleteNamedCtx
            result <- runNameBoundContextT nameCtx $ rewriteForeachTensor foreachArgs
            case result of
              Unevaluable {} -> Forced . VBuiltin (LossBuiltinFunction ForeachTensor) <$> traverseArgs go args
              Evaluated val -> go val
    -}
    convertDerived :: S.DerivedFunction -> UnforcedSpine Builtin -> m (Thunk LossBuiltin)
    convertDerived f args = do
      value <- forceFreeVar (identifierOf f) args
      go $ Forced value

    convertRatTensorComparison :: ComparisonOp -> UnforcedSpine Builtin -> m (Thunk LossBuiltin)
    convertRatTensorComparison op args = case getExpr accessSpine args of
      Nothing -> do
        declProv <- getDeclProvenance
        throwError $ UnsupportedLossOperation declProv ("partially applied" <+> quotePretty op)
      Just (TensorComparisonArgs pDims rDims xs ys) -> do
        xs' <- go xs
        ys' <- go ys
        fpDims <- forceThunk pDims
        frDims <- forceThunk rDims
        case (fpDims, frDims) of
          (_, IDimNil) -> do
            pDims' <- go pDims
            let pointwiseArgs = mkExpr accessSpine $ TensorOp2Args pDims' xs' ys'
            convertBooleanOp (comparisonOpToField op) pointwiseArgs
          (IDimNil, _) -> do
            rDims' <- go rDims
            let pointwiseArgs = mkExpr accessSpine $ TensorOp2Args rDims' xs' ys'
            pointwise <- convertBooleanOp (comparisonOpToField op) pointwiseArgs
            let reductionOp = case op of
                  Ne -> ReduceDisjunction
                  _ -> ReduceConjunction
            convertBooleanOp reductionOp $
              mkExpr accessSpine $
                TensorReductionArgs rDims' pointwise
          _ -> developerError "Mixed comparisons not yet handled"

    -- \| This function converts a DeBruijn level back into a loss value.
    -- Crucially if the variable represents a slice of a quantified user variable
    -- (e.g. X[0,1]) then it is replaced in terms of the original tensor variable
    -- (e.g. X ! 0 ! 1)
    convertBoundVar ::
      Lv ->
      UnforcedSpine Builtin ->
      m (Thunk LossBuiltin)
    convertBoundVar lv = \case
      _ : _ -> unexpectedExprError currentPass "bound function variables"
      [] -> do
        (originalLv, maybeVars) <- lookupVariableInNestedCtx lv
        let var = VBoundVar originalLv []
        case maybeVars of
          Nothing -> return $ Forced var
          Just (parentVar, sliceVar) -> do
            let indices = findSliceIndices parentVar sliceVar
            return $ Forced $ mkIndexInto (Forced IRatType) var (shapeOf parentVar) indices

    convertFreeVar ::
      Identifier ->
      UnforcedSpine Builtin ->
      m (Thunk LossBuiltin)
    convertFreeVar name = \case
      [] -> return $ Forced $ VFreeVar name []
      spine -> case getExpr accessSpine spine of
        Nothing -> unexpectedExprError currentPass "non-network args"
        Just (NetworkAppArgs arg) -> do
          args' <- NetworkAppArgs <$> go arg
          return $ Forced $ VFreeVar name $ mkExpr accessSpine args'

convertQuantifiers :: QuantifierHandling m -> Quantifier -> UnforcedSpine Builtin -> m (Thunk LossBuiltin)
convertQuantifiers handling q args = case handling of
  Nothing -> developerError "Unexpected quantifier found during loss compilation"
  Just handle -> case getExpr accessQuantifyRatTensorSpine args of
    Nothing -> developerError "ill-formed quantifier args"
    Just qArgs -> handle (q, qArgs)

convertBooleanOp ::
  (MonadLogic m) =>
  TensorDifferentiableLogicField ->
  UnforcedSpine LossBuiltin ->
  m (Thunk LossBuiltin)
convertBooleanOp field args = do
  fn <- getLogicFieldValue field
  logDebugM MaxDetail $ do
    fnDoc <- prettyFriendlyInCtx fn
    return $ "subst-field" <+> pretty field <> ":" <+> fnDoc
  Forced <$> forceApplication fn args

--------------------------------------------------------------------------------
-- Dims

convertClosure ::
  (MonadLogic m) =>
  (Thunk Builtin -> m (Thunk LossBuiltin)) ->
  UnforcedBinder Builtin ->
  Closure Builtin ->
  m (Closure LossBuiltin)
convertClosure convertValue binder closure = do
  currentLv <- getBinderDepth
  let normBody = extendClosureWithBound closure binder currentLv
  finalCtx <- getShrunkenContext
  lossBody <- addNonTensorBinderToContext binder $ do
    logDebugM MaxDetail $ pretty <$> getNestedVariableCtx
    normLossBody <- convertValue normBody
    return $ unnormalise (1 + boundCtxLv finalCtx) normLossBody
  return $ Closure (boundContextToEnv finalCtx) lossBody

convertBoolTensorLiteral :: (MonadLogic m) => Tensor Bool -> m (ForcedValue LossBuiltin)
convertBoolTensorLiteral tensor = do
  trueExpr <- forceThunk =<< getLogicFieldValue TruthityElement
  falseExpr <- forceThunk =<< getLogicFieldValue FalsityElement

  let convertBool b = if b then trueExpr else falseExpr
  let foldLayer shape elems = do
        let dim = length elems
        let dims = implicitIrrelevant (mkDims shape)
        let args = implicit (INatLiteral dim) : dims : implicit INatType : fmap explicit elems
        VBuiltin (LossBuiltinFunction StackTensor) (fmap (fmap Forced) args)
  return $ foldMapTensor convertBool foldLayer tensor

--------------------------------------------------------------------------------
-- Utils

orLossValue :: (MonadLogic m) => Thunk LossBuiltin -> Thunk LossBuiltin -> m (Thunk LossBuiltin)
orLossValue e1 e2 =
  convertBooleanOp PointwiseDisjunction $
    mkExpr accessSpine (TensorOp2Args (Forced IDimNil) e1 e2)

andLossValue :: (MonadLogic m) => Thunk LossBuiltin -> Thunk LossBuiltin -> m (Thunk LossBuiltin)
andLossValue e1 e2 =
  convertBooleanOp PointwiseConjunction $
    mkExpr accessSpine (TensorOp2Args (Forced IDimNil) e1 e2)

notLossValue :: (MonadLogic m) => Thunk LossBuiltin -> Thunk LossBuiltin -> m (Thunk LossBuiltin)
notLossValue dims e =
  convertBooleanOp PointwiseNegation $
    mkExpr accessSpine (TensorOp1Args dims e)

--------------------------------------------------------------------------------
-- Utils

currentPass :: Doc a
currentPass = "logic translation"

logConversion2 ::
  (MonadLogger m, MonadReadableNameContext m) =>
  Thunk Builtin ->
  m (Thunk LossBuiltin) ->
  m (Thunk LossBuiltin)
logConversion2 e action = do
  logDebugM MaxDetail $ do
    inputDoc <- prettyFriendlyInCtx e
    return $ "enter-loss" <+> ":" <+> inputDoc
  incrCallDepth

  result <- action

  decrCallDepth
  logDebugM MaxDetail $ do
    outputDoc <- prettyFriendlyInCtx result
    return $ "exit-loss" <+> ": " <+> outputDoc

  return result
