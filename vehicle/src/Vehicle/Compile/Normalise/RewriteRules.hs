module Vehicle.Compile.Normalise.RewriteRules
  ( forceAndRewriteTensor,
    forceAndRewriteDims,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (asum)
import Data.Set qualified as Set
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Core (Negatable (..))
import Vehicle.Data.Builtin.Core.BasicOperations (ComparisonOp (..))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Code.ForcedValue as Forced
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Context.Name

type RewritableBuiltin builtin =
  ( NormalisableBuiltin builtin,
    BuiltinHasNatType builtin,
    BuiltinHasIndexLiterals builtin,
    BuiltinHasRatLiterals builtin,
    BuiltinHasForeach builtin,
    BuiltinHasTensors builtin,
    BuiltinHasListLiterals builtin,
    BuiltinHasNatLiterals builtin,
    BuiltinHasBoolLiterals builtin,
    HasTensorLiterals ForcedValue builtin,
    HasLiftableTensorOperations ForcedValue Thunk builtin,
    BuiltinHasRatType builtin,
    BuiltinHasVectors builtin,
    BuiltinHasVectorType builtin,
    BuiltinHasTensorType builtin
  )

type MonadRewrite builtin m =
  ( MonadNorm builtin m,
    MonadNameContext m,
    RewritableBuiltin builtin
  )

forceAndRewriteTensor ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  Thunk builtin ->
  m (ForcedValue builtin)
forceAndRewriteTensor value = do
  forcedValue <- forceThunk value
  case forcedValue of
    (getExpr accessConstTensor -> Just args) -> rewriteConstTensor args
    (getExpr accessStackTensor -> Just args) -> rewriteStackTensor args
    (getExpr accessTransposeTensor -> Just args) -> rewriteTransposeTensor args
    (getExpr accessAtTensor -> Just args) -> rewriteAtTensor args
    (getExpr accessForeachTensor -> Just args) -> rewriteForeachTensor args
    (getExpr accessReduceAnd -> Just args) -> rewriteReduceAndTensor args
    (getExpr accessReduceOr -> Just args) -> rewriteReduceOrTensor args
    (getExpr accessReduceMinRat -> Just args) -> rewriteReduceMinTensor args
    (getExpr accessReduceMaxRat -> Just args) -> rewriteReduceMaxTensor args
    (getExpr accessReduceAddRat -> Just args) -> rewriteReduceAddTensor args
    (getExpr accessReduceMulRat -> Just args) -> rewriteReduceMulTensor args
    (getExpr accessNotTensor -> Just args) -> rewriteNotTensor args
    (getExpr accessForeachVector -> Just args) -> rewriteForeachVector args
    _ -> return forcedValue

forceAndRewriteDims ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  Thunk builtin ->
  m (ForcedValue builtin)
forceAndRewriteDims value = do
  forcedValue <- forceThunk value
  case forcedValue of
    (getExpr accessAppendList -> Just args) -> rewriteAppendList args
    _ -> return forcedValue

-----------------------------------------------------------------------------
-- AppendList

rewriteAppendList ::
  (MonadRewrite builtin m) =>
  AppendListArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteAppendList args = logCompilerSection2 MaxDetail "rewrite-appendList" $ do
  args' <- traverseAppendListArgs (fmap Forced . forceAndRewriteDims) args
  force =<< forceEvaluation accessAppendList evalAppendList args'

-----------------------------------------------------------------------------
-- Const

rewriteConstTensor ::
  (MonadRewrite builtin m) =>
  ConstTensorArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteConstTensor args = logCompilerSection2 MaxDetail "rewrite-const" $ do
  args' <- traverseConstTensorValue (fmap Forced . forceAndRewriteTensor) args
  force =<< forceEvaluation accessConstTensor evalConstTensor args'

-----------------------------------------------------------------------------
-- Stack

rewriteStackTensor ::
  (MonadRewrite builtin m) =>
  StackTensorArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteStackTensor args = logCompilerSection2 MaxDetail "rewrite-stack" $ do
  args' <- traverseStackTensorElements (fmap Forced . forceAndRewriteTensor) args
  force =<< forceEvaluation accessStackTensor evalStackTensor args'

-----------------------------------------------------------------------------
-- ReduceAnd

rewriteReduceTensor ::
  forall m builtin b.
  (MonadRewrite builtin m) =>
  Doc b ->
  TensorOp2Accessor ForcedValue Thunk builtin ->
  TensorReductionAccessor ForcedValue Thunk builtin ->
  EvalSimple ForcedValue Thunk TensorReductionArgs builtin m ->
  Maybe ((ComparisonOp, TensorComparisonArgs (Thunk builtin)) -> m (Maybe (ForcedValue builtin))) ->
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceTensor opName accessBop accessReductionOp evalReductionOp rewriteComparison (TensorReductionArgs dims t) =
  logCompilerSection2 MaxDetail ("rewrite-" <> opName) $ go . Forced =<< forceAndRewriteTensor t
  where
    go :: Thunk builtin -> m (ForcedValue builtin)
    go tensor = logRewrite getNameContext mkReduceTensor opName tensor $ do
      forcedTensor <- force tensor
      maybeResult <- case forcedTensor of
        (getExpr accessBop -> Just (TensorOp2Args ds xs ys)) -> do
          xs' <- Forced <$> go xs
          ys' <- Forced <$> go ys
          return $
            Just $
              mkExpr accessBop $
                TensorOp2Args
                  { tensorOp2Dims = ds,
                    tensorOp2Arg1 = xs',
                    tensorOp2Arg2 = ys'
                  }
        (getExpr accessCompareRatTensor -> Just args) ->
          case rewriteComparison of
            Nothing -> return Nothing
            Just rewriteComp -> rewriteComp args
        _ -> return Nothing

      case maybeResult of
        Just result -> return result
        Nothing -> do
          evalResult <-
            forceEvaluation accessReductionOp evalReductionOp $
              TensorReductionArgs
                { tensorReductionDims = dims,
                  tensorReductionTensor = tensor
                }
          forceThunk evalResult

    mkReduceTensor :: Thunk builtin -> Thunk builtin
    mkReduceTensor tensor = Forced $ mkExpr accessReductionOp $ TensorReductionArgs dims tensor

rewriteReduceAndTensor ::
  forall m builtin.
  (MonadRewrite builtin m) =>
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceAndTensor =
  rewriteReduceTensor "reduceAnd" accessAndTensor accessReduceAnd evalReduceAndTensor (Just rewritePointwiseComparison)
  where
    rewritePointwiseComparison :: (ComparisonOp, TensorComparisonArgs (Thunk builtin)) -> m (Maybe (ForcedValue builtin))
    rewritePointwiseComparison (op, TensorComparisonArgs pDims rDims xs ys)
      | op == Ne = return Nothing
      | otherwise = do
          rDims' <- forceEvaluation accessAppendList evalAppendList $ AppendListArgs (Forced INatType) pDims rDims
          let args =
                TensorComparisonArgs
                  { tensorPointwiseDims = Forced IDimNil,
                    tensorReduceDims = rDims',
                    tensorOp2Arg1 = xs,
                    tensorOp2Arg2 = ys
                  }
          return $ Just $ mkExpr accessCompareRatTensor (op, args)

rewriteReduceOrTensor ::
  (MonadRewrite builtin m) =>
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceOrTensor = rewriteReduceTensor "reduceOr" accessOrTensor accessReduceOr evalReduceOrTensor Nothing

rewriteReduceMinTensor ::
  (MonadRewrite builtin m) =>
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceMinTensor = rewriteReduceTensor "reduceMin" accessMinRatTensor accessReduceMinRat evalReduceMinRatTensor Nothing

rewriteReduceMaxTensor ::
  (MonadRewrite builtin m) =>
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceMaxTensor = rewriteReduceTensor "reduceMax" accessMaxRatTensor accessReduceMaxRat evalReduceMaxRatTensor Nothing

rewriteReduceAddTensor ::
  (MonadRewrite builtin m) =>
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceAddTensor = rewriteReduceTensor "reduceAdd" accessAddRatTensor accessReduceAddRat evalReduceAddRatTensor Nothing

rewriteReduceMulTensor ::
  (MonadRewrite builtin m) =>
  TensorReductionArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteReduceMulTensor = rewriteReduceTensor "reduceMul" accessMulRatTensor accessReduceMulRat evalReduceMulRatTensor Nothing

-----------------------------------------------------------------------------
-- Tranpose

rewriteTransposeTensor ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  TransposeTensorArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteTransposeTensor args@(TransposeTensorArgs _t _ds tensor) = do
  logCompilerSection2 MaxDetail "rewrite-transpose" $ go tensor
  where
    go :: Thunk builtin -> m (ForcedValue builtin)
    go value = logRewrite getNameContext mkTranspose "transpose" value $ do
      rewrittenValue <- forceAndRewriteTensor value
      let maybeResult = goTranspose rewrittenValue
      case maybeResult of
        Just result -> result
        Nothing -> do
          evalResult <- forceEvaluation accessTransposeTensor evalTransposeTensor $ args {transposeTensor = Forced rewrittenValue}
          forceThunk evalResult

    goTranspose :: ForcedValue builtin -> Maybe (m (ForcedValue builtin))
    goTranspose forcedTensor = case getExpr accessTransposeTensor forcedTensor of
      Just (TransposeTensorArgs _ _ t) -> Just $ forceThunk t
      _ -> Nothing

    mkTranspose :: Thunk builtin -> Thunk builtin
    mkTranspose t = Forced $ mkExpr accessTransposeTensor $ args {transposeTensor = t}

-----------------------------------------------------------------------------
-- At

-- | An optimised evaluation procedure for `At` that attempts to minimise the
-- amount of work needed by deferring evaluation of operations until after indexing.
-- For example:
--    `(xs + ys) ! i` becomes `xs ! i + ys ! i`.
--    `(foreach j . f j) ! i` becomes `f i`
rewriteAtTensor ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  AtTensorArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteAtTensor args@(AtTensorArgs _tElem _d ds t index) =
  logCompilerSection2 MaxDetail "rewrite-at" $ go t
  where
    go :: Thunk builtin -> m (ForcedValue builtin)
    go value = logRewrite getNameContext mkAt "at" value $ do
      rewrittenValue <- forceAndRewriteTensor value
      let maybeResult =
            goOp1 rewrittenValue liftableTensorOp1s
              <|> goOp2 rewrittenValue liftableTensorOp2s
              <|> goForeach rewrittenValue
      case maybeResult of
        Just result -> result
        Nothing -> do
          evalResult <- forceEvaluation accessAtTensor evalAtTensor $ args {atTensor = Forced rewrittenValue}
          forceThunk evalResult

    goOp1 :: ForcedValue builtin -> [TensorOpEvalData ForcedValue Thunk TensorOp1Args builtin] -> Maybe (m (ForcedValue builtin))
    goOp1 forcedTensor = \case
      (accessOp1, _) : remainingOp1s -> case getExpr accessOp1 forcedTensor of
        Just (TensorOp1Args _ xs) -> Just $ do
          xsi <- Forced <$> go xs
          return $ mkExpr accessOp1 (TensorOp1Args ds xsi)
        _ -> goOp1 forcedTensor remainingOp1s
      [] -> Nothing

    goOp2 :: ForcedValue builtin -> [TensorOpEvalData ForcedValue Thunk TensorOp2Args builtin] -> Maybe (m (ForcedValue builtin))
    goOp2 forcedTensor = \case
      (accessOp2, _) : remainingOps2 -> case getExpr accessOp2 forcedTensor of
        Just (TensorOp2Args _ xs ys) -> Just $ do
          xsi <- Forced <$> go xs
          ysi <- Forced <$> go ys
          return $ mkExpr accessOp2 $ TensorOp2Args ds xsi ysi
        _ -> goOp2 forcedTensor remainingOps2
      _ -> Nothing

    goForeach :: ForcedValue builtin -> Maybe (m (ForcedValue builtin))
    goForeach forcedTensor = case getExpr accessForeachTensor forcedTensor of
      Just (ForeachTensorArgs _ _ _ fn) -> Just $ forceApp fn [explicit index]
      _ -> Nothing

    mkAt :: Thunk builtin -> Thunk builtin
    mkAt tensor = Forced $ mkExpr accessAtTensor $ args {atTensor = tensor}

{-
    goTranpose :: ForcedValue builtin -> m (Maybe (ForcedValue builtin))
    goTranpose forcedValue = do
      fds <- force ds
      case fds of
        IDimNil -> do
          maybeChain <- collect forcedValue [(d, index)]
          case maybeChain of
            Just (underlying, pairs) -> return $ Just $ rebuild underlying (reverse pairs)
            Nothing -> Nothing
        _ -> Nothing
      where
      collect ::
        ForcedValue builtin ->
        [(Thunk builtin, Thunk builtin)] ->
        m (Maybe (Thunk builtin, [(Thunk builtin, Thunk builtin)]))
      collect inner acc = case getExpr accessTransposeTensor inner of
        Just (TransposeTensorArgs _ _ underlying) -> return $ Just (underlying, acc)
        Nothing -> case getExpr accessAtTensor inner of
          Just (AtTensorArgs _ d' _ inner' i') -> do
            fInner' <- force inner'
            collect fInner' ((d', i') : acc)
          Nothing -> return Nothing

      rebuild :: Thunk builtin -> [(Thunk builtin, Thunk builtin)] -> Thunk builtin
      rebuild underlying pairs = do
        let dims = map fst pairs
        let consifyDims = foldr (\x acc -> exprToThunk (IDimCons x acc)) (exprToThunk IDimNil)
        let step (acc, j) (dj, idx) = do
              let remDims = consifyDims (drop (j + 1) dims)
              (exprToThunk (mkExpr accessAtTensor (AtTensorArgs t dj remDims acc idx)), j + 1)
        let (result, _) = foldl step (underlying, 0) pairs
        result
-}
-----------------------------------------------------------------------------
-- Not

rewriteNotTensor ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  TensorOp1Args (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteNotTensor (TensorOp1Args dimensions tensor) =
  logCompilerSection2 MaxDetail "rewrite-not" $
    go dimensions tensor
  where
    go :: Thunk builtin -> Thunk builtin -> m (ForcedValue builtin)
    go dims value = logRewrite getNameContext (mkNot dims) "not" value $ do
      rewrittenValue <- forceAndRewriteTensor value
      case rewrittenValue of
        (getExpr accessBoolTensorLiteral -> Just xs) ->
          return $ mkExpr accessBoolTensorLiteral (mapTensor not xs)
        (getExpr accessNotTensor -> Just args) ->
          force $ tensorOp1Arg args
        (getExpr accessCompareIndex -> Just (op, args)) ->
          return $ mkExpr accessCompareIndex (neg op, args)
        (getExpr accessCompareNat -> Just (op, args)) ->
          return $ mkExpr accessCompareNat (neg op, args)
        (getExpr accessCompareRatTensor -> Just (op, args)) ->
          negateCompareRatTensor (op, args)
        (getExpr accessQuantifyRatTensor -> Just (q, args)) ->
          return $ mkExpr accessQuantifyRatTensor (neg q, negateQuantifierBody args)
        (getExpr accessQuantifyRecord -> Just (q, args)) -> return $ mkExpr accessQuantifyRecord (neg q, negateRecordQuantifierBody args)
        -- Recursive cases
        (getExpr accessConstTensor -> Just args) ->
          return $ mkExpr accessConstTensor $ mapConstTensorValue (mkNot $ Forced IDimNil) args
        (getExpr accessStackTensor -> Just args) ->
          return $ mkExpr accessStackTensor $ mapStackTensorElements (mkNot $ stackRemainingDims args) args
        (getExpr accessOrTensor -> Just args) ->
          return $ mkExpr accessAndTensor $ mapTensorOp2Args (mkNot dims) args
        (getExpr accessAndTensor -> Just args) ->
          return $ mkExpr accessOrTensor $ mapTensorOp2Args (mkNot dims) args
        (getExpr accessImpliesTensor -> Just (TensorOp2Args _ xs ys)) ->
          return $ mkExpr accessAndTensor $ TensorOp2Args dims xs (mkNot dims ys)
        (getExpr accessIf -> Just args) ->
          return $ mkExpr accessIf $ mapIfArgBranches (mkNot dims) args
        (getExpr accessReduceOr -> Just args) ->
          return $ mkExpr accessReduceAnd $ mapReductionArgs (mkNot $ tensorReductionDims args) args
        (getExpr accessReduceAnd -> Just args) ->
          return $ mkExpr accessReduceOr $ mapReductionArgs (mkNot $ tensorReductionDims args) args
        (getExpr accessAtTensor -> Just args) ->
          return $ mkExpr accessAtTensor $ mapAtTensorArg (mkNot $ Forced $ IDimCons (atFirstDim args) dims) args
        (getExpr accessForeachTensor -> Just args) ->
          mkExpr accessForeachTensor <$> negateForeachArgs args
        _ -> do
          evalResult <- forceEvaluation accessNotTensor evalNot $ TensorOp1Args dims value
          forceThunk evalResult

    mkNot :: Thunk builtin -> Thunk builtin -> Thunk builtin
    mkNot dims value = Forced $ mkExpr accessNotTensor $ TensorOp1Args dims value

negateCompareRatTensor ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  (ComparisonOp, TensorComparisonArgs (Thunk builtin)) ->
  m (ForcedValue builtin)
negateCompareRatTensor (op, TensorComparisonArgs pDims rDims xs ys) = do
  fpDims <- forceThunk pDims
  frDims <- forceThunk rDims
  case (fpDims, frDims) of
    (_, IDimNil) -> return $ mkExpr accessCompareRatTensor (neg op, TensorComparisonArgs pDims rDims xs ys)
    (IDimNil, _) -> do
      let pointwiseArgs =
            TensorComparisonArgs
              { tensorPointwiseDims = Forced frDims,
                tensorReduceDims = Forced IDimNil,
                tensorOp2Arg1 = xs,
                tensorOp2Arg2 = ys
              }
      let pointwiseComparison = Forced $ mkExpr accessCompareRatTensor (neg op, pointwiseArgs)
      return $ mkExpr accessReduceOr $ TensorReductionArgs (Forced frDims) pointwiseComparison
    _ -> developerError "negation of mixed comparisons not yet implemented"

negateQuantifierBody ::
  (RewritableBuiltin builtin) =>
  QuantifyRatTensorArgs (Thunk builtin) (Closure builtin) ->
  QuantifyRatTensorArgs (Thunk builtin) (Closure builtin)
negateQuantifierBody (QuantifyRatTensorArgs pDims bDims binder (Closure env body)) = do
  let newBody = mkExpr accessNotTensor $ TensorOp1Args IDimNil body
  QuantifyRatTensorArgs
    { quantifyPointwiseDims = pDims,
      quantifyBaseDims = bDims,
      quantifyBinder = binder,
      quantifyBody = Closure env newBody
    }

negateRecordQuantifierBody ::
  (RewritableBuiltin builtin) =>
  QuantifyRecordArgs (Thunk builtin) (Closure builtin) ->
  QuantifyRecordArgs (Thunk builtin) (Closure builtin)
negateRecordQuantifierBody (QuantifyRecordArgs typ binder (Closure env body)) = do
  let newBody = mkExpr accessNotTensor $ TensorOp1Args IDimNil body
  QuantifyRecordArgs
    { quantifyRecordType = typ,
      quantifyRecordBinder = binder,
      quantifyRecordBody = Closure env newBody
    }

negateForeachArgs ::
  (MonadRewrite builtin m) =>
  ForeachTensorArgs (Thunk builtin) ->
  m (ForeachTensorArgs (Thunk builtin))
negateForeachArgs (ForeachTensorArgs t d ds fn) = do
  forcedFn <- forceThunk fn
  (binder, Closure env body) <- case forcedFn of
    VLam binder closure -> return (binder, closure)
    _ -> developerError "Malformed foreachTensor"
  lv <- getBinderDepth
  let ds' = quote mempty lv ds
  let newBody = mkExpr accessNotTensor $ TensorOp1Args ds' body
  let newFn = Forced $ VLam binder (Closure env newBody)
  return $ ForeachTensorArgs t d ds newFn

-----------------------------------------------------------------------------
-- Foreach

-- | An optimised evaluation procedure for `Foreach` that attempts to minimise the
-- amount of work needed by lifting operations to higher-tensor levels.
-- For example `foreach i . xs ! i + ys ! i` becomes `xs + ys`.
rewriteForeachTensor ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  ForeachTensorArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteForeachTensor (ForeachTensorArgs t d ds fn) =
  logCompilerSection2 MaxDetail "rewrite-foreach" $
    case getExpr accessForcedLamC fn of
      Just (binder, closure) -> do
        ctx <- getNameContext
        let lv = boundCtxLv ctx
        let body = extendClosureWithBound closure binder lv

        let createForeachArgs tElem newBody = do
              let newBody' = quote mempty (lv + 1) newBody
              let newLam = mkExpr accessForcedLamC (binder, Closure (namedBoundContextToEnv ctx) newBody')
              ForeachTensorArgs tElem d ds newLam

        addNameToContext binder $ do
          rewrittenBody <- forceAndRewriteTensor body
          liftForeach ctx createForeachArgs lv d t (Forced rewrittenBody)
      _ -> unexpectedExprError "NBE" "foreachIndex"

liftForeach ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  NamedBoundCtx ->
  (Thunk builtin -> Thunk builtin -> ForeachTensorArgs (Thunk builtin)) ->
  Lv ->
  Thunk builtin ->
  Thunk builtin ->
  Thunk builtin ->
  m (ForcedValue builtin)
liftForeach outputCtx createForeachArgs lv dim = go
  where
    go ::
      Thunk builtin ->
      Thunk builtin ->
      m (ForcedValue builtin)
    go typ body = logRewrite (return outputCtx) mkForeach "foreach" body $ do
      forcedBody <- force body
      -- Try each of the following in turn until it works.
      maybeResult <-
        runMaybeT $
          asum $
            map
              MaybeT
              [ goOp1 forcedBody liftableTensorOp1s,
                goOp2 forcedBody liftableTensorOp2s,
                goComparisons forcedBody liftableTensorComparisons,
                goConst forcedBody,
                goLiterals forcedBody tensorLiterals,
                goAt forcedBody
              ]
      case maybeResult of
        Just result -> return result
        Nothing
          | doesNotReferenceBoundVar body -> do
              let dims = Forced $ IDimCons dim $ Forced IDimNil
              let constArgs = ConstTensorArgs typ body dims
              forceThunk =<< forceEvaluation accessConstTensor evalConstTensor constArgs
          | otherwise -> do
              let args = createForeachArgs typ body
              maybeEvalResult <- evalForeachTensor args
              case maybeEvalResult of
                Unevaluable {} -> return $ mkExpr accessForeachTensor args
                Evaluated xs -> forceAndRewriteTensor xs

    -- Distribute the `forallIndex` across a liftable operation (e.g. `not`).
    -- e.g. `foreach i . op (x(i))` -> `op (foreach i . x(i))`
    goOp1 ::
      ForcedValue builtin ->
      [TensorOpEvalData ForcedValue Thunk TensorOp1Args builtin] ->
      m (Maybe (ForcedValue builtin))
    goOp1 body = \case
      (accessOp1, typ) : remainingOp1s -> case getExpr accessOp1 body of
        Just (TensorOp1Args ds e) -> do
          e' <- Forced <$> go (Forced typ) e
          return $ Just $ mkExpr accessOp1 (TensorOp1Args (Forced $ IDimCons dim ds) e')
        _ -> goOp1 body remainingOp1s
      [] -> return Nothing

    -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
    -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
    goOp2 ::
      ForcedValue builtin ->
      [TensorOpEvalData ForcedValue Thunk TensorOp2Args builtin] ->
      m (Maybe (ForcedValue builtin))
    goOp2 body = \case
      (accessOp2, typ) : remainingOps -> case getExpr accessOp2 body of
        Just (TensorOp2Args ds e1 e2) -> do
          e1' <- Forced <$> go (Forced typ) e1
          e2' <- Forced <$> go (Forced typ) e2
          let newSpine = TensorOp2Args (Forced $ IDimCons dim ds) e1' e2'
          return $ Just $ mkExpr accessOp2 newSpine
        _ -> goOp2 body remainingOps
      [] -> return Nothing

    -- Eliminate `forall i . e ! i` into `e` if `e` does not reference `i`
    goAt :: ForcedValue builtin -> m (Maybe (ForcedValue builtin))
    goAt value = case getExpr accessAtTensor value of
      Just (AtTensorArgs _ _ _ xs i) -> do
        i' <- force i
        case getExpr (accessBoundVarC @ForcedValue @Thunk @Closure) i' of
          Just (lv1, [])
            | lv1 == lv && doesNotReferenceBoundVar xs ->
                Just <$> force xs
          _ -> return Nothing
      _ -> return Nothing

    goLiterals :: ForcedValue builtin -> [TensorLiteralAccessor ForcedValue builtin] -> m (Maybe (ForcedValue builtin))
    goLiterals value literals = case literals of
      Wrapper Access {..} : remainingLiterals -> do
        forcedDim <- force dim
        case (getExpr value, forcedDim) of
          (Just xs, INatLiteral dim') -> return $ Just $ mkExpr $ extendTensor dim' xs
          _ -> goLiterals value remainingLiterals
      _ -> return Nothing

    -- Distribute the `forallIndex` across comparisons operation (e.g. `<=`).
    -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
    goComparisons ::
      ForcedValue builtin ->
      [TensorOpEvalData ForcedValue Thunk TensorComparisonArgs builtin] ->
      m (Maybe (ForcedValue builtin))
    goComparisons body = \case
      (accessOp, typ) : remainingOps -> case getExpr accessOp body of
        Just (TensorComparisonArgs pDims rDims e1 e2) -> do
          e1' <- go (exprToThunk typ) e1
          e2' <- go (exprToThunk typ) e2
          let newSpine = TensorComparisonArgs (exprToThunk $ IDimCons dim pDims) rDims (exprToThunk e1') (exprToThunk e2')
          return $ Just $ mkExpr accessOp newSpine
        _ -> goComparisons body remainingOps
      [] -> return Nothing

    goConst :: ForcedValue builtin -> m (Maybe (ForcedValue builtin))
    goConst value = case getExpr accessConstTensor value of
      Just (ConstTensorArgs t x ds) | doesNotReferenceBoundVar x -> do
        return $
          Just $
            mkExpr accessConstTensor $
              ConstTensorArgs t x (Forced $ IDimCons dim ds)
      _ -> return Nothing

    mkForeach :: Thunk builtin -> Thunk builtin
    mkForeach body = Forced $ mkExpr accessForeachTensor $ createForeachArgs (Forced IRatType) body

    doesNotReferenceBoundVar :: Thunk builtin -> Bool
    doesNotReferenceBoundVar value = lv `Set.notMember` Forced.boundVariablesIn (lv + 1) value

  -- rewrite ForeachVector into a ForeachTensor IF is over type tensor, and then call rewriteForeachTensor
rewriteForeachVector ::
  forall builtin m.
  (MonadRewrite builtin m) =>
  ForeachVectorArgs (Thunk builtin) ->
  m (ForcedValue builtin)
rewriteForeachVector (ForeachVectorArgs vType vDim fn) = do
  vType' <- forceThunk vType
  case vType' of
    IVectorType vElem _vDim -> do
      vElem' <- forceThunk vElem
      case vElem' of 
        (ITensorType tElem tDims) -> do
          let args = ForeachTensorArgs tElem vDim tDims fn
          rewriteForeachTensor args
        _ -> return $ mkExpr accessForeachVector (ForeachVectorArgs vType vDim fn)
    _ -> return $ mkExpr accessForeachVector (ForeachVectorArgs vType vDim fn)

logRewrite ::
  (MonadNormBuiltin m, HasRatType ForcedValue Thunk builtin, BuiltinHasForeach builtin, PrintableBuiltin builtin) =>
  m NamedBoundCtx ->
  (Thunk builtin -> Thunk builtin) ->
  Doc b ->
  Thunk builtin ->
  m (ForcedValue builtin) ->
  m (ForcedValue builtin)
logRewrite getCtx createInputExpr op input outputFn = do
  logDebugM MaxDetail $ do
    ctx <- getCtx
    let expr = createInputExpr input
    let inputDoc = prettyFriendly (WithContext expr ctx)
    return $ "rewrite-" <> op <> "-enter:" <+> inputDoc
  incrCallDepth

  output <- outputFn

  decrCallDepth
  logDebugM MaxDetail $ do
    ctx <- getCtx
    let outputDoc = prettyFriendly (WithContext output ctx)
    return $ "rewrite-" <> op <> "-exit:" <+> outputDoc

  return output
