module Vehicle.Compile.Normalise.Builtin where

import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import Data.Maybe (isJust)
import Data.Ratio
import Data.Vector qualified as Vector
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core.BasicOperations (ComparisonOp, comparisonOp)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Tensor
import Vehicle.Data.Tensor qualified as Tensor

-- Okay so the important thing to remember about this module is that we have
-- a variety of different typing schemes for builtins (standard, polarity,
-- linearity etc.). Normalisation needs to work for all of these, and
-- therefore we can't guarantee what the implicit and instance arguments are
-- going to be for a given builtin. However, explicit arguments are always
-- the same in every type system.

-- Therefore this can be viewed as a type of runtime irrelevance, where only
-- the explicit arguments are runtime relevant. This notion isn't made
-- explicit in the code below. Maybe there's a nice way of doing so?

-----------------------------------------------------------------------------
-- Main method

type MonadNormBuiltin m =
  ( MonadLogger m
  )

forceEvaluation ::
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m) =>
  Accessor (expr builtin) (args (thunk builtin)) ->
  EvalSimple expr thunk args builtin m ->
  args (thunk builtin) ->
  m (thunk builtin)
forceEvaluation accessOp evalFn args = do
  evalResult <- evalFn args
  return $ case evalResult of
    Evaluated result -> result
    Unevaluable {} -> exprToThunk $ mkExpr accessOp args

forceEvalSimpleBuiltin ::
  (IsArgs args, MonadLogger m, Pretty builtin, PrintableBuiltin builtin) =>
  Provenance ->
  builtin ->
  EvalSimple Expr Expr args builtin m ->
  [GenericArg (Expr builtin)] ->
  m (Expr builtin)
forceEvalSimpleBuiltin p b eval spine =
  case getExpr accessSpine spine of
    Just args -> do
      maybeResult <- eval args
      case maybeResult of
        Unevaluable {} -> return $ normAppList (Builtin p b) spine
        Evaluated result -> return result
    Nothing -> return $ normAppList (Builtin p b) spine

-----------------------------------------------------------------------------
-- Utils

getDim ::
  forall expr thunk builtin m.
  (NormalisableExpr expr thunk builtin m, HasNatExpr expr thunk builtin, Monad m) =>
  thunk builtin ->
  m (Maybe Int)
getDim value = do
  forcedValue <- force @expr value
  return $ case forcedValue of
    INatLiteral n -> Just n
    _ -> Nothing

getDimsExprs ::
  (NormalisableExpr expr thunk builtin m, HasNatType expr thunk builtin, HasNatExpr expr thunk builtin, HasListExpr expr thunk builtin, Monad m) =>
  thunk builtin ->
  m (Either (expr builtin) [thunk builtin])
getDimsExprs value = do
  forcedValue <- force value
  case forcedValue of
    IDimNil -> return $ Right []
    IDimCons d ds -> do
      r <- getDimsExprs ds
      return ((d :) <$> r)
    e -> return $ Left e

getDims ::
  (NormalisableExpr expr thunk builtin m, HasNatType expr thunk builtin, HasNatExpr expr thunk builtin, HasListExpr expr thunk builtin, Monad m) =>
  thunk builtin ->
  m (Maybe TensorShape)
getDims value = do
  dims <- getDimsExprs value
  case dims of
    Left {} -> return Nothing
    Right xs -> do
      rs <- traverse getDim xs
      return $ sequence rs

--------------------------------------------------------------------------------
-- Evaluation

type EvalSimple expr thunk args builtin m =
  ( HasBuiltinConstructor expr thunk,
    NormalisableExpr expr thunk builtin m
  ) =>
  args (thunk builtin) ->
  m (BuiltinEvaluationResult expr thunk builtin)

evalTensorOp1 ::
  forall expr thunk builtin a m.
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasTensorExpr expr thunk builtin, Eq a) =>
  Accessor (expr builtin) (TensorOp1Args (thunk builtin)) ->
  Accessor (expr builtin) (Tensor a) ->
  (a -> a) ->
  EvalSimple expr thunk TensorOp1Args builtin m
evalTensorOp1 accessOp accessLit op = go
  where
    go :: EvalSimple expr thunk TensorOp1Args builtin m
    go (TensorOp1Args vds vxs) = do
      ds' <- force vds
      xs' <- force vxs
      case (ds', xs') of
        (_ds, getExpr accessLit -> Just t) ->
          return $ Evaluated $ exprToThunk $ mkExpr accessLit $ mapTensor op t
        (IDimCons _ ds, getExpr accessConstTensor -> Just xs) -> do
          xs'' <- traverseConstTensorValue (evalFull ds) xs
          return $ Evaluated $ exprToThunk $ mkExpr accessConstTensor xs''
        (IDimCons _ ds, getExpr accessStackTensor -> Just xs) -> do
          xs'' <- traverseStackTensorElements (evalFull ds) xs
          return $ Evaluated $ exprToThunk $ mkExpr accessStackTensor xs''
        _ -> return $ Unevaluable [ds', xs']

    evalFull :: thunk builtin -> thunk builtin -> m (thunk builtin)
    evalFull ds x = forceEvaluation accessOp go $ TensorOp1Args ds x

evalTensorOp2 ::
  forall expr thunk builtin a m.
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasTensorExpr expr thunk builtin, Eq a) =>
  Accessor (expr builtin) (TensorOp2Args (thunk builtin)) ->
  Accessor (expr builtin) (Tensor a) ->
  (a -> a -> a) ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  EvalSimple expr thunk TensorOp2Args builtin m
evalTensorOp2 accessOp2 accessLit =
  evalHeteroTensorOp2 (mkExpr accessOp2) accessLit accessLit

evalHeteroTensorOp2 ::
  forall expr thunk builtin a b m.
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasTensorExpr expr thunk builtin, Eq a, Eq b) =>
  (TensorOp2Args (thunk builtin) -> expr builtin) ->
  Accessor (expr builtin) (Tensor a) ->
  Accessor (expr builtin) (Tensor b) ->
  (a -> a -> b) ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  EvalSimple expr thunk TensorOp2Args builtin m
evalHeteroTensorOp2 accessOp2 inputLit outputLit op leftUnit rightUnit leftZero rightZero = go
  where
    go :: EvalSimple expr thunk TensorOp2Args builtin m
    go (TensorOp2Args vds vxs vys) = do
      fds <- force @expr vds
      fxs <- force @expr vxs
      fys <- force @expr vys
      case (fds, fxs, fys) of
        (_ds, getExpr inputLit -> Just xs, getExpr inputLit -> Just ys) -> do
          return $ Evaluated $ exprToThunk $ mkExpr outputLit $ zipWithTensor op xs ys
        (IDimCons _ ds, getExpr accessConstTensor -> Just xs, getExpr accessConstTensor -> Just ys) -> do
          newConstValue <- evalFull ds (constValue xs) (constValue ys)
          return $ Evaluated $ exprToThunk $ mkExpr accessConstTensor $ xs {constValue = newConstValue}
        -- Unlike const tensors, we need to eval stack tensors as after being combined with constants, short-circuiting of
        -- operations may allow for further reduction.
        (IDimCons _ ds, getExpr inputLit -> Just xs, getExpr accessStackTensor -> Just ys) -> do
          newElements <- zipWithM (evalFull ds) (unstackExpr xs) (stackElements ys)
          return $ Evaluated $ exprToThunk $ mkExpr accessStackTensor $ ys {stackElements = newElements}
        (IDimCons _ ds, getExpr accessStackTensor -> Just xs, getExpr inputLit -> Just ys) -> do
          newElements <- zipWithM (evalFull ds) (stackElements xs) (unstackExpr ys)
          return $ Evaluated $ exprToThunk $ mkExpr accessStackTensor $ xs {stackElements = newElements}
        (IDimCons _ ds, getExpr accessStackTensor -> Just xs, getExpr accessStackTensor -> Just ys) -> do
          newElements <- zipWithM (evalFull ds) (stackElements xs) (stackElements ys)
          return $ Evaluated $ exprToThunk $ mkExpr accessStackTensor $ xs {stackElements = newElements}
        _ -> do
          maybeLeftConst <- getConstValue fxs
          maybeRightConst <- getConstValue fys
          if (isJust leftUnit && leftUnit == maybeLeftConst) || (isJust rightZero && rightZero == maybeRightConst)
            then return $ Evaluated $ exprToThunk fys
            else
              if (isJust rightUnit && rightUnit == maybeRightConst) || (isJust leftZero && leftZero == maybeLeftConst)
                then return $ Evaluated $ exprToThunk fxs
                else return $ Unevaluable [fds, fxs, fys]

    evalFull :: thunk builtin -> thunk builtin -> thunk builtin -> m (thunk builtin)
    evalFull d x y = do
      result <- go $ TensorOp2Args d x y
      case result of
        Unevaluable {} -> return $ exprToThunk $ accessOp2 $ TensorOp2Args d x y
        Evaluated res -> return res

    unstackExpr :: Tensor a -> [thunk builtin]
    unstackExpr xs = exprToThunk . mkExpr inputLit <$> unstack xs

    getConstValue :: expr builtin -> m (Maybe a)
    getConstValue = \case
      (getExpr inputLit -> Just (ConstantTensor _ v)) -> return $ Just v
      (getExpr accessConstTensor -> Just constTensor) -> do
        forcedValue <- force $ constValue constTensor
        getConstValue forcedValue
      _ -> return Nothing

evalReduceTensor ::
  forall expr thunk builtin a m.
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasTensorExpr expr thunk builtin) =>
  Accessor (expr builtin) (TensorReductionArgs (thunk builtin)) ->
  Accessor (expr builtin) (Tensor a) ->
  Accessor (expr builtin) (TensorOp2Args (thunk builtin)) ->
  (a -> a -> a) ->
  a ->
  EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceTensor accessReductionOp accessLit accessOp2 op2 unit = go
  where
    go :: EvalSimple expr thunk TensorReductionArgs builtin m
    go (TensorReductionArgs vds vxs) = do
      fds <- force @expr vds
      fxs <- force @expr vxs
      case (fds, fxs) of
        (IDimNil, _) ->
          return $ Evaluated $ exprToThunk fxs
        (_, getExpr accessLit -> Just xs) ->
          return $ Evaluated $ exprToThunk $ mkExpr accessLit $ foldTensor op2 unit xs
        (IDimCons _ ds, getExpr accessStackTensor -> Just xs) -> do
          case stackElements xs of
            [] -> return $ Evaluated $ exprToThunk $ mkExpr accessLit (ZeroDimTensor unit)
            v : vs -> do
              v' <- evalFull ds v
              Evaluated <$> foldM (foldFn ds) v' vs
        _ ->
          return $ Unevaluable [fds, fxs]

    evalFull :: thunk builtin -> thunk builtin -> m (thunk builtin)
    evalFull ds xs = forceEvaluation accessReductionOp go (TensorReductionArgs ds xs)

    evalBop :: thunk builtin -> thunk builtin -> thunk builtin -> thunk builtin
    evalBop ds xs ys = exprToThunk $ mkExpr accessOp2 (TensorOp2Args ds xs ys)

    foldFn :: thunk builtin -> thunk builtin -> thunk builtin -> m (thunk builtin)
    foldFn ds r y = evalBop ds r <$> evalFull ds y

-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------
-- Not

evalNot ::
  (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) =>
  EvalSimple expr thunk TensorOp1Args builtin m
evalNot = evalTensorOp1 accessNotTensor accessBoolTensorLiteral not

-----------------------------------------------------------------------------
-- And

evalAnd ::
  (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) =>
  EvalSimple expr thunk TensorOp2Args builtin m
evalAnd = evalTensorOp2 accessAndTensor accessBoolTensorLiteral (&&) (Just True) (Just True) (Just False) (Just False)

-----------------------------------------------------------------------------
-- Or

evalOr ::
  (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) =>
  EvalSimple expr thunk TensorOp2Args builtin m
evalOr = evalTensorOp2 accessOrTensor accessBoolTensorLiteral (||) (Just False) (Just False) (Just True) (Just True)

-----------------------------------------------------------------------------
-- Implies

elimImplies ::
  (HasBoolExpr expr thunk builtin) =>
  TensorOp2Args (thunk builtin) ->
  thunk builtin
elimImplies (TensorOp2Args ds xs ys) = do
  let notXs = exprToThunk $ mkExpr accessNotTensor (TensorOp1Args ds xs)
  let notXsOrYs = mkExpr accessOrTensor (TensorOp2Args ds notXs ys)
  exprToThunk notXsOrYs

evalImplies ::
  (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) =>
  EvalSimple expr thunk TensorOp2Args builtin m
evalImplies args = return $ Evaluated $ elimImplies args

-----------------------------------------------------------------------------
-- ReduceAnd

evalReduceAndTensor :: (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) => EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceAndTensor = evalReduceTensor accessReduceAnd accessBoolTensorLiteral accessAndTensor (&&) True

evalReduceOrTensor :: (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) => EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceOrTensor = evalReduceTensor accessReduceOr accessBoolTensorLiteral accessOrTensor (||) False

-----------------------------------------------------------------------------
-- If

evalIf :: forall m expr thunk builtin. (MonadNormBuiltin m, HasBoolExpr expr thunk builtin) => EvalSimple expr thunk IfArgs builtin m
evalIf (IfArgs _t c e1 e2) = do
  fc <- force @expr c
  case fc of
    IBoolLiteral True -> return $ Evaluated e1
    IBoolLiteral False -> return $ Evaluated e2
    _ -> return $ Unevaluable [fc]

-----------------------------------------------------------------------------
-- Index

evalCompareIndex ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, HasBoolExpr expr thunk builtin, BuiltinHasIndexLiterals builtin) =>
  ComparisonOp ->
  EvalSimple expr thunk IndexComparisonArgs builtin m
evalCompareIndex op (IndexComparisonArgs _ _ v1 v2) = do
  v1' <- force @expr v1
  v2' <- force @expr v2
  case (v1', v2') of
    (IIndexLiteral x _, IIndexLiteral y _) ->
      return $ Evaluated $ exprToThunk $ IBoolLiteral (comparisonOp op x y)
    _ -> return $ Unevaluable [v1', v2']

-----------------------------------------------------------------------------
-- Nat

evalNatOp2 ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasNatExpr expr thunk builtin) =>
  (Int -> Int -> Int) ->
  EvalSimple expr thunk Op2Args builtin m
evalNatOp2 f (Op2Args vx vy) = do
  fx <- force @expr @thunk vx
  fy <- force @expr @thunk vy
  case (fx, fy) of
    (INatLiteral x, INatLiteral y) -> return $ Evaluated $ exprToThunk $ INatLiteral (f x y)
    _ -> return $ Unevaluable [fx, fy]

evalAddNat ::
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasNatExpr expr thunk builtin) =>
  EvalSimple expr thunk Op2Args builtin m
evalAddNat = evalNatOp2 (+)

evalMulNat ::
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasNatExpr expr thunk builtin) =>
  EvalSimple expr thunk Op2Args builtin m
evalMulNat = evalNatOp2 (*)

evalCompareNat ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, HasBuiltinConstructor expr thunk, NormalisableExpr expr thunk builtin m, HasBoolExpr expr thunk builtin, HasNatExpr expr thunk builtin) =>
  ComparisonOp ->
  EvalSimple expr thunk Op2Args builtin m
evalCompareNat op (Op2Args vx vy) = do
  fx <- force @expr vx
  fy <- force @expr vy
  case (fx, fy) of
    (INatLiteral x, INatLiteral y) -> return $ Evaluated $ exprToThunk $ IBoolLiteral (comparisonOp op x y)
    _ -> return $ Unevaluable [fx, fy]

-----------------------------------------------------------------------------
-- List

evalMapList ::
  forall expr thunk builtin m.
  (MonadLogger m, HasBuiltinConstructor expr thunk, NormalisableExpr expr thunk builtin m, BuiltinHasListLiterals builtin) =>
  EvalSimple expr thunk MapListArgs builtin m
evalMapList (MapListArgs t1 t2 f xs) = do
  fxs <- force xs
  case fxs of
    INil _ -> return $ Evaluated $ exprToThunk $ INil t2
    ICons _ v vs -> do
      v' <- exprToThunk <$> forceApp f [explicit v]
      let vs' = exprToThunk $ mkExpr accessMapList (MapListArgs t1 t2 f vs)
      return $ Evaluated $ exprToThunk $ ICons t2 v' vs'
    _ -> return $ Unevaluable [fxs]

evalFoldList ::
  forall m expr thunk builtin.
  (MonadLogger m, HasBuiltinConstructor expr thunk, NormalisableExpr expr thunk builtin m, BuiltinHasListLiterals builtin) =>
  EvalSimple expr thunk FoldListArgs builtin m
evalFoldList (FoldListArgs a b f e xs) = do
  fxs <- force xs
  case fxs of
    INil _ -> return $ Evaluated e
    ICons _ v vs -> do
      let r = exprToThunk $ mkExpr accessFoldList (FoldListArgs a b f e vs)
      Evaluated . exprToThunk <$> forceApp f [explicit v, explicit r]
    _ -> return $ Unevaluable [fxs]

evalReverseList ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, PrintableBuiltin builtin, BuiltinHasListLiterals builtin, NormalisableExpr expr thunk builtin m) =>
  EvalSimple expr thunk ReverseListArgs builtin m
evalReverseList (ReverseListArgs t xs) = go xs (exprToThunk (INil t))
  where
    go :: thunk builtin -> thunk builtin -> m (BuiltinEvaluationResult expr thunk builtin)
    go curr acc = do
      fcurr <- force curr
      case fcurr of
        INil _ -> return $ Evaluated acc
        ICons _ v vs -> go vs (exprToThunk (ICons t v acc))
        _ -> return $ Unevaluable [fcurr]

evalTransposeTensor ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, NormalisableExpr expr thunk builtin m, HasTensorLiterals expr builtin, BuiltinHasNatLiterals builtin, BuiltinHasNatType builtin, HasTensorExpr expr thunk builtin) =>
  EvalSimple expr thunk TransposeTensorArgs builtin m
evalTransposeTensor (TransposeTensorArgs _ inputDims tensor) = do
  ftensor <- force tensor
  case goLiteral ftensor tensorLiterals <|> goConst ftensor of
    Just result -> return $ Evaluated $ exprToThunk result
    Nothing -> do
      maybeResult <- goStack ftensor
      case maybeResult of
        Just result -> return $ Evaluated result
        Nothing -> return $ Unevaluable [ftensor]
  where
    goLiteral :: expr builtin -> [TensorLiteralAccessor expr builtin] -> Maybe (expr builtin)
    goLiteral _ [] = Nothing
    goLiteral ft (Wrapper Access {getExpr = getLit, mkExpr = mkLit} : rest) =
      (mkLit . Tensor.transposeTensor <$> getLit ft) <|> goLiteral ft rest

    goConst :: expr builtin -> Maybe (expr builtin)
    goConst ft = do
      ConstTensorArgs t v _ <- getExpr accessConstTensor ft
      let rds = exprToThunk $ mkExpr accessReverseList $ ReverseListArgs (exprToThunk INatType) inputDims
      pure $ mkExpr accessConstTensor (ConstTensorArgs t v rds)

    goStack :: expr builtin -> m (Maybe (thunk builtin))
    goStack forcedTensor = do
      maybeShape <- getDims inputDims
      case maybeShape of
        Just shape -> do
          maybeLeaves <- gatherStack shape forcedTensor
          case maybeLeaves of
            Just leaves -> return $ Just $ foldMapTensorLike id mkStack (reverse shape) (permuteFlat shape leaves)
            Nothing -> return Nothing
        Nothing -> return Nothing
      where
        gatherStack :: TensorShape -> expr builtin -> m (Maybe [thunk builtin])
        gatherStack [] v = return $ Just [exprToThunk v]
        gatherStack (d : ds) v = case getExpr accessStackTensor v of
          Nothing -> return Nothing
          Just (StackTensorArgs _ _ _ rows) ->
            if length rows /= d
              then return Nothing
              else do
                forcedRows <- traverse (force @expr) rows
                subs <- traverse (gatherStack ds) forcedRows
                return $ fmap concat (sequence subs)

        permuteFlat :: TensorShape -> [thunk builtin] -> [thunk builtin]
        permuteFlat shape leaves = do
          let values = Vector.fromList leaves
          [values Vector.! flattenIndices shape (reverse revIs) | revIs <- allMultiIndices (reverse shape)]

        mkStack :: TensorShape -> [thunk builtin] -> thunk builtin
        mkStack ds elems =
          exprToThunk $
            mkExpr
              accessStackTensor
              ( StackTensorArgs
                  (exprToThunk INatType)
                  (exprToThunk (INatLiteral (length elems)))
                  (exprToThunk (mkDims ds))
                  elems
              )

evalAppendList ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, PrintableBuiltin builtin, BuiltinHasListLiterals builtin, NormalisableExpr expr thunk builtin m) =>
  EvalSimple expr thunk AppendListArgs builtin m
evalAppendList (AppendListArgs t xs ys) = do
  fxs <- force xs
  fys <- force ys
  case (fxs, fys) of
    (_, INil _) -> return $ Evaluated $ exprToThunk fxs
    (INil _, _) -> return $ Evaluated $ exprToThunk fys
    (ICons _ x xs', _) -> do
      recAppend <- forceEvaluation accessAppendList evalAppendList $ AppendListArgs t xs' ys
      return $ Evaluated $ exprToThunk $ ICons t x recAppend
    _ -> return $ Unevaluable []

-----------------------------------------------------------------------------
-- Rational tensors

evalNegRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp1Args builtin m
evalNegRatTensor = evalTensorOp1 accessNegRatTensor accessRatTensorLiteral (\x -> -x)

evalLogRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp1Args builtin m
evalLogRatTensor _x = return $ Unevaluable []

evalExpRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp1Args builtin m
evalExpRatTensor _x = return $ Unevaluable []

evalAddRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalAddRatTensor = evalTensorOp2 accessAddRatTensor accessRatTensorLiteral (+) (Just 0) (Just 0) Nothing Nothing

evalMulRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalMulRatTensor = evalTensorOp2 accessMulRatTensor accessRatTensorLiteral (*) (Just 1) (Just 1) (Just 0) (Just 0)

evalSubRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalSubRatTensor = evalTensorOp2 accessSubRatTensor accessRatTensorLiteral (-) Nothing (Just 0) Nothing Nothing

evalDivRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalDivRatTensor = evalTensorOp2 accessDivRatTensor accessRatTensorLiteral (/) Nothing (Just 1) Nothing Nothing

evalMinRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalMinRatTensor = evalTensorOp2 accessMinRatTensor accessRatTensorLiteral min Nothing Nothing Nothing Nothing

evalMaxRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalMaxRatTensor = evalTensorOp2 accessMaxRatTensor accessRatTensorLiteral max Nothing Nothing Nothing Nothing

evalPowRatTensor :: forall expr thunk builtin m. (MonadNormBuiltin m, HasRatExpr expr thunk builtin) => EvalSimple expr thunk TensorOp2Args builtin m
evalPowRatTensor (TensorOp2Args _ xs e) = do
  xs' <- force @expr xs
  e' <- force @expr e
  case (xs', e') of
    (IRatTensor t, IRatLiteral (Finite n))
      -- We can only evaluate this if the exponent is an integer
      | denominator n == 1 -> return $ Evaluated $ exprToThunk $ IRatTensor (mapTensor (^^ numerator n) t)
    _ -> return $ Unevaluable [xs', e']

evalReduceAddRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin, PrintableBuiltin builtin) => EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceAddRatTensor = evalReduceTensor accessReduceAddRat accessRatTensorLiteral accessAddRatTensor (+) 0

evalReduceMulRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin, PrintableBuiltin builtin) => EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceMulRatTensor = evalReduceTensor accessReduceMulRat accessRatTensorLiteral accessMulRatTensor (*) 1

evalReduceMinRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin, PrintableBuiltin builtin) => EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceMinRatTensor = evalReduceTensor accessReduceMinRat accessRatTensorLiteral accessMinRatTensor min PosInfinity

evalReduceMaxRatTensor :: (MonadNormBuiltin m, HasRatExpr expr thunk builtin, PrintableBuiltin builtin) => EvalSimple expr thunk TensorReductionArgs builtin m
evalReduceMaxRatTensor = evalReduceTensor accessReduceMaxRat accessRatTensorLiteral accessMaxRatTensor max NegInfinity

-----------------------------------------------------------------------------
-- Generic vector operations

evalAtVector ::
  forall expr thunk builtin m.
  (MonadNormBuiltin m, BuiltinHasIndexLiterals builtin, HasVectorExpr expr thunk builtin) =>
  EvalSimple expr thunk AtVectorArgs builtin m
evalAtVector (AtVectorArgs _t _d vector index) = do
  vector' <- force @expr vector
  index' <- force @expr index
  case (vector', index') of
    (IVecLiteral _t _d xs, IIndexLiteral i _) -> do
      return $ Evaluated (xs !! i)
    _ -> return $ Unevaluable [vector', index']

-----------------------------------------------------------------------------
-- Generic tensor operations
-----------------------------------------------------------------------------
-- At

evalAtTensor ::
  forall expr thunk builtin m.
  (MonadNormBuiltin m, HasTensorLiterals expr builtin, BuiltinHasListLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasNatType builtin, HasTensorExpr expr thunk builtin) =>
  EvalSimple expr thunk AtTensorArgs builtin m
evalAtTensor (AtTensorArgs _t _d ds tensor index) = do
  fTensor <- force @expr tensor
  case fTensor of
    (getExpr accessConstTensor -> Just constArgs) -> return $ Evaluated $ exprToThunk $ mkExpr accessConstTensor $ constArgs {constDims = ds}
    (getExpr accessStackTensor -> Just stackArgs) -> do
      fIndex <- force @expr index
      case fIndex of
        IIndexLiteral i _ -> return $ Evaluated $ stackElements stackArgs !! i
        _ -> return $ Unevaluable [fIndex, fTensor]
    _ -> goLiterals fTensor tensorLiterals
  where
    goLiterals :: expr builtin -> [TensorLiteralAccessor expr builtin] -> m (BuiltinEvaluationResult expr thunk builtin)
    goLiterals fTensor literals = case literals of
      Wrapper Access {..} : remainingLiterals -> case getExpr fTensor of
        Nothing -> goLiterals fTensor remainingLiterals
        Just (ConstantTensor (_dim : dims) c) -> return $ Evaluated $ exprToThunk $ mkExpr $ ConstantTensor dims c
        Just xs -> do
          fIndex <- force @expr index
          case fIndex of
            IIndexLiteral ci _ -> return $ Evaluated $ exprToThunk $ mkExpr (xs `at` ci)
            _ -> return $ Unevaluable [fIndex, fTensor]
      [] -> return $ Unevaluable [fTensor]

-----------------------------------------------------------------------------
-- Foreach

type HasOptimisedAtBuiltins builtin =
  ( NormalisableBuiltin builtin,
    BuiltinHasListLiterals builtin,
    BuiltinHasNatType builtin,
    BuiltinHasNatLiterals builtin,
    BuiltinHasIndexLiterals builtin,
    BuiltinHasTensors builtin,
    BuiltinHasForeach builtin
  )

evalForeachTensor ::
  forall m expr thunk builtin.
  (MonadLogger m, NormalisableExpr expr thunk builtin m, HasTensorLiterals expr builtin, HasTensorExpr expr thunk builtin, BuiltinHasNatLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin) =>
  EvalSimple expr thunk ForeachTensorArgs builtin m
evalForeachTensor (ForeachTensorArgs t d ds f) = do
  d' <- force @expr d
  case d' of
    INatLiteral n -> do
      xs <- traverse (\i -> exprToThunk <$> forceApp f [explicit (exprToThunk $ IIndexLiteral i d)]) [0 .. (n - 1 :: Int)]
      let stackArgs = StackTensorArgs t d ds xs
      return $ Evaluated $ exprToThunk $ mkExpr accessStackTensor stackArgs
    _ -> return $ Unevaluable [d']

-----------------------------------------------------------------------------
-- Stack

evalStackTensor ::
  (MonadNormBuiltin m, HasTensorLiterals expr builtin, BuiltinHasNatLiterals builtin, HasTensorExpr expr thunk builtin) =>
  EvalSimple expr thunk StackTensorArgs builtin m
evalStackTensor = evalStackTensorWithPrimitives tensorLiterals

evalStackTensorWithPrimitives ::
  forall m expr thunk builtin.
  (MonadNormBuiltin m, BuiltinHasNatLiterals builtin, HasTensorExpr expr thunk builtin) =>
  [TensorLiteralAccessor expr builtin] ->
  EvalSimple expr thunk StackTensorArgs builtin m
evalStackTensorWithPrimitives tensorLits (StackTensorArgs _t d ds xs) = do
  fd <- force @expr d
  fds <- getDims ds
  -- If we know that all the tensors being stacked are concrete tensors, then
  -- we must know the dimensions as well.
  maybeResult <- case (fd, fds) of
    (INatLiteral n, Just ns) | length xs == n -> do
      fxs <- traverse force xs
      sequence $ go ns fxs tensorLits
    _ -> return Nothing
  case maybeResult of
    Nothing -> return $ Unevaluable [fd]
    Just result -> return result
  where
    go :: TensorShape -> [expr builtin] -> [TensorLiteralAccessor expr builtin] -> Maybe (m (BuiltinEvaluationResult expr thunk builtin))
    go elemDims elements = \case
      Wrapper Access {..} : prims ->
        case traverse getExpr elements of
          Just xss -> Just $ return $ Evaluated $ exprToThunk $ mkExpr $ stack elemDims xss
          Nothing -> go elemDims elements prims
      [] -> Nothing

-----------------------------------------------------------------------------
-- Const

evalConstTensor ::
  forall expr thunk builtin m.
  ( MonadNormBuiltin m,
    NormalisableExpr expr thunk builtin m,
    HasTensorLiterals expr builtin,
    BuiltinHasNatLiterals builtin,
    HasTensorExpr expr thunk builtin
  ) =>
  EvalSimple expr thunk ConstTensorArgs builtin m
evalConstTensor (ConstTensorArgs _t xs ds) = do
  fxs <- force xs
  -- Pattern matching on ds here is technically a bug as blocking will not
  -- function correctly. However, to fix it we would need to go via `StackTensor`
  -- and in particular make `StackTensor` take the size argument as an expression.
  -- Our type-system can't handle that easily yet.
  maybeDims <- getDims ds
  case maybeDims of
    Nothing -> do
      forcedDims <- force ds
      return $ Unevaluable [fxs, forcedDims]
    Just [] -> return $ Evaluated xs
    Just dims -> case go dims fxs tensorLiterals of
      Just result -> return $ Evaluated $ exprToThunk result
      _ -> do
        forcedDims <- force ds
        return $ Unevaluable [fxs, forcedDims]
  where
    go :: [Int] -> expr builtin -> [TensorLiteralAccessor expr builtin] -> Maybe (expr builtin)
    go dims fxs = \case
      [] -> Nothing
      Wrapper Access {..} : prims -> case getExpr fxs of
        Just t -> case t of
          ZeroDimTensor v -> Just $ mkExpr $ ConstantTensor dims v
          _ -> developerError "Non-zero dimensional tensor argument for ConstTensor"
        Nothing -> go dims fxs prims

evalForeachVector ::
  forall m expr thunk builtin.
  (MonadLogger m, NormalisableExpr expr thunk builtin m, HasTensorLiterals expr builtin, HasVectorExpr expr thunk builtin, BuiltinHasNatLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin) =>
  EvalSimple expr thunk ForeachVectorArgs builtin m
evalForeachVector (ForeachVectorArgs t d f) = do
  fd <- force @expr d
  case fd of
    INatLiteral n -> do
      xs <- traverse (\i -> exprToThunk <$> forceApp f [explicit (exprToThunk $ IIndexLiteral i d)]) [0 .. (n - 1 :: Int)]
      return $ Evaluated $ exprToThunk $ IVecLiteral t d xs
    _ -> return $ Unevaluable [fd]

evalIterate ::
  forall m expr thunk builtin.
  (MonadLogger m, NormalisableExpr expr thunk builtin m, HasNatExpr expr thunk builtin, BuiltinHasIterate builtin) =>
  EvalSimple expr thunk IterateArgs builtin m
evalIterate (IterateArgs t f n e) = do
  fn <- force @expr n
  case fn of
    INatLiteral 0 -> return $ Evaluated e
    INatLiteral v -> do
      let recFn = exprToThunk $ mkBuiltin accessIterateBuiltin () [t, explicit f, explicit (exprToThunk $ INatLiteral (v - 1))]
      Evaluated . exprToThunk <$> forceApp f [explicit recFn, explicit e]
    _ -> return $ Unevaluable [fn]

evalCompareRatTensor ::
  forall expr thunk builtin m.
  (MonadNormBuiltin m, HasBoolExpr expr thunk builtin, BuiltinHasBoolType builtin, HasRatExpr expr thunk builtin, PrintableBuiltin builtin) =>
  ComparisonOp ->
  EvalSimple expr thunk TensorComparisonArgs builtin m
evalCompareRatTensor op (TensorComparisonArgs pointwiseDims rDims xs ys) = do
  fpDims <- force pointwiseDims
  case fpDims of
    IDimNil -> do
      let mkFun (TensorOp2Args ds x y) = mkExpr accessCompareRatTensor (op, TensorComparisonArgs (exprToThunk IDimNil) ds x y)
      let pointwiseArgs = TensorOp2Args rDims xs ys
      evalHeteroTensorOp2 mkFun accessRatTensorLiteral accessBoolTensorLiteral (comparisonOp op) Nothing Nothing Nothing Nothing pointwiseArgs
    IDimCons pDim pDims -> do
      fxs <- force xs
      fys <- force ys

      let compareElements us vs = do
            let subcompare x y = exprToThunk $ mkExpr accessCompareRatTensor (op, TensorComparisonArgs pDims rDims x y)
            let newElements = zipWith subcompare us vs
            let stackArgs = StackTensorArgs (exprToThunk IBoolType) pDim pDims newElements
            Evaluated <$> forceEvaluation accessStackTensor (evalStackTensorWithPrimitives [Wrapper accessBoolTensorLiteral]) stackArgs
      let mkConstElements n args = replicate n (exprToThunk $ mkExpr accessConstTensor $ args {constDims = pDims})

      case (fxs, fys) of
        -- Const, Const
        (getExpr accessConstTensor -> Just cxs, getExpr accessConstTensor -> Just cys) -> do
          let newConstValue = exprToThunk $ mkExpr accessCompareRatTensor (op, TensorComparisonArgs (exprToThunk IDimNil) (exprToThunk IDimNil) (constValue cxs) (constValue cys))
          return $ Evaluated $ exprToThunk $ mkExpr accessConstTensor $ ConstTensorArgs {constType = exprToThunk IBoolType, constValue = newConstValue, constDims = pDims}
        -- Lit , Lit
        (getExpr accessRatTensorLiteral -> Just vxs, getExpr accessRatTensorLiteral -> Just vys) -> do
          compareElements (unstackExpr vxs) (unstackExpr vys)
        -- Stack , Stack
        (getExpr accessStackTensor -> Just vxs, getExpr accessStackTensor -> Just vys) -> do
          compareElements (stackElements vxs) (stackElements vys)
        -- Const , Lit
        (getExpr accessConstTensor -> Just vxs, getExpr accessRatTensorLiteral -> Just vys) -> do
          compareElements (mkConstElements (length $ unstackExpr vys) vxs) (unstackExpr vys)
        -- Lit , Const
        (getExpr accessRatTensorLiteral -> Just vxs, getExpr accessConstTensor -> Just vys) -> do
          compareElements (unstackExpr vxs) (mkConstElements (length $ unstackExpr vxs) vys)
        -- Stack , Lit
        (getExpr accessStackTensor -> Just vxs, getExpr accessRatTensorLiteral -> Just vys) -> do
          compareElements (stackElements vxs) (unstackExpr vys)
        -- Lit , Stack
        (getExpr accessRatTensorLiteral -> Just vxs, getExpr accessStackTensor -> Just vys) -> do
          compareElements (unstackExpr vxs) (stackElements vys)
        -- Stack , Const
        (getExpr accessStackTensor -> Just vxs, getExpr accessConstTensor -> Just vys) -> do
          compareElements (stackElements vxs) (mkConstElements (length $ stackElements vxs) vys)
        -- Const , Stack
        (getExpr accessConstTensor -> Just vxs, getExpr accessStackTensor -> Just vys) -> do
          compareElements (mkConstElements (length $ stackElements vys) vxs) (stackElements vys)
        _ -> return $ Unevaluable []
    _ -> return $ Unevaluable []
  where
    unstackExpr :: Tensor ExtendedRational -> [thunk builtin]
    unstackExpr t = exprToThunk . mkExpr accessRatTensorLiteral <$> unstack t

-----------------------------------------------------------------------------
-- Where

evalWhereTensor ::
  forall expr thunk builtin m.
  (MonadNormBuiltin m, HasTensorLiterals expr builtin, BuiltinHasListLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasNatType builtin, HasTensorExpr expr thunk builtin) =>
  EvalSimple expr thunk WhereTensorArgs builtin m
evalWhereTensor (WhereTensorArgs _dims _input _condition _index) = do
  developerError "evalWhereTensor not yet implemented"
