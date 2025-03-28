{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Vehicle.Data.Builtin.Interface.Normalise where

import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import Control.Monad.Writer (Any (..), MonadWriter (..), execWriterT)
import Data.Maybe (fromMaybe, isJust)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, at, foldTensor, stack, unstack, zipWithTensor, pattern ConstantTensor, pattern ZeroDimTensor)

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

type MonadNormBuiltin m = MonadLogger m

type Eval builtin m =
  (MonadLogger m) =>
  Expr builtin ->
  m (Value builtin)

-- | A method for evaluating an application.
-- Although there is only one implementation of this type, it needs to be
-- passed around as an argument to avoid dependency cycles between
-- this module and the module in which the general NBE algorithm lives in.
type EvalApp builtin m = Value builtin -> [VArg builtin] -> m (Value builtin)

data EvalScheme builtin m
  = forall args. (IsArgs args) => Simple (args (Value builtin) -> m (Value builtin))
  | forall args. (IsArgs args) => NonSimple (EvalApp builtin m -> args (Value builtin) -> m (Value builtin))
  | Derived Identifier
  | None

-- | A type-class for builtins that can be normalised compositionally.
class (PrintableBuiltin builtin) => NormalisableBuiltin builtin where
  evalScheme :: (MonadLogger m) => builtin -> EvalScheme builtin m
  blockingArgs :: builtin -> BlockingArgs
  isTypeClassOp :: builtin -> Bool
  isCast :: (MonadLogger m) => Provenance -> builtin -> Maybe ([GenericArg (Expr builtin)] -> m (Expr builtin))

forceEvalSimpleBuiltin ::
  (IsArgs args, MonadLogger m, Pretty builtin, PrintableBuiltin builtin) =>
  Provenance ->
  builtin ->
  EvalSimple args Expr builtin m ->
  [GenericArg (Expr builtin)] ->
  m (Expr builtin)
forceEvalSimpleBuiltin p b eval spine =
  case getExpr accessSpine spine of
    Just args -> eval args
    Nothing -> return $ normAppList (Builtin p b) spine

--------------------------------------------------------------------------------
-- Evaluation

-- | A method for evaluating builtins that takes in an argument allowing the
-- recursive evaluation of applications. that takes in an argument allowing
-- the subsequent further evaluation of applications.
-- Such recursive evaluation is necessary when evaluating higher order
-- functions such as fold, map etc.
type EvalBuiltin args builtin m =
  (MonadNormBuiltin m) =>
  EvalApp builtin m ->
  args (Value builtin) ->
  m (Maybe (Value builtin))

type EvalSimple args expr builtin m =
  args (expr builtin) ->
  m (expr builtin)

type EvalSimplePartial args builtin m =
  args (Value builtin) ->
  Maybe (m (Value builtin))

evalSimple ::
  (MonadNormBuiltin m, IsArgs args) =>
  builtin ->
  EvalSimplePartial args builtin m ->
  EvalSimple args Value builtin m
evalSimple b eval args = case eval args of
  Just result -> result
  Nothing -> return $ VBuiltin b (mkExpr accessSpine args)

evalNonSimple ::
  (MonadNormBuiltin m, IsArgs args) =>
  EvalApp builtin m ->
  Accessor builtin () ->
  EvalBuiltin args builtin m ->
  args (Value builtin) ->
  m (Value builtin)
evalNonSimple evalApp accessBuiltin eval args = do
  maybeResult <- eval evalApp args
  return $ case maybeResult of
    Just result -> result
    Nothing -> VBuiltin (mkExpr accessBuiltin ()) (mkExpr accessSpine args)

evalTensorOp1 ::
  forall builtin a m.
  (MonadNormBuiltin m, HasTensorExpr Value builtin) =>
  Accessor builtin () ->
  Accessor (Value builtin) (Tensor a) ->
  (a -> a) ->
  EvalSimple TensorOp1Args Value builtin m
evalTensorOp1 accessBuiltinOp accessLit op args =
  evalSimple (mkExpr accessBuiltinOp ()) eval args
  where
    eval :: EvalSimplePartial TensorOp1Args builtin m
    eval = \case
      TensorOp1Args _ds (getExpr accessLit -> Just t) ->
        Just $ return $ mkExpr accessLit $ fmap op t
      TensorOp1Args (argExpr -> ICons _ d _) (getExpr accessConstTensor -> Just xs) ->
        Just $ mkExpr accessConstTensor <$> traverseConstTensorValue (evalFull d) xs
      TensorOp1Args (argExpr -> ICons _ d _) (getExpr accessStackTensor -> Just xs) ->
        Just $ mkExpr accessStackTensor <$> traverseStackTensorElements (evalFull d) xs
      _ -> Nothing

    evalFull :: Value builtin -> Value builtin -> m (Value builtin)
    evalFull d x = evalSimple (mkExpr accessBuiltinOp ()) eval (TensorOp1Args (implicitIrrelevant d) x)

evalTensorOp2 ::
  forall builtin a m.
  (MonadNormBuiltin m, HasTensorExpr Value builtin, Eq a) =>
  Accessor builtin () ->
  Accessor (Value builtin) (Tensor a) ->
  (a -> a -> a) ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  EvalSimple TensorOp2Args Value builtin m
evalTensorOp2 accessBuiltin accessLit =
  evalHeteroTensorOp2 (mkExpr accessBuiltin ()) accessLit accessLit

evalHeteroTensorOp2 ::
  forall builtin a b m.
  (MonadNormBuiltin m, HasTensorExpr Value builtin, Eq a, Eq b) =>
  builtin ->
  Accessor (Value builtin) (Tensor a) ->
  Accessor (Value builtin) (Tensor b) ->
  (a -> a -> b) ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  Maybe a ->
  EvalSimple TensorOp2Args Value builtin m
evalHeteroTensorOp2 b inputLit outputLit op leftUnit rightUnit leftZero rightZero args =
  evalSimple b eval args
  where
    eval :: EvalSimplePartial TensorOp2Args builtin m
    eval = \case
      TensorOp2Args _ds (getExpr inputLit -> Just xs) (getExpr inputLit -> Just ys) ->
        Just $ return $ mkExpr outputLit $ zipWithTensor op xs ys
      TensorOp2Args (argExpr -> ICons _ _ ds) (getExpr accessConstTensor -> Just xs) (getExpr accessConstTensor -> Just ys) ->
        Just $ do
          newConstValue <- evalFull ds (constValue xs) (constValue ys)
          return $ mkExpr accessConstTensor $ xs {constValue = newConstValue}
      -- Unlike const tensors, we need to eval stack tensors as after being combined with constants, short-circuiting of
      -- operations may allow for further reduction.
      TensorOp2Args (argExpr -> ICons _ _ ds) (getExpr inputLit -> Just xs) (getExpr accessStackTensor -> Just ys) ->
        Just $ do
          newElements <- zipWithM (evalFull ds) (unstackExpr xs) (stackElements ys)
          evalStackTensorWithPrimitives [Wrapper outputLit] $ ys {stackElements = newElements}
      TensorOp2Args (argExpr -> ICons _ _ ds) (getExpr accessStackTensor -> Just xs) (getExpr inputLit -> Just ys) ->
        Just $ do
          newElements <- zipWithM (evalFull ds) (stackElements xs) (unstackExpr ys)
          evalStackTensorWithPrimitives [Wrapper outputLit] $ xs {stackElements = newElements}
      TensorOp2Args (argExpr -> ICons _ _ ds) (getExpr accessStackTensor -> Just xs) (getExpr accessStackTensor -> Just ys) ->
        Just $ do
          newElements <- zipWithM (evalFull ds) (stackElements xs) (stackElements ys)
          evalStackTensorWithPrimitives [Wrapper outputLit] $ xs {stackElements = newElements}
      TensorOp2Args _ds xs ys
        | isJust leftUnit && leftUnit == getConstValue xs -> Just $ return ys
      TensorOp2Args _ds xs ys
        | isJust rightUnit && rightUnit == getConstValue ys -> Just $ return xs
      TensorOp2Args _ds xs _ys
        | isJust leftZero && leftZero == getConstValue xs -> Just $ return xs
      TensorOp2Args _ds _xs ys
        | isJust rightZero && rightZero == getConstValue ys -> Just $ return ys
      _ -> Nothing

    evalFull :: Value builtin -> Value builtin -> Value builtin -> m (Value builtin)
    evalFull d x y = evalSimple b eval (TensorOp2Args (implicitIrrelevant d) x y)

    unstackExpr :: Tensor a -> [Value builtin]
    unstackExpr xs = mkExpr inputLit <$> unstack xs

    getConstValue :: Value builtin -> Maybe a
    getConstValue value = case getExpr inputLit value of
      Just (ConstantTensor _ v) -> Just v
      _ -> case getExpr accessConstTensor value of
        Just constTensor -> getConstValue (constValue constTensor)
        _ -> Nothing

evalReduceTensor ::
  forall builtin a m.
  (MonadNormBuiltin m, HasTensorExpr Value builtin, PrintableBuiltin builtin) =>
  Accessor builtin () ->
  Accessor (Value builtin) (Tensor a) ->
  EvalSimple TensorOp2Args Value builtin m ->
  (a -> a -> a) ->
  EvalSimple TensorReductionArgs Value builtin m
evalReduceTensor accessReductionOp accessLit evalOp2 op2 args =
  evalSimple (mkExpr accessReductionOp ()) eval args
  where
    eval :: EvalSimplePartial TensorReductionArgs builtin m
    eval = \case
      TensorOp2Args _ (getExpr accessLit -> Just e) (getExpr accessLit -> Just xs) ->
        Just $ return $ mkExpr accessLit $ foldTensor op2 e xs
      TensorOp2Args (argExpr -> ICons _ _ ds) e (getExpr accessStackTensor -> Just xs) ->
        Just $ foldM (foldFn e (implicitIrrelevant ds)) e (stackElements xs)
      _ -> Nothing

    evalFull :: VArg builtin -> Value builtin -> Value builtin -> m (Value builtin)
    evalFull ds e xs = evalSimple (mkExpr accessReductionOp ()) eval (TensorOp2Args ds e xs)

    evalBop :: VArg builtin -> Value builtin -> Value builtin -> m (Value builtin)
    evalBop ds xs ys = evalOp2 (TensorOp2Args ds xs ys)

    foldFn e ds x y = do
      x' <- evalFull ds e x
      evalBop ds x' y

-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------
-- Bool

evalNot :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp1Args Value builtin m
evalNot = evalTensorOp1 accessNotBuiltin accessBoolTensorLiteral not

evalAnd :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalAnd = evalTensorOp2 accessAndBuiltin accessBoolTensorLiteral (&&) (Just True) (Just True) (Just False) (Just False)

evalOr :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalOr args = evalTensorOp2 accessOrBuiltin accessBoolTensorLiteral (||) (Just False) (Just False) (Just True) (Just True) args

evalImplies :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalImplies (TensorOp2Args ds xs ys) = do
  notXs <- evalNot (TensorOp1Args ds xs)
  evalOr (TensorOp2Args ds notXs ys)

evalReduceAndTensor :: (MonadNormBuiltin m, HasBoolExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceAndTensor = evalReduceTensor accessReduceAndBuiltin accessBoolTensorLiteral evalAnd (&&)

evalReduceOrTensor :: (MonadNormBuiltin m, HasBoolExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceOrTensor = evalReduceTensor accessReduceOrBuiltin accessBoolTensorLiteral evalOr (||)

evalIf :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple IfArgs Value builtin m
evalIf args@(IfArgs _t c e1 e2) = return $ case c of
  IBoolLiteral True -> e1
  IBoolLiteral False -> e2
  _ -> mkExpr accessIf args

-----------------------------------------------------------------------------
-- Index

evalCompareIndex ::
  (MonadNormBuiltin m, HasBoolExpr Value builtin, BuiltinHasIndexLiterals builtin) =>
  ComparisonOp ->
  EvalSimple IndexComparisonArgs Value builtin m
evalCompareIndex op = \case
  IndexCompArgs _ _ (IIndexLiteral x) (IIndexLiteral y) -> return $ IBoolLiteral (comparisonOp op x y)
  args -> return $ mkExpr accessCompareIndex (op, args)

-----------------------------------------------------------------------------
-- Nat

evalAddNat ::
  (MonadNormBuiltin m, BuiltinHasNatLiterals builtin) =>
  EvalSimple Op2Args Value builtin m
evalAddNat = \case
  Op2Args (INatLiteral x) (INatLiteral y) -> return $ INatLiteral (x + y)
  args -> return $ mkExpr accessAddNat args

evalMulNat ::
  (MonadNormBuiltin m, BuiltinHasNatLiterals builtin) =>
  EvalSimple Op2Args Value builtin m
evalMulNat = \case
  Op2Args (INatLiteral x) (INatLiteral y) -> return $ INatLiteral (x * y)
  args -> return $ mkExpr accessMulNat args

evalCompareNat ::
  (MonadNormBuiltin m, HasBoolExpr Value builtin, BuiltinHasNatLiterals builtin) =>
  ComparisonOp ->
  EvalSimple Op2Args Value builtin m
evalCompareNat op = \case
  Op2Args (INatLiteral x) (INatLiteral y) -> return $ IBoolLiteral (comparisonOp op x y)
  args -> return $ mkExpr accessCompareNat (op, args)

-----------------------------------------------------------------------------
-- List

evalMapList ::
  forall builtin m.
  (MonadLogger m, BuiltinHasListLiterals builtin) =>
  EvalApp builtin m ->
  MapListArgs (Value builtin) ->
  m (Value builtin)
evalMapList evalApp (MapListArgs a b f xs) = eval xs
  where
    eval :: Value builtin -> m (Value builtin)
    eval = \case
      INil _ -> return $ INil b
      ICons _ v vs -> do
        v' <- evalApp f [explicit v]
        vs' <- evalMapList evalApp (recArgs vs)
        return $ ICons b v' vs'
      vs -> return $ mkExpr accessMapList (recArgs vs)

    recArgs :: Value builtin -> MapListArgs (Value builtin)
    recArgs = MapListArgs a b f

evalFoldList ::
  forall m builtin.
  (MonadLogger m, BuiltinHasListLiterals builtin) =>
  EvalApp builtin m ->
  FoldListArgs (Value builtin) ->
  m (Value builtin)
evalFoldList evalApp (FoldListArgs a b f e xs) = eval xs
  where
    eval :: Value builtin -> m (Value builtin)
    eval = \case
      INil _ -> return e
      ICons _ v vs -> do
        r <- evalFoldList evalApp (recArgs vs)
        evalApp f [explicit v, explicit r]
      vs -> return $ mkExpr accessFoldList (recArgs vs)

    recArgs :: Value builtin -> FoldListArgs (Value builtin)
    recArgs = FoldListArgs a b f e

-----------------------------------------------------------------------------
-- Rational tensors

evalNegRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp1Args Value builtin m
evalNegRatTensor = evalTensorOp1 accessNegRatTensorBuiltin accessRatTensorLiteral (\x -> -x)

evalAddRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalAddRatTensor = evalTensorOp2 accessAddRatTensorBuiltin accessRatTensorLiteral (+) (Just 0) (Just 0) Nothing Nothing

evalMulRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalMulRatTensor = evalTensorOp2 accessMulRatTensorBuiltin accessRatTensorLiteral (*) (Just 1) (Just 1) (Just 0) (Just 0)

evalSubRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalSubRatTensor = evalTensorOp2 accessSubRatTensorBuiltin accessRatTensorLiteral (-) Nothing (Just 0) Nothing Nothing

evalDivRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalDivRatTensor args = evalTensorOp2 accessDivRatTensorBuiltin accessRatTensorLiteral (/) Nothing (Just 1) Nothing Nothing args

evalMinRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalMinRatTensor = evalTensorOp2 accessMinRatTensorBuiltin accessRatTensorLiteral min Nothing Nothing Nothing Nothing

evalMaxRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalMaxRatTensor = evalTensorOp2 accessMaxRatTensorBuiltin accessRatTensorLiteral max Nothing Nothing Nothing Nothing

evalPowRat ::
  (MonadNormBuiltin m, HasRatExpr Value builtin, BuiltinHasNatLiterals builtin) =>
  EvalSimple TensorOp2Args Value builtin m
evalPowRat = \case
  TensorOp2Args _ (IRatTensor xs) (INatLiteral n) -> return $ IRatTensor (fmap (^^ n) xs)
  args -> return $ mkExpr accessPowRatTensor args

evalReduceAddRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceAddRatTensor = evalReduceTensor accessReduceAddRatBuiltin accessRatTensorLiteral evalAddRatTensor (+)

evalReduceMulRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceMulRatTensor = evalReduceTensor accessReduceMulRatBuiltin accessRatTensorLiteral evalMulRatTensor (*)

evalReduceMinRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceMinRatTensor = evalReduceTensor accessReduceMinRatBuiltin accessRatTensorLiteral evalMinRatTensor min

evalReduceMaxRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceMaxRatTensor = evalReduceTensor accessReduceMaxRatBuiltin accessRatTensorLiteral evalMaxRatTensor max

evalCompareRatTensor ::
  (MonadNormBuiltin m, HasBoolExpr Value builtin, HasRatExpr Value builtin, PrintableBuiltin builtin) =>
  ComparisonOp ->
  EvalSimple TensorOp2Args Value builtin m
evalCompareRatTensor op =
  evalHeteroTensorOp2
    (mkExpr accessCompareRatTensorBuiltin op)
    accessRatTensorLiteral
    accessBoolTensorLiteral
    (comparisonOp op)
    Nothing
    Nothing
    Nothing
    Nothing

-----------------------------------------------------------------------------
-- Generic tensor operations

data TensorLiteralAccessor builtin
  = forall a. (Eq a) => Wrapper (Accessor (Value builtin) (Tensor a))

type TensorOp1EvalData builtin m =
  ( TensorOp1Accessor (Value builtin),
    EvalSimple TensorOp1Args Value builtin m
  )

type TensorOp2EvalData builtin m =
  ( TensorOp2Accessor (Value builtin),
    EvalSimple TensorOp2Args Value builtin m
  )

class HasPrimitives builtin where
  tensorLiterals :: [TensorLiteralAccessor builtin]
  tensorOp1s :: (MonadNormBuiltin m) => [TensorOp1EvalData builtin m]
  tensorOp2s :: (MonadNormBuiltin m) => [TensorOp2EvalData builtin m]

evalAt ::
  forall builtin m.
  (MonadNormBuiltin m, HasPrimitives builtin, BuiltinHasListLiterals builtin, BuiltinHasIndexLiterals builtin, HasTensorExpr Value builtin) =>
  EvalSimple AtArgs Value builtin m
evalAt args@(AtArgs t d ds tensor index) = do
  fromMaybe (return $ mkExpr accessAtTensor args) $
    goOp1 tensorOp1s
      <|> goOp2 tensorOp2s
      <|> case index of
        IIndexLiteral i ->
          goLiterals i tensorLiterals
            <|> case tensor of
              (getExpr accessStackTensor -> Just stackArgs) -> Just $ return $ stackElements stackArgs !! i
              (getExpr accessConstTensor -> Just constArgs) -> Just $ return $ mkExpr accessConstTensor $ constArgs {constDims = argExpr ds}
              _ -> Nothing
        _ -> Nothing
  where
    recEvalAt :: Value builtin -> m (Value builtin)
    recEvalAt ys = evalAt (AtArgs t d ds ys index)

    goOp1 :: [TensorOp1EvalData builtin m] -> Maybe (m (Value builtin))
    goOp1 = \case
      (accessOp1, evalOp1) : remainingOp1s -> case getExpr accessOp1 tensor of
        Just (TensorOp1Args _ xs) -> Just $ do
          xsi <- recEvalAt xs
          evalOp1 (TensorOp1Args ds xsi)
        _ -> goOp1 remainingOp1s
      [] -> Nothing

    goOp2 :: [TensorOp2EvalData builtin m] -> Maybe (m (Value builtin))
    goOp2 = \case
      (accessOp2, evalOp2) : remainingOps2 -> case getExpr accessOp2 tensor of
        Just (TensorOp2Args _ xs ys) -> Just $ do
          xsi <- recEvalAt xs
          ysi <- recEvalAt ys
          evalOp2 $ TensorOp2Args ds xsi ysi
        _ -> goOp2 remainingOps2
      _ -> Nothing

    goLiterals :: Int -> [TensorLiteralAccessor builtin] -> Maybe (m (Value builtin))
    goLiterals i literals = case literals of
      Wrapper Access {..} : remainingLiterals -> case getExpr tensor of
        Just xs -> Just $ return $ mkExpr (xs `at` i)
        Nothing -> goLiterals i remainingLiterals
      _ -> Nothing

evalStackTensor ::
  (MonadNormBuiltin m, HasPrimitives builtin, BuiltinHasNatLiterals builtin, HasTensorExpr Value builtin) =>
  EvalSimple StackTensorArgs Value builtin m
evalStackTensor = evalStackTensorWithPrimitives tensorLiterals

evalStackTensorWithPrimitives ::
  (MonadNormBuiltin m, BuiltinHasNatLiterals builtin, HasTensorExpr Value builtin) =>
  [TensorLiteralAccessor builtin] ->
  EvalSimple StackTensorArgs Value builtin m
evalStackTensorWithPrimitives tensorLits args@(StackTensorArgs _t d ds xs) =
  return $
    fromMaybe (mkExpr accessStackTensor args) $
      -- If we know that all the tensors being stacked are concrete tensors, then
      -- we must know the dimensions as well.
      case (d, getDims (argExpr ds)) of
        (INatLiteral n, Just ns) | length xs == n -> go ns xs tensorLits
        _ -> Nothing
  where
    go :: [Int] -> [Value builtin] -> [TensorLiteralAccessor builtin] -> Maybe (Value builtin)
    go elemDims elements = \case
      Wrapper Access {..} : prims -> case traverse getExpr elements of
        Just xss -> Just $ mkExpr $ stack elemDims xss
        Nothing -> go elemDims elements prims
      [] -> Nothing

evalConstTensor ::
  forall builtin m.
  (MonadNormBuiltin m, HasPrimitives builtin, BuiltinHasNatLiterals builtin, HasTensorExpr Value builtin) =>
  EvalSimple ConstTensorArgs Value builtin m
evalConstTensor args@(ConstTensorArgs _t xs ds) =
  -- Pattern matching on ds here is technically a bug as blocking will not
  -- function correctly. However, to fix it we would need to go via `StackTensor`
  -- and in particular make `StackTensor` take the size argument as an expression.
  -- Our type-system can't handle that easily yet.
  case (`go` tensorLiterals) =<< getDims ds of
    Just result -> return result
    _ -> return $ mkExpr accessConstTensor args
  where
    go :: [Int] -> [TensorLiteralAccessor builtin] -> Maybe (Value builtin)
    go dims = \case
      [] -> Nothing
      Wrapper Access {..} : prims -> case getExpr xs of
        Just t -> case t of
          ZeroDimTensor v -> Just $ mkExpr $ ConstantTensor dims v
          _ -> developerError "Non-zero dimensional tensor argument for ConstTensor"
        Nothing -> go dims prims

evalForeach ::
  (MonadLogger m, HasPrimitives builtin, HasTensorExpr Value builtin, BuiltinHasNatLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin) =>
  EvalApp builtin m ->
  ForeachArgs (Value builtin) ->
  m (Value builtin)
evalForeach evalApp args@(ForeachArgs t d ds f) = case d of
  INatLiteral n -> do
    xs <- traverse (\i -> evalApp f [explicit (IIndexLiteral i)]) [0 .. (n - 1 :: Int)]
    evalStackTensor (StackTensorArgs t d ds xs)
  _ -> return $ mkExpr accessForeachTensor args

evalIterate ::
  (MonadLogger m, BuiltinHasNatLiterals builtin, BuiltinHasIterate builtin) =>
  EvalApp builtin m ->
  IterateArgs (Value builtin) ->
  m (Value builtin)
evalIterate evalApp args@(IterateArgs t f n e) = case n of
  INatLiteral 0 -> return e
  INatLiteral v -> do
    let recFn = VBuiltin (mkExpr accessIterateBuiltin ()) [t, explicit f, explicit (INatLiteral (v - 1))]
    evalApp f [explicit recFn, explicit e]
  _ -> return $ mkExpr accessIterate args

-----------------------------------------------------------------------------
-- Blocking arguments

type BlockingArguments builtin = [Value builtin]

data BlockingArgs
  = Unknown
  | Known [Int]
  deriving (Eq)

noBlockingArgs :: BlockingArgs
noBlockingArgs = Known []

blockingArgsNot :: BlockingArgs
blockingArgsNot = Known [1]

blockingArgsAnd :: BlockingArgs
blockingArgsAnd = Known [1, 2]

blockingArgsOr :: BlockingArgs
blockingArgsOr = Known [1, 2]

blockingArgsNeg :: NegDomain -> BlockingArgs
blockingArgsNeg = \case
  NegRatTensor -> Known [1]

blockingArgsAdd :: AddDomain -> BlockingArgs
blockingArgsAdd = \case
  AddNat -> Known [0, 1]
  AddRatTensor -> Known [1, 2]

blockingArgsMul :: MulDomain -> BlockingArgs
blockingArgsMul = \case
  MulNat -> Known [0, 1]
  MulRatTensor -> Known [1, 2]

blockingArgsSub :: SubDomain -> BlockingArgs
blockingArgsSub = \case
  SubRatTensor -> Known [1, 2]

blockingArgsDiv :: DivDomain -> BlockingArgs
blockingArgsDiv = \case
  DivRatTensor -> Known [1, 2]

blockingArgsMin :: MinDomain -> BlockingArgs
blockingArgsMin = \case
  MinRatTensor -> Known [1, 2]

blockingArgsMax :: MaxDomain -> BlockingArgs
blockingArgsMax = \case
  MaxRatTensor -> Known [1, 2]

functionBlockingArgs :: BuiltinFunction -> BlockingArgs
functionBlockingArgs = \case
  QuantifyRatTensor {} -> noBlockingArgs
  Not -> blockingArgsNot
  And -> blockingArgsAnd
  Or -> blockingArgsOr
  Neg dom -> blockingArgsNeg dom
  Add dom -> blockingArgsAdd dom
  Sub dom -> blockingArgsSub dom
  Mul dom -> blockingArgsMul dom
  Div dom -> blockingArgsDiv dom
  Min dom -> blockingArgsMin dom
  Max dom -> blockingArgsMax dom
  PowRat -> Known [0, 1]
  CompareIndex _op -> Known [2, 3]
  CompareNat _op -> Known [0, 1]
  CompareRatTensorPointwise _op -> Known [1, 2]
  If -> Known [1]
  At -> Known [3, 4]
  FoldList -> Known [4]
  MapList -> Known [3]
  Implies -> noBlockingArgs
  ConstTensor -> Known [0, 1]
  ReduceAddRatTensor -> Known [1]
  ReduceMulRatTensor -> Known [1]
  ReduceMinRatTensor -> Known [1]
  ReduceMaxRatTensor -> Known [1]
  ReduceOrTensor -> Known [1]
  ReduceAndTensor -> Known [1]
  Foreach -> Known [1]
  Iterate -> Known [2]
  StackTensor -> Unknown

derivedFunctionBlockingArgs :: DerivedFunction -> BlockingArgs
derivedFunctionBlockingArgs = \case
  TypeAnn -> noBlockingArgs
  QuantifyIndex {} -> Known [0]
  QuantifyInList {} -> Known [2]
  CompareRatTensorReduced {} -> Known [1, 2]
  AppendList -> Known [1, 2]

castBlockingArgs :: BuiltinCast -> BlockingArgs
castBlockingArgs = \case
  FromVectorToList -> Known [1]
  FromNat FromNatToIndex -> Known [1]
  FromNat FromNatToNat -> Known [0]
  FromNat FromNatToRat -> Known [0]
  FromRat FromRatToRat -> noBlockingArgs

traverseBlockingArgs ::
  forall m builtin.
  (MonadLogger m, NormalisableBuiltin builtin) =>
  (Value builtin -> m (Value builtin)) ->
  builtin ->
  Spine builtin ->
  m (Spine builtin)
traverseBlockingArgs f b spine = case blockingArgs b of
  Unknown -> traverseSpine f spine
  Known xs -> recurse spine 0 xs
  where
    recurse ::
      Spine builtin ->
      Int ->
      [Int] ->
      m (Spine builtin)
    recurse [] _currentIndex _blockingArgs = return []
    recurse args _currentIndex [] = return args
    recurse (arg : args) currentIndex (blockingIndex : blockingIndices)
      | currentIndex == blockingIndex = do
          arg' <- traverse f arg
          args' <- recurse args (currentIndex + 1) blockingIndices
          return $ arg' : args'
      | otherwise = do
          args' <- recurse args (currentIndex + 1) (blockingIndex : blockingIndices)
          return $ arg : args'

isValueBlocked ::
  (NormalisableBuiltin builtin) =>
  (MonadLogger m, MonadWriter Any m) =>
  Value builtin ->
  m (Value builtin)
isValueBlocked v = do
  let blocked = case v of
        VUniverse {} -> False
        VMeta {} -> True
        VFreeVar {} -> False
        VBoundVar {} -> False
        VLam {} -> False
        VPi {} -> False
        VBuiltin b _ -> blockingArgs b /= noBlockingArgs
  tell (Any blocked)
  return v

isBlocked ::
  forall m builtin.
  (MonadLogger m, NormalisableBuiltin builtin) =>
  builtin ->
  Spine builtin ->
  m Bool
isBlocked b args = do
  u <- execWriterT @m $ traverseBlockingArgs isValueBlocked b args
  return $ getAny u
