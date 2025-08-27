{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Vehicle.Data.Builtin.Interface.Normalise where

import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import Data.Maybe (fromMaybe, isJust)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Blocked
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, at, extendTensor, foldTensor, mapTensor, stack, unstack, zipWithTensor, pattern ConstantTensor, pattern ZeroDimTensor)

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

-- | A method for evaluating an application.
-- Although there is only one implementation of this type, it needs to be
-- passed around as an argument to avoid dependency cycles between
-- this module and the module in which the general NBE algorithm lives in.
type EvalApp builtin m = NamedBoundCtx -> Value builtin -> [VArg builtin] -> m (Value builtin)

type Eval builtin m = NamedBoundCtx -> BoundEnv builtin -> Expr builtin -> m (Value builtin)

data EvalScheme builtin m
  = forall args. (IsArgs args) => Simple (args (Value builtin) -> m (Value builtin))
  | forall args. (IsArgs args) => NonSimple (NamedBoundCtx -> EvalApp builtin m -> Eval builtin m -> args (Value builtin) -> m (Value builtin))
  | Derived Identifier
  | None

-- | A type-class for builtins that can be normalised compositionally.
class (PrintableBuiltin builtin) => NormalisableBuiltin builtin where
  evalScheme :: (MonadLogger m) => builtin -> EvalScheme builtin m
  blockingStatus :: builtin -> Spine builtin -> BlockingStatus builtin
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
  (MonadNormBuiltin m, HasTensorExpr Value builtin, Eq a) =>
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
        Just $ return $ mkExpr accessLit $ mapTensor op t
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
      TensorOp2Args (argExpr -> INil _) _e xs ->
        Just $ return xs
      _ -> Nothing

    evalFull :: VArg builtin -> Value builtin -> Value builtin -> m (Value builtin)
    evalFull ds e xs = evalSimple (mkExpr accessReductionOp ()) eval (TensorOp2Args ds e xs)

    evalBop :: VArg builtin -> Value builtin -> Value builtin -> m (Value builtin)
    evalBop ds xs ys = evalOp2 (TensorOp2Args ds xs ys)

    foldFn e ds r y = do
      y' <- evalFull ds e y
      evalBop ds r y'

-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------
-- Not

evalNot :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp1Args Value builtin m
evalNot = evalTensorOp1 accessNotBuiltin accessBoolTensorLiteral not

-----------------------------------------------------------------------------
-- And

evalAnd :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalAnd = evalTensorOp2 accessAndBuiltin accessBoolTensorLiteral (&&) (Just True) (Just True) (Just False) (Just False)

-----------------------------------------------------------------------------
-- Or

evalOr :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalOr args = evalTensorOp2 accessOrBuiltin accessBoolTensorLiteral (||) (Just False) (Just False) (Just True) (Just True) args

-----------------------------------------------------------------------------
-- Implies

evalImplies :: (MonadNormBuiltin m, HasBoolExpr Value builtin) => EvalSimple TensorOp2Args Value builtin m
evalImplies (TensorOp2Args ds xs ys) = do
  notXs <- evalNot (TensorOp1Args ds xs)
  evalOr (TensorOp2Args ds notXs ys)

-----------------------------------------------------------------------------
-- ReduceAnd

evalReduceAndTensor ::
  forall m builtin.
  (MonadNormBuiltin m, PrintableBuiltin builtin, NormalisableBuiltin builtin, BuiltinHasNatType builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin, BuiltinHasTensors builtin, BuiltinHasListLiterals builtin, BuiltinHasNatLiterals builtin, BuiltinHasBoolLiterals builtin, HasTensorLiterals builtin, HasLiftableTensorOperations builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  EvalSimple TensorReductionArgs Value builtin m
evalReduceAndTensor ctx evalApp eval args@(TensorOp2Args dims e tensor) = case e of
  IBoolLiteral True -> go tensor
  _ -> unoptimisedEvalReduceAndTensor args
  where
    go :: Value builtin -> m (Value builtin)
    go = \case
      (getExpr accessAndTensor -> Just (TensorOp2Args ds xs ys)) -> do
        xs' <- go xs
        ys' <- go ys
        evalAnd (TensorOp2Args ds xs' ys')
      vs -> do
        result <- fuseReduceAndForeachTensor ctx evalApp eval tensor
        case result of
          Nothing -> unoptimisedEvalReduceAndTensor (TensorOp2Args dims e vs)
          Just (newDims, fusedTensor) -> return $ mkExpr accessReduceAnd (TensorOp2Args newDims e fusedTensor)

-- | An optimised evaluation procedure for `Foreach` that attempts to minimise the
-- amount of work needed by lifting operations to higher-tensor levels.
-- For example `foreach i . xs ! i + ys ! i` becomes `xs + ys`.
fuseReduceAndForeachTensor ::
  (MonadLogger m, PrintableBuiltin builtin, NormalisableBuiltin builtin, BuiltinHasNatType builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin, BuiltinHasTensors builtin, BuiltinHasListLiterals builtin, BuiltinHasNatLiterals builtin, BuiltinHasBoolLiterals builtin, HasTensorLiterals builtin, HasLiftableTensorOperations builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  Value builtin ->
  m (Maybe (VArg builtin, Value builtin))
fuseReduceAndForeachTensor ctx evalApp eval value = do
  fusionEnter ctx value
  fusionExit ctx =<< case getExpr accessForeachTensor value of
    Just (ForeachTensorArgs typ d _ (VLam binder (Closure env body))) -> do
      let lv = boundCtxLv ctx
      let newEnv = extendEnvWithBound lv binder env
      let newCtx = nameOf binder : ctx
      body' <- eval newCtx newEnv body
      case getExpr accessReduceAnd body' of
        Just (TensorOp2Args tensorDims (IBoolLiteral True) tensor) -> do
          (newDims, newTensor) <- fromMaybe (tensorDims, tensor) <$> fuseReduceAndForeachTensor newCtx evalApp eval tensor
          let newTensor' = quote mempty (lv + 1) newTensor
          let newLam = VLam binder (Closure (namedBoundContextToEnv ctx) newTensor')
          let newForeachArgs = ForeachTensorArgs typ d newDims newLam
          newBody' <- evalForeachTensor newCtx evalApp eval newForeachArgs
          return $ Just (implicit (ICons (implicit INatType) d (argExpr newDims)), newBody')
        _ -> return Nothing
    _ -> return Nothing

unoptimisedEvalReduceAndTensor ::
  (MonadNormBuiltin m, HasBoolExpr Value builtin, PrintableBuiltin builtin) =>
  EvalSimple TensorReductionArgs Value builtin m
unoptimisedEvalReduceAndTensor =
  evalReduceTensor accessReduceAndBuiltin accessBoolTensorLiteral evalAnd (&&)

-----------------------------------------------------------------------------
-- ReduceOr

evalReduceOrTensor :: (MonadNormBuiltin m, HasBoolExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceOrTensor = evalReduceTensor accessReduceOrBuiltin accessBoolTensorLiteral evalOr (||)

-----------------------------------------------------------------------------
-- If

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
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  MapListArgs (Value builtin) ->
  m (Value builtin)
evalMapList ctx evalApp eval (MapListArgs a b f xs) = evalList xs
  where
    evalList :: Value builtin -> m (Value builtin)
    evalList = \case
      INil _ -> return $ INil b
      ICons _ v vs -> do
        v' <- evalApp ctx f [explicit v]
        vs' <- evalMapList ctx evalApp eval (recArgs vs)
        return $ ICons b v' vs'
      vs -> return $ mkExpr accessMapList (recArgs vs)

    recArgs :: Value builtin -> MapListArgs (Value builtin)
    recArgs = MapListArgs a b f

evalFoldList ::
  forall m builtin.
  (MonadLogger m, BuiltinHasListLiterals builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  FoldListArgs (Value builtin) ->
  m (Value builtin)
evalFoldList ctx evalApp eval (FoldListArgs a b f e xs) = evalList xs
  where
    evalList :: Value builtin -> m (Value builtin)
    evalList = \case
      INil _ -> return e
      ICons _ v vs -> do
        r <- evalFoldList ctx evalApp eval (recArgs vs)
        evalApp ctx f [explicit v, explicit r]
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
  TensorOp2Args _ (IRatTensor xs) (INatLiteral n) -> return $ IRatTensor (mapTensor (^^ n) xs)
  args -> return $ mkExpr accessPowRatTensor args

evalReduceAddRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceAddRatTensor = evalReduceTensor accessReduceAddRatBuiltin accessRatTensorLiteral evalAddRatTensor (+)

evalReduceMulRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceMulRatTensor = evalReduceTensor accessReduceMulRatBuiltin accessRatTensorLiteral evalMulRatTensor (*)

evalReduceMinRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceMinRatTensor = evalReduceTensor accessReduceMinRatBuiltin accessRatTensorLiteral evalMinRatTensor min

evalReduceMaxRatTensor :: (MonadNormBuiltin m, HasRatExpr Value builtin, PrintableBuiltin builtin) => EvalSimple TensorReductionArgs Value builtin m
evalReduceMaxRatTensor = evalReduceTensor accessReduceMaxRatBuiltin accessRatTensorLiteral evalMaxRatTensor max

evalCompareRatTensorPointwise ::
  (MonadNormBuiltin m, HasBoolExpr Value builtin, HasRatExpr Value builtin, PrintableBuiltin builtin) =>
  ComparisonOp ->
  EvalSimple TensorOp2Args Value builtin m
evalCompareRatTensorPointwise op =
  evalHeteroTensorOp2
    (mkExpr accessCompareRatTensorPointwiseBuiltin op)
    accessRatTensorLiteral
    accessBoolTensorLiteral
    (comparisonOp op)
    Nothing
    Nothing
    Nothing
    Nothing

-----------------------------------------------------------------------------
-- Generic vector operations

evalAtVector ::
  forall builtin m.
  (MonadNormBuiltin m, BuiltinHasIndexLiterals builtin, HasVectorExpr Value builtin) =>
  EvalSimple AtVectorArgs Value builtin m
evalAtVector args@(AtVectorArgs _t _d vector index) = do
  fromMaybe (return $ mkExpr accessAtVector args) $
    case (vector, index) of
      (IVecLiteral _t _d xs, IIndexLiteral i) -> Just $ return $ xs !! i
      _ -> Nothing

-----------------------------------------------------------------------------
-- Generic tensor operations
-----------------------------------------------------------------------------

type TensorOpEvalData args builtin m =
  ( Destruct (Value builtin) (args (Value builtin)),
    EvalSimple args Value builtin m,
    VType builtin
  )

class HasLiftableTensorOperations builtin where
  liftableTensorOp1s :: (MonadNormBuiltin m) => [TensorOpEvalData TensorOp1Args builtin m]
  liftableTensorOp2s :: (MonadNormBuiltin m) => [TensorOpEvalData TensorOp2Args builtin m]

data TensorLiteralAccessor builtin
  = forall a. (Eq a) => Wrapper (Accessor (Value builtin) (Tensor a))

class HasTensorLiterals builtin where
  tensorLiterals :: [TensorLiteralAccessor builtin]

-----------------------------------------------------------------------------
-- At

-- | An optimised evaluation procedure for `At` that attempts to minimise the
-- amount of work needed by deferring evaluation of operations until after indexing.
-- For example `(xs + ys) ! i` becomes `xs ! i + ys ! i`.
evalAtTensor ::
  forall builtin m.
  (MonadNormBuiltin m, HasTensorLiterals builtin, HasLiftableTensorOperations builtin, BuiltinHasListLiterals builtin, BuiltinHasIndexLiterals builtin, HasTensorExpr Value builtin, BuiltinHasForeach builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  EvalSimple AtTensorArgs Value builtin m
evalAtTensor ctx evalApp eval args@(AtTensorArgs t d ds tensor index) =
  fromMaybe (unoptimisedEvalAtTensor args) $
    goOp1 liftableTensorOp1s
      <|> goOp2 liftableTensorOp2s
      <|> goForeach
  where
    recEvalAt :: Value builtin -> m (Value builtin)
    recEvalAt ys = evalAtTensor ctx evalApp eval (AtTensorArgs t d ds ys index)

    goOp1 :: [TensorOpEvalData TensorOp1Args builtin m] -> Maybe (m (Value builtin))
    goOp1 = \case
      (accessOp1, evalOp1, _) : remainingOp1s -> case accessOp1 tensor of
        Just (TensorOp1Args _ xs) -> Just $ do
          xsi <- recEvalAt xs
          evalOp1 (TensorOp1Args ds xsi)
        _ -> goOp1 remainingOp1s
      [] -> Nothing

    goOp2 :: [TensorOpEvalData TensorOp2Args builtin m] -> Maybe (m (Value builtin))
    goOp2 = \case
      (accessOp2, evalOp2, _) : remainingOps2 -> case accessOp2 tensor of
        Just (TensorOp2Args _ xs ys) -> Just $ do
          xsi <- recEvalAt xs
          ysi <- recEvalAt ys
          evalOp2 $ TensorOp2Args ds xsi ysi
        _ -> goOp2 remainingOps2
      _ -> Nothing

    goForeach :: Maybe (m (Value builtin))
    goForeach = case getExpr accessForeachTensor tensor of
      Just (ForeachTensorArgs _ _ _ fn) -> Just $ do
        evalApp ctx fn [explicit index]
      _ -> Nothing

unoptimisedEvalAtTensor ::
  forall builtin m.
  (MonadNormBuiltin m, HasTensorLiterals builtin, BuiltinHasListLiterals builtin, BuiltinHasIndexLiterals builtin, HasTensorExpr Value builtin) =>
  EvalSimple AtTensorArgs Value builtin m
unoptimisedEvalAtTensor args@(AtTensorArgs _t _d ds tensor index) = do
  fromMaybe (return $ mkExpr accessAtTensor args) $
    case index of
      IIndexLiteral i ->
        goLiterals i tensorLiterals
          <|> case tensor of
            (getExpr accessStackTensor -> Just stackArgs) -> Just $ return $ stackElements stackArgs !! i
            (getExpr accessConstTensor -> Just constArgs) -> Just $ return $ mkExpr accessConstTensor $ constArgs {constDims = argExpr ds}
            _ -> Nothing
      _ -> Nothing
  where
    goLiterals :: Int -> [TensorLiteralAccessor builtin] -> Maybe (m (Value builtin))
    goLiterals i literals = case literals of
      Wrapper Access {..} : remainingLiterals -> case getExpr tensor of
        Just xs -> Just $ return $ mkExpr (xs `at` i)
        Nothing -> goLiterals i remainingLiterals
      _ -> Nothing

-----------------------------------------------------------------------------
-- Foreach

type HasOptimisedAtBuiltins builtin =
  ( HasTensorLiterals builtin,
    HasLiftableTensorOperations builtin,
    NormalisableBuiltin builtin,
    BuiltinHasListLiterals builtin,
    BuiltinHasNatType builtin,
    BuiltinHasNatLiterals builtin,
    BuiltinHasIndexLiterals builtin,
    BuiltinHasTensors builtin,
    BuiltinHasForeach builtin
  )

-- | An optimised evaluation procedure for `Foreach` that attempts to minimise the
-- amount of work needed by lifting operations to higher-tensor levels.
-- For example `foreach i . xs ! i + ys ! i` becomes `xs + ys`.
evalForeachTensor ::
  forall builtin m.
  (MonadNormBuiltin m, HasOptimisedAtBuiltins builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  ForeachTensorArgs (Value builtin) ->
  m (Value builtin)
evalForeachTensor ctx _evalApp eval (ForeachTensorArgs typ d ds fn) = case fn of
  VLam binder (Closure env body) -> do
    let lv = boundCtxLv ctx
    let newEnv = extendEnvWithBound lv binder env
    let newCtx = nameOf binder : ctx
    body' <- eval newCtx newEnv body
    let createForeach t newBody = do
          let newBody' = quote mempty (lv + 1) newBody
          let newLam = VLam binder (Closure (namedBoundContextToEnv ctx) newBody')
          let args = ForeachTensorArgs (implicit t) d ds newLam
          -- We simply recreate the foreach so that the call site
          -- can do tensor fusion.
          let result = mkExpr accessForeachTensor args
          return result
    result <- liftForeach newCtx createForeach lv d (argExpr typ) body'
    return result
  e -> unexpectedExprError "NBE" ("foreachIndex" <+> prettyVerbose e)

liftForeach ::
  forall builtin m.
  (MonadNormBuiltin m, HasOptimisedAtBuiltins builtin) =>
  NamedBoundCtx ->
  (VType builtin -> Value builtin -> m (Value builtin)) ->
  Lv ->
  Value builtin ->
  VType builtin ->
  Value builtin ->
  m (Value builtin)
liftForeach ctx evalForeach lv d = go
  where
    go :: VType builtin -> Value builtin -> m (Value builtin)
    go typ body = do
      showFusionEntry ctx body
      result <-
        fromMaybe (evalForeach typ body) $
          goOp1 body liftableTensorOp1s
            <|> goOp2 body liftableTensorOp2s
            <|> goAt body
            <|> goConst body
            <|> goLiterals body tensorLiterals
      showFusionExit ctx result

    -- Distribute the `forallIndex` across a liftable operation (e.g. `not`).
    -- e.g. `foreach i . op (x(i))` -> `op (foreach i . x(i))`
    goOp1 :: Value builtin -> [TensorOpEvalData TensorOp1Args builtin m] -> Maybe (m (Value builtin))
    goOp1 body = \case
      (accessOp1, evalOp1, typ) : remainingOp1s -> case accessOp1 body of
        Just (TensorOp1Args ds e) -> Just $ do
          e' <- go typ e
          evalOp1 (TensorOp1Args (extendArgDims ds) e')
        _ -> goOp1 body remainingOp1s
      [] -> Nothing

    -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
    -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
    goOp2 :: Value builtin -> [TensorOpEvalData TensorOp2Args builtin m] -> Maybe (m (Value builtin))
    goOp2 body = \case
      (accessOp, evalOp, typ) : remainingOps -> case accessOp body of
        Just (TensorOp2Args ds e1 e2) -> Just $ do
          e1' <- go typ e1
          e2' <- go typ e2
          let newSpine = TensorOp2Args (extendArgDims ds) e1' e2'
          evalOp newSpine
        _ -> goOp2 body remainingOps
      [] -> Nothing

    -- Eliminate `forall i . xs ! i` into `xs`
    goAt :: Value builtin -> Maybe (m (Value builtin))
    goAt value = case getExpr accessAtTensor value of
      Just (AtTensorArgs _ _ _ xs (VBoundVar lv1 [])) | lv1 == lv -> Just $ return xs
      _ -> Nothing

    goLiterals :: Value builtin -> [TensorLiteralAccessor builtin] -> Maybe (m (Value builtin))
    goLiterals value literals = case literals of
      Wrapper Access {..} : remainingLiterals -> case (getExpr value, d) of
        (Just xs, INatLiteral dim) -> Just $ return $ mkExpr $ extendTensor dim xs
        _ -> goLiterals value remainingLiterals
      _ -> Nothing

    goConst :: Value builtin -> Maybe (m (Value builtin))
    goConst value = case getExpr accessConstTensor value of
      Just (ConstTensorArgs t x ds) ->
        Just $
          evalConstTensor $
            ConstTensorArgs t x (extendDims ds)
      _ -> Nothing

    extendDims :: Value builtin -> Value builtin
    extendDims ds = mkExpr accessCons (implicit INatType, d, ds)

    extendArgDims :: VArg builtin -> VArg builtin
    extendArgDims = implicit . extendDims . argExpr

unoptimisedEvalForeachTensor ::
  (MonadLogger m, HasTensorLiterals builtin, HasTensorExpr Value builtin, BuiltinHasNatLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  ForeachTensorArgs (Value builtin) ->
  m (Value builtin)
unoptimisedEvalForeachTensor ctx evalApp args@(ForeachTensorArgs t d ds f) = case d of
  INatLiteral n -> do
    xs <- traverse (\i -> evalApp ctx f [explicit (IIndexLiteral i)]) [0 .. (n - 1 :: Int)]
    evalStackTensor (StackTensorArgs t d ds xs)
  _ -> return $ mkExpr accessForeachTensor args

-----------------------------------------------------------------------------
-- Stack

evalStackTensor ::
  (MonadNormBuiltin m, HasTensorLiterals builtin, BuiltinHasNatLiterals builtin, HasTensorExpr Value builtin) =>
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

-----------------------------------------------------------------------------
-- Const

evalConstTensor ::
  forall builtin m.
  (MonadNormBuiltin m, HasTensorLiterals builtin, BuiltinHasNatLiterals builtin, HasTensorExpr Value builtin) =>
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

evalForeachVector ::
  (MonadLogger m, HasTensorLiterals builtin, HasVectorExpr Value builtin, BuiltinHasNatLiterals builtin, BuiltinHasIndexLiterals builtin, BuiltinHasForeach builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  ForeachVectorArgs (Value builtin) ->
  m (Value builtin)
evalForeachVector ctx evalApp _eval args@(ForeachVectorArgs t d f) = case d of
  INatLiteral n -> do
    xs <- traverse (\i -> evalApp ctx f [explicit (IIndexLiteral i)]) [0 .. (n - 1 :: Int)]
    return $ IVecLiteral t d xs
  _ -> return $ mkExpr accessForeachVector args

evalIterate ::
  (MonadLogger m, BuiltinHasNatLiterals builtin, BuiltinHasIterate builtin) =>
  NamedBoundCtx ->
  EvalApp builtin m ->
  Eval builtin m ->
  IterateArgs (Value builtin) ->
  m (Value builtin)
evalIterate ctx evalApp _eval args@(IterateArgs t f n e) = case n of
  INatLiteral 0 -> return e
  INatLiteral v -> do
    let recFn = VBuiltin (mkExpr accessIterateBuiltin ()) [t, explicit f, explicit (INatLiteral (v - 1))]
    evalApp ctx f [explicit recFn, explicit e]
  _ -> return $ mkExpr accessIterate args

-----------------------------------------------------------------------------
-- Logging

showFusionEntry :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Value builtin -> m ()
showFusionEntry _ctx _expr = return ()

showFusionExit :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Value builtin -> m (Value builtin)
showFusionExit _ctx result = return result

{-
showFusionEntry :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Value builtin -> m ()
showFusionEntry ctx expr = do
  logDebug MidDetail $ "fusion-entry" <+> prettyFriendly (WithContext expr ctx)
  -- logDebug MidDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (boundEnvToCtx boundEnv)) <+> "   { boundEnv =" <+> prettyFriendly boundEnv <+> "}"
  -- logDebug MidDetail $ "nbe-entry" <+> prettyVerbose expr -- <+> "   { boundEnv=" <+> prettyVerbose boundEnv <+> "}"
  incrCallDepth
  return ()

showFusionExit :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Value builtin -> m (Value builtin)
showFusionExit ctx result = do
  decrCallDepth
  -- logDebug MidDetail $ "nbe-exit" <+> prettyVerbose result
  logDebug MidDetail $ "fusion-exit" <+> prettyFriendly (WithContext result ctx)
  return result
-}

fusionEnter :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Value builtin -> m ()
fusionEnter _ctx _value = return ()

fusionExit :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Maybe (VArg builtin, Value builtin) -> m (Maybe (VArg builtin, Value builtin))
fusionExit _ctx result = return result

{-
fusionEnter :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Value builtin -> m ()
fusionEnter ctx value = do
  logDebug MaxDetail $ "fusion-enter" <+> prettyFriendly (WithContext value ctx)
  incrCallDepth

fusionExit :: (MonadLogger m, PrintableBuiltin builtin) => NamedBoundCtx -> Maybe (VArg builtin, Value builtin) -> m (Maybe (VArg builtin, Value builtin))
fusionExit ctx result = do
  decrCallDepth
  logDebug MaxDetail $
    "fusion-exit" <+> case result of
      Nothing -> ""
      Just (dims, value) -> prettyFriendly (WithContext value ctx) <+> parens (prettyFriendly (WithContext (argExpr dims) ctx))
  return result-}
