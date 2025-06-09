{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.LogicCompilation
  ( compileLogic,
  )
where

import Control.Monad (foldM, void)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Backend.LossFunction.Core
import Vehicle.Backend.LossFunction.Logics (DifferentialLogicDSL)
import Vehicle.Backend.LossFunction.LossCompilation
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx)
import Vehicle.Data.Builtin.Core (Builtin)
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (Closure (..), Value (..), boundContextToEnv)
import Vehicle.Data.DSL
import Vehicle.Data.Tensor (pattern ZeroDimTensor)
import Vehicle.Syntax.Builtin
  ( AddDomain (..),
    Builtin (..),
    BuiltinConstructor (..),
    BuiltinFunction (..),
    DivDomain (..),
    MaxDomain (..),
    MinDomain (..),
    MulDomain (..),
    NegDomain (..),
    SubDomain (..),
  )

--------------------------------------------------------------------------------
-- Monad

lookupLogicField :: (Ord field, Pretty field) => field -> Map field value -> value
lookupLogicField field logic = do
  case Map.lookup field logic of
    Nothing -> developerError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
    Just value -> value

--------------------------------------------------------------------------------
-- Logic compilation

-- | Compiles a differentiable logic from the DSL over booleans to normalised
-- values over tensors that are suitable for substitution.
-- Eventually the DSL should be replaced by the something in the language.
compileLogic ::
  forall m.
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DifferentialLogicDSL ->
  m CompiledDifferentiableLogic
compileLogic logicID dsl = do
  logCompilerPass MidDetail ("compiling logic" <+> quotePretty logicID) $ do
    -- Lift fields to the tensor level
    let tensorLogicFields = [minBound .. maxBound] :: [TensorDifferentiableLogicField]
    lossTensorImplementation <- foldM (compileLogicField logicID dsl) mempty tensorLogicFields
    -- Convert fields to loss tensors
    return (logicID, lossTensorImplementation)

compileLogicField ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DifferentialLogicDSL ->
  Map TensorDifferentiableLogicField (Value LossBuiltin) ->
  TensorDifferentiableLogicField ->
  m (Map TensorDifferentiableLogicField (Value LossBuiltin))
compileLogicField logicID dsl impl field =
  logCompilerSection MaxDetail ("compiling tensor-field" <+> quotePretty field) $ do
    let tensorExprFn = case field of
          TruthityElement -> compileBoolLiteral Truthity
          FalsityElement -> compileBoolLiteral Falsity
          PointwiseNegation -> liftOp1 Negation
          PointwiseConjunction -> liftOp2 Conjunction
          PointwiseDisjunction -> liftOp2 Disjunction
          PointwiseLe -> liftOp2 LessEqual
          PointwiseLt -> liftOp2 LessThan
          PointwiseGe -> liftOp2 GreaterEqual
          PointwiseGt -> liftOp2 GreaterThan
          PointwiseEq -> liftOp2 Equal
          PointwiseNe -> liftOp2 NotEqual
          ReduceConjunction -> reduceOp2 Conjunction
          ReduceDisjunction -> reduceOp2 Disjunction

    tensorExpr <- flip runReaderT (logicID, field) $ tensorExprFn dsl
    logDebug MaxDetail $ "tensor-result:" <+> prettyFriendlyEmptyCtx tensorExpr <> line

    let fieldProv = (fieldIdentifier logicID field, mempty)
    lossTensorExpr <- runFreshNameContextT $ runMonadLogicT (logicID, mempty) fieldProv $ convertExpr mempty tensorExpr
    logDebug MaxDetail $ "loss-tensor-result:" <+> prettyFriendlyEmptyCtx tensorExpr
    return $ Map.insert field lossTensorExpr impl

--------------------------------------------------------------------------------
-- Compilation of logic fields
--------------------------------------------------------------------------------

type MonadCompileField m =
  ( MonadCompile m,
    MonadReader (DifferentiableLogicID, TensorDifferentiableLogicField) m
  )

compileBoolLiteral ::
  (MonadCompileField m) =>
  BooleanDifferentiableLogicField ->
  DifferentialLogicDSL ->
  m (Expr Builtin)
compileBoolLiteral field dsl = do
  let expr = lookupLogicField field dsl
  value <- eval mempty mempty expr
  case value :: Value Builtin of
    IRatLiteral l -> return $ Builtin mempty (BuiltinConstructor (RatTensorLiteral (ZeroDimTensor l)))
    _ -> developerError "Boolean literals must currently be converted to Rat literals"

liftOp1 ::
  (MonadCompileField m) =>
  BooleanDifferentiableLogicField ->
  DifferentialLogicDSL ->
  m (Expr Builtin)
liftOp1 field dsl = do
  liftedOp1 <- extractOp1Body dsl field liftOp1Body
  return $
    fromDSL mempty $
      implLam "dims" tDims $ \dims ->
        explLam "xs" (tRatTensor dims) $ \xs ->
          liftedOp1 dims xs

liftOp2 ::
  (MonadCompileField m) =>
  BooleanDifferentiableLogicField ->
  DifferentialLogicDSL ->
  m (Expr Builtin)
liftOp2 field dsl = do
  liftedOp2 <- extractOp2Body dsl field liftOp2Body
  return $
    fromDSL mempty $
      implLam "dims" tDims $ \dims ->
        explLam "xs" (tRatTensor dims) $ \xs ->
          explLam "ys" (tRatTensor dims) $ \ys -> do
            liftedOp2 dims xs ys

reduceOp2 ::
  (MonadCompileField m) =>
  BooleanDifferentiableLogicField ->
  DifferentialLogicDSL ->
  m (Expr Builtin)
reduceOp2 field dsl = do
  reducedOp <- extractOp2Body dsl field reduceOp2Body
  return $
    fromDSL mempty $
      implLam "dims" tDims $ \dims ->
        explLam "xs" (tRatTensor dims) $ \xs ->
          reducedOp dims xs

extractOp1Body ::
  (MonadCompileField m) =>
  DifferentialLogicDSL ->
  BooleanDifferentiableLogicField ->
  (Value Builtin -> NameContextT (ExceptT (Value Builtin) m) a) ->
  m a
extractOp1Body dsl field process = do
  op1 <- eval mempty mempty (lookupLogicField field dsl)
  case op1 of
    VLam binder (Closure [] body) -> runBodyExtraction (field, op1) process [void binder] body
    fn -> developerError $ "Expecting arity 1 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx fn

extractOp2Body ::
  (MonadCompileField m) =>
  DifferentialLogicDSL ->
  BooleanDifferentiableLogicField ->
  (Value Builtin -> NameContextT (ExceptT (Value Builtin) m) a) ->
  m a
extractOp2Body dsl field process = do
  op2 <- eval mempty mempty (lookupLogicField field dsl)
  case op2 of
    VLam2 binder1 [] binder2 body -> runBodyExtraction (field, op2) process [void binder2, void binder1] body
    fn -> developerError $ "Expecting arity 2 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx fn

runBodyExtraction ::
  (MonadCompileField m) =>
  (BooleanDifferentiableLogicField, Value Builtin) ->
  (Value Builtin -> NameContextT (ExceptT (Value Builtin) m) a) ->
  BoundCtx () ->
  Expr Builtin ->
  m a
runBodyExtraction originalFn process ctx body = do
  bodyValue <- eval mempty (boundContextToEnv ctx) body
  let nameCtx = toNamedBoundCtx ctx
  resultOrError <- runExceptT $ runNameContextT nameCtx $ process bodyValue
  case resultOrError of
    Right result -> return result
    Left blockedExpr -> do
      (logicID, tensorField) <- ask
      throwError $ UnableToLiftLogicFieldToTensors logicID tensorField originalFn nameCtx blockedExpr

--------------------------------------------------------------------------------
-- Compilation of logic field bodies
--------------------------------------------------------------------------------

isLiftableOp :: BuiltinFunction -> Bool
isLiftableOp = \case
  Not -> True
  And -> True
  Or -> True
  Neg NegRatTensor -> True
  Add AddRatTensor -> True
  Sub SubRatTensor -> True
  Mul MulRatTensor -> True
  Div DivRatTensor -> True
  Min MinRatTensor -> True
  Max MaxRatTensor -> True
  CompareRatTensorPointwise _ -> True
  Implies -> False
  QuantifyRatTensor {} -> False
  If -> False
  Add {} -> False
  Mul {} -> False
  PowRat -> False
  CompareNat {} -> False
  CompareIndex {} -> False
  AtTensor -> False
  AtVector -> False
  FoldList -> False
  MapList -> False
  ReduceAndTensor -> False
  ReduceOrTensor -> False
  ReduceAddRatTensor -> False
  ReduceMulRatTensor -> False
  ReduceMinRatTensor -> False
  ReduceMaxRatTensor -> False
  StackTensor {} -> False
  ConstTensor -> False
  ForeachTensor -> False
  ForeachVector -> False
  Iterate -> False

reduceOp :: BuiltinFunction -> Maybe BuiltinFunction
reduceOp = \case
  And -> Just ReduceAndTensor
  Or -> Just ReduceOrTensor
  Add AddRatTensor -> Just ReduceAddRatTensor
  Mul MulRatTensor -> Just ReduceMulRatTensor
  Min MinRatTensor -> Just ReduceMinRatTensor
  Max MaxRatTensor -> Just ReduceMaxRatTensor
  Not -> Nothing
  CompareRatTensorPointwise {} -> Nothing
  CompareNat {} -> Nothing
  CompareIndex {} -> Nothing
  Neg NegRatTensor -> Nothing
  Sub SubRatTensor -> Nothing
  Div DivRatTensor -> Nothing
  Implies -> Nothing
  QuantifyRatTensor {} -> Nothing
  If -> Nothing
  Add _ -> Nothing
  Mul _ -> Nothing
  PowRat -> Nothing
  AtVector -> Nothing
  AtTensor -> Nothing
  FoldList -> Nothing
  MapList -> Nothing
  ReduceAndTensor -> Nothing
  ReduceOrTensor -> Nothing
  ReduceAddRatTensor -> Nothing
  ReduceMulRatTensor -> Nothing
  ReduceMinRatTensor -> Nothing
  ReduceMaxRatTensor -> Nothing
  StackTensor {} -> Nothing
  ConstTensor -> Nothing
  ForeachTensor -> Nothing
  ForeachVector -> Nothing
  Iterate -> Nothing

type MonadCompileBody m =
  ( MonadLogger m,
    MonadError (Value Builtin) m,
    MonadNameContext m
  )

liftOp1Body ::
  (MonadCompileBody m) =>
  Value Builtin ->
  m (DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin)
liftOp1Body = convertHigherOrderFunction "liftOp1" $ \case
  VBuiltin (BuiltinFunction op) [_ds, argExpr -> e] | isLiftableOp op -> do
    e' <- liftOp1Body e
    return $ \dims xs -> builtinFunction op .@@@ [dims] @@ [e' dims xs]
  VBuiltin (BuiltinFunction op) [_ds, argExpr -> e1, argExpr -> e2] | isLiftableOp op -> do
    e1' <- liftOp1Body e1
    e2' <- liftOp1Body e2
    return $ \dims xs -> builtinFunction op .@@@ [dims] @@ [e1' dims xs, e2' dims xs]
  VBoundVar v [] | v == 0 ->
    return $ \_dim xs -> xs
  IRatLiteral r ->
    return $ \dims _xs -> constTensor tRat (ratLit r) dims
  blockedExpr ->
    throwError blockedExpr

liftOp2Body ::
  (MonadCompileBody m) =>
  Value Builtin ->
  m (DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin)
liftOp2Body = convertHigherOrderFunction "liftOp2" $ \case
  VBuiltin (BuiltinFunction op) [_ds, argExpr -> e] | isLiftableOp op -> do
    e' <- liftOp2Body e
    return $ \dims xs ys -> builtinFunction op .@@@ [dims] @@ [e' dims xs ys]
  VBuiltin (BuiltinFunction op) [_ds, argExpr -> e1, argExpr -> e2] | isLiftableOp op -> do
    e1' <- liftOp2Body e1
    e2' <- liftOp2Body e2
    return $ \dims xs ys -> builtinFunction op .@@@ [dims] @@ [e1' dims xs ys, e2' dims xs ys]
  VBoundVar lv []
    | lv == 0 -> return $ \_dims xs _ys -> xs
    | lv == 1 -> return $ \_dims _xs ys -> ys
  IRatLiteral r ->
    return $ \dims _xs _ys -> constTensor tRat (ratLit r) dims
  blockedExpr ->
    throwError blockedExpr

reduceOp2Body ::
  (MonadCompileBody m) =>
  Value Builtin ->
  m (DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin)
reduceOp2Body = convertHigherOrderFunction "reduction" $ \case
  VBuiltin (BuiltinFunction (reduceOp -> Just reducedOp)) [argExpr -> VBoundVar 0 [], argExpr -> VBoundVar 1 []] ->
    return $ \dims xs -> builtinFunction reducedOp .@@@ [dims] @@ [xs]
  blockedExpr -> throwError blockedExpr

convertHigherOrderFunction ::
  (MonadLogger m, MonadNameContext m) =>
  Doc a ->
  (Value Builtin -> m a) ->
  Value Builtin ->
  m a
convertHigherOrderFunction field convert lamBody = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ doc <+> ":" <+> prettyVerbose e
  logDebug MaxDetail $ "enter-" <> field <> ":" <+> prettyFriendly (WithContext lamBody ctx)
  incrCallDepth
  result <- convert lamBody
  decrCallDepth
  return result

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

fieldIdentifier :: DifferentiableLogicID -> TensorDifferentiableLogicField -> Identifier
fieldIdentifier logicID field = do
  let fieldName = layoutAsText $ pretty field
  let recordModule = Record $ layoutAsText $ pretty logicID
  Identifier (ModulePath [StdLib, recordModule]) fieldName
