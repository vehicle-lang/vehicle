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
import Vehicle.Data.Builtin.Core (BoolTensorBuiltin (..), Builtin, BuiltinFunction (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Tensor (TensorBuiltin (..))
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (Value (..), WHNFClosure (..), WHNFValue, boundContextToEnv, pattern VBuiltinFunction)
import Vehicle.Data.DSL
import Vehicle.Syntax.Builtin
  ( AddDomain (..),
    DivDomain (..),
    EqualityDomain (..),
    MulDomain (..),
    NegDomain (..),
    OrderDomain (..),
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
  Map TensorDifferentiableLogicField (WHNFValue LossTensorBuiltin) ->
  TensorDifferentiableLogicField ->
  m (Map TensorDifferentiableLogicField (WHNFValue LossTensorBuiltin))
compileLogicField logicID dsl impl field =
  logCompilerSection MaxDetail ("compiling tensor-field" <+> quotePretty field) $ do
    let tensorExprFn = case field of
          TruthityElement -> compileBool Truthity
          FalsityElement -> compileBool Falsity
          PointwiseConjunction -> liftOp2 Conjunction
          PointwiseDisjunction -> liftOp2 Disjunction
          PointwiseNegation -> liftOp1 Negation
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

compileBool ::
  (MonadCompileField m) =>
  BooleanDifferentiableLogicField ->
  DifferentialLogicDSL ->
  m (Expr TensorBuiltin)
compileBool field dsl = do
  let expr = lookupLogicField field dsl
  value <- eval mempty mempty expr
  case value :: WHNFValue Builtin of
    IRatLiteral _ l -> return $ IRatTensorOp (RatLiteral l) []
    _ -> developerError $ "Expecting arity 1 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx value

liftOp1 ::
  (MonadCompileField m) =>
  BooleanDifferentiableLogicField ->
  DifferentialLogicDSL ->
  m (Expr TensorBuiltin)
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
  m (Expr TensorBuiltin)
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
  m (Expr TensorBuiltin)
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
  (WHNFValue Builtin -> NameContextT (ExceptT (WHNFValue Builtin) m) a) ->
  m a
extractOp1Body dsl field process = do
  op1 <- eval mempty mempty (lookupLogicField field dsl)
  case op1 of
    VLam binder (WHNFClosure [] body) -> runBodyExtraction (field, op1) process [void binder] body
    fn -> developerError $ "Expecting arity 1 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx fn

extractOp2Body ::
  (MonadCompileField m) =>
  DifferentialLogicDSL ->
  BooleanDifferentiableLogicField ->
  (WHNFValue Builtin -> NameContextT (ExceptT (WHNFValue Builtin) m) a) ->
  m a
extractOp2Body dsl field process = do
  op2 <- eval mempty mempty (lookupLogicField field dsl)
  case op2 of
    VLam2 binder1 [] binder2 body -> runBodyExtraction (field, op2) process [void binder2, void binder1] body
    fn -> developerError $ "Expecting arity 2 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx fn

runBodyExtraction ::
  (MonadCompileField m) =>
  (BooleanDifferentiableLogicField, WHNFValue Builtin) ->
  (WHNFValue Builtin -> NameContextT (ExceptT (WHNFValue Builtin) m) a) ->
  BoundCtx () ->
  Expr Builtin ->
  m a
runBodyExtraction originalFn process ctx body = do
  bodyValue <- eval mempty (boundContextToEnv ctx) body
  resultOrError <- runExceptT $ runNameContextT ctx $ process bodyValue
  case resultOrError of
    Right result -> return result
    Left blockedExpr -> do
      let nameCtx = toNamedBoundCtx ctx
      (logicID, tensorField) <- ask
      throwError $ UnableToLiftLogicFieldToTensors logicID tensorField originalFn nameCtx blockedExpr

--------------------------------------------------------------------------------
-- Compilation of logic field bodies
--------------------------------------------------------------------------------

liftOp :: BuiltinFunction -> Maybe TensorBuiltin
liftOp = \case
  Not -> Just $ TensorBool NotBoolTensor
  And -> Just $ TensorBool AndBoolTensor
  Or -> Just $ TensorBool OrBoolTensor
  Neg NegRat -> Just $ TensorRat NegRatTensor
  Add AddRat -> Just $ TensorRat AddRatTensor
  Sub SubRat -> Just $ TensorRat SubRatTensor
  Mul MulRat -> Just $ TensorRat MulRatTensor
  Div DivRat -> Just $ TensorRat DivRatTensor
  MinRat -> Just $ TensorRat MinRatTensor
  MaxRat -> Just $ TensorRat MaxRatTensor
  Equals EqRat op -> Just $ TensorBool $ EqualsRatTensor op
  Order OrderRat op -> Just $ TensorBool $ OrderRatTensor op
  Implies -> Nothing
  Quantifier {} -> Nothing
  If -> Nothing
  FromNat {} -> Nothing
  FromRat {} -> Nothing
  Add _ -> Nothing
  Mul _ -> Nothing
  PowRat -> Nothing
  Equals _ _ -> Nothing
  Order _ _ -> Nothing
  At -> Nothing
  FoldList -> Nothing
  FoldVector -> Nothing
  MapList -> Nothing
  MapVector -> Nothing
  ZipWithVector -> Nothing
  Indices -> Nothing

reduceOp :: BuiltinFunction -> Maybe TensorBuiltin
reduceOp = \case
  Not -> Nothing
  And -> Just $ TensorBool ReduceAndTensor
  Or -> Just $ TensorBool ReduceOrTensor
  Add AddRat -> Just $ TensorRat ReduceAddRatTensor
  Mul MulRat -> Just $ TensorRat ReduceMulRatTensor
  MinRat -> Just $ TensorRat ReduceMinRatTensor
  MaxRat -> Just $ TensorRat ReduceMaxRatTensor
  Equals {} -> Nothing
  Order {} -> Nothing
  Neg NegRat -> Nothing
  Sub SubRat -> Nothing
  Div DivRat -> Nothing
  Implies -> Nothing
  Quantifier {} -> Nothing
  If -> Nothing
  FromNat {} -> Nothing
  FromRat {} -> Nothing
  Add _ -> Nothing
  Mul _ -> Nothing
  PowRat -> Nothing
  At -> Nothing
  FoldList -> Nothing
  FoldVector -> Nothing
  MapList -> Nothing
  MapVector -> Nothing
  ZipWithVector -> Nothing
  Indices -> Nothing

type MonadCompileBody m =
  ( MonadLogger m,
    MonadError (WHNFValue Builtin) m,
    MonadNameContext m
  )

liftOp1Body ::
  (MonadCompileBody m) =>
  WHNFValue Builtin ->
  m (DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin)
liftOp1Body = convertHigherOrderFunction "liftOp1" $ \case
  VBuiltinFunction (liftOp -> Just liftedOp) [argExpr -> e] -> do
    e' <- liftOp1Body e
    return $ \dims xs -> builtin liftedOp .@@@ [dims] @@ [e' dims xs]
  VBuiltinFunction (liftOp -> Just liftedOp) [argExpr -> e1, argExpr -> e2] -> do
    e1' <- liftOp1Body e1
    e2' <- liftOp1Body e2
    return $ \dims xs -> builtin liftedOp .@@@ [dims] @@ [e1' dims xs, e2' dims xs]
  VBoundVar v [] | v == 0 ->
    return $ \_dim xs -> xs
  IRatLiteral _ r ->
    return $ \dims _xs -> constTensor tRatElemType (ratTensorBuiltin (RatLiteral r)) dims
  blockedExpr ->
    throwError blockedExpr

liftOp2Body ::
  (MonadCompileBody m) =>
  WHNFValue Builtin ->
  m (DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin)
liftOp2Body = convertHigherOrderFunction "liftOp2" $ \case
  VBuiltinFunction (liftOp -> Just liftedOp) [argExpr -> e] -> do
    e' <- liftOp2Body e
    return $ \dims xs ys -> builtin liftedOp .@@@ [dims] @@ [e' dims xs ys]
  VBuiltinFunction (liftOp -> Just liftedOp) [argExpr -> e1, argExpr -> e2] -> do
    e1' <- liftOp2Body e1
    e2' <- liftOp2Body e2
    return $ \dims xs ys -> builtin liftedOp .@@@ [dims] @@ [e1' dims xs ys, e2' dims xs ys]
  VBoundVar lv []
    | lv == 0 -> return $ \_dims xs _ys -> xs
    | lv == 1 -> return $ \_dims _xs ys -> ys
  IRatLiteral _ r ->
    return $ \dims _xs _ys -> constTensor tRatElemType (ratTensorBuiltin (RatLiteral r)) dims
  blockedExpr ->
    throwError blockedExpr

reduceOp2Body ::
  (MonadCompileBody m) =>
  WHNFValue Builtin ->
  m (DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin -> DSLExpr TensorBuiltin)
reduceOp2Body = convertHigherOrderFunction "reduction" $ \case
  VBuiltinFunction (reduceOp -> Just reducedOp) [argExpr -> VBoundVar 0 [], argExpr -> VBoundVar 1 []] ->
    return $ \dims xs -> builtin reducedOp .@@@ [dims] @@ [xs]
  blockedExpr -> throwError blockedExpr

convertHigherOrderFunction ::
  (MonadLogger m, MonadNameContext m) =>
  Doc a ->
  (WHNFValue Builtin -> m a) ->
  WHNFValue Builtin ->
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
