{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.LogicCompilation
  ( findAndCompileLogic,
  )
where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState, StateT, execStateT, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.Core hiding (lookupLogicField)
import Vehicle.Backend.Loss.LossCompilation (convertFunction, convertRatTensor)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Data.Builtin.Core (Builtin)
import Vehicle.Data.Builtin.Interface.Normalise (evalCompareRatTensorPointwise)
import Vehicle.Data.Builtin.Loss (ComparisonOp (..), LogicDirection, LossBuiltin)
import Vehicle.Data.Builtin.Standard ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Free.Context

--------------------------------------------------------------------------------
-- Interface

findAndCompileLogic ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Builtin ->
  m DifferentiableLogicImplementation
findAndCompileLogic logicID prog = do
  MonadLossState {..} <-
    runMonadLossT $ traverseNormalisedDecls_ (convertLogicDecl logicID) prog
  case maybeImplementation of
    Just definition -> return definition
    Nothing -> do
      let names = fmap nameOf foundLogics
      missingLogicError names logicID

--------------------------------------------------------------------------------
-- Monad

data MonadLossState = MonadLossState
  { maybeImplementation :: Maybe DifferentiableLogicImplementation,
    foundLogics :: [Identifier]
  }

type MonadLoss m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadState MonadLossState m
  )

runMonadLossT ::
  (MonadCompile m) =>
  FreeContextT Builtin (StateT MonadLossState m) a ->
  m MonadLossState
runMonadLossT action = do
  let freshState = MonadLossState Nothing mempty
  flip execStateT freshState $
    runFreshFreeContextT
      (Proxy @Builtin)
      action

registerUnmatchedLogic ::
  (MonadLoss m) =>
  Identifier ->
  m ()
registerUnmatchedLogic ident = modify $
  \MonadLossState {..} -> do
    MonadLossState
      { foundLogics = ident : foundLogics,
        ..
      }

registerMatchedLogic ::
  (MonadLoss m) =>
  DifferentiableLogicImplementation ->
  m ()
registerMatchedLogic implementation = modify $
  \MonadLossState {..} -> do
    MonadLossState
      { maybeImplementation = Just implementation,
        ..
      }

--------------------------------------------------------------------------------
-- Monad

convertLogicDecl ::
  (MonadLoss m) =>
  DifferentiableLogicID ->
  VDecl Builtin ->
  m ()
convertLogicDecl logicID decl =
  case decl of
    DefFunction p ident _ann _typ body
      | isLogicDecl decl -> do
          if nameOf logicID /= nameOf ident
            then registerUnmatchedLogic ident
            else case body of
              VRecord _ fields -> do
                logic <- compileLogic logicID decl fields
                registerMatchedLogic logic
              _ -> throwError $ UnreducableDifferentiableLogic (ident, p)
    _ -> return ()

-- | Compiles a differentiable logic from the DSL over booleans to normalised
-- values over tensors that are suitable for substitution.
-- Eventually the DSL should be replaced by the something in the language.
compileLogic ::
  forall m.
  (MonadLoss m) =>
  DifferentiableLogicID ->
  VDecl Builtin ->
  OMap FieldName (Value Builtin) ->
  m DifferentiableLogicImplementation
compileLogic logicID decl fields = do
  logCompilerSection2 MinDetail ("compiling logic" <+> quotePretty logicID) $ do
    -- Lift fields to the tensor level
    let tensorLogicFields = [minBound .. maxBound] :: [TensorDifferentiableLogicField]
    lossTensorImplementation <- foldM (compileLogicField logicID decl fields) mempty tensorLogicFields
    minimise <- calculateLogicDirection decl fields
    -- Convert fields to loss tensors
    return (lossTensorImplementation, minimise)

calculateLogicDirection ::
  (MonadLoss m) =>
  VDecl Builtin ->
  OMap FieldName (Value Builtin) ->
  m LogicDirection
calculateLogicDirection decl fields = do
  let trueValue = lookupLogicField TruthityElement fields
  let falseValue = lookupLogicField FalsityElement fields
  result <- evalCompareRatTensorPointwise Le $ TensorOp2Args IDimNil trueValue falseValue
  case result of
    IBoolLiteral b -> return b
    _ -> do
      let prov = (identifierOf decl, provenanceOf decl)
      throwError $ UnorderableDifferentiableLogic prov result

compileLogicField ::
  (MonadLoss m) =>
  DifferentiableLogicID ->
  VDecl Builtin ->
  OMap FieldName (Value Builtin) ->
  Map TensorDifferentiableLogicField (Value LossBuiltin) ->
  TensorDifferentiableLogicField ->
  m (Map TensorDifferentiableLogicField (Value LossBuiltin))
compileLogicField logicID decl fields impl field =
  logCompilerSection2 MidDetail ("compiling tensor-field" <+> quotePretty field) $ do
    let tensorValue = lookupLogicField field fields
    logDebug MaxDetail $ "tensor-result:" <+> prettyFriendlyEmptyCtx tensorValue <> line
    logDebug MaxDetail $ "tensor-result:" <+> prettyVerbose tensorValue <> line

    lossTensorExpr <-
      runMonadLogicT logicID (mempty, True) decl $ do
        convertFunction convertRatTensor tensorValue
    logDebug MaxDetail $ "loss-tensor-result:" <+> prettyFriendlyEmptyCtx lossTensorExpr
    return $ Map.insert field lossTensorExpr impl

lookupLogicField :: TensorDifferentiableLogicField -> OMap FieldName value -> value
lookupLogicField field logicFields = do
  case OMap.lookup (FieldName mempty (nameOf field)) logicFields of
    Nothing -> developerError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
    Just value -> value

{-
fieldIdentifier :: DifferentiableLogicID -> TensorDifferentiableLogicField -> Identifier
fieldIdentifier logicID field = do
  let fieldName = layoutAsText $ pretty field
  let recordModule = RecordModule $ layoutAsText $ pretty logicID
  Identifier (ModulePath [StdLib, recordModule]) fieldName

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
  value <- eval mempty mempty emptyBoundEnv expr
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
        explLam "e" (tRatTensor dimNil) $ \e ->
          explLam "xs" (tRatTensor dims) $ \xs ->
            reducedOp dims e xs

extractOp1Body ::
  (MonadCompileField m) =>
  DifferentialLogicDSL ->
  BooleanDifferentiableLogicField ->
  (Value Builtin -> NameBoundContextT (ExceptT (Value Builtin) m) a) ->
  m a
extractOp1Body dsl field process = do
  op1 <- eval mempty mempty emptyBoundEnv (lookupLogicField field dsl)
  case op1 of
    VLam binder (Closure _env body) -> runBodyExtraction (field, op1) process [void binder] body
    fn -> developerError $ "Expecting arity 1 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx fn

extractOp2Body ::
  (MonadCompileField m) =>
  DifferentialLogicDSL ->
  BooleanDifferentiableLogicField ->
  (Value Builtin -> NameBoundContextT (ExceptT (Value Builtin) m) a) ->
  m a
extractOp2Body dsl field process = do
  op2 <- eval mempty mempty emptyBoundEnv (lookupLogicField field dsl)
  case op2 of
    VLam2 binder1 _env binder2 body -> runBodyExtraction (field, op2) process [void binder2, void binder1] body
    fn -> developerError $ "Expecting arity 2 function for" <+> pretty field <> "but found" <+> prettyFriendlyEmptyCtx fn

pattern VLam2 :: VBinder builtin -> BoundEnv builtin -> Binder builtin -> Expr builtin -> Value builtin
pattern VLam2 binder1 env binder2 body <- VLam binder1 (Closure env (Lam _ binder2 body))

runBodyExtraction ::
  (MonadCompileField m) =>
  (BooleanDifferentiableLogicField, Value Builtin) ->
  (Value Builtin -> NameBoundContextT (ExceptT (Value Builtin) m) a) ->
  BoundCtx () ->
  Expr Builtin ->
  m a
runBodyExtraction originalFn process ctx body = do
  bodyValue <- eval mempty (toNamedBoundCtx ctx) (boundContextToEnv ctx) body
  let nameCtx = toNamedBoundCtx ctx
  resultOrError <- runExceptT $ runNameBoundContextT nameCtx $ process bodyValue
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
  VBuiltin (BuiltinFunction op) (getExpr accessSpine -> Just (TensorOp1Args _ds e)) | isLiftableOp op -> do
    e' <- liftOp1Body e
    return $ \dims xs -> builtinFunction op .@@@ [dims] @@ [e' dims xs]
  VBuiltin (BuiltinFunction op) (getExpr accessSpine -> Just (TensorOp2Args _ds e1 e2)) | isLiftableOp op -> do
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
  VBuiltin (BuiltinFunction op) (getExpr accessSpine -> Just (TensorOp1Args _ds e)) | isLiftableOp op -> do
    e' <- liftOp2Body e
    return $ \dims xs ys -> builtinFunction op .@@@ [dims] @@ [e' dims xs ys]
  VBuiltin (BuiltinFunction op) (getExpr accessSpine -> Just (TensorOp2Args _ds e1 e2)) | isLiftableOp op -> do
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
  m (DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin)
reduceOp2Body = convertHigherOrderFunction "reduction" $ \case
  VBuiltin (BuiltinFunction (reduceOp -> Just reducedOp)) (getExpr accessSpine -> Just (TensorOp2Args _ (VBoundVar 0 []) (VBoundVar 1 []))) ->
    return $ \dims e xs -> builtinFunction reducedOp .@@@ [dims] @@ [e, xs]
  blockedExpr -> do
    logDebug MaxDetail $ prettyVerbose blockedExpr
    throwError blockedExpr

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
-}
