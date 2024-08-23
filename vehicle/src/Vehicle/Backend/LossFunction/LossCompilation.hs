module Vehicle.Backend.LossFunction.LossCompilation
  ( convertValue,
    convertExpr,
    MonadLossCtx,
    runMonadLogicT,
  )
where

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
import Vehicle.Data.Builtin.Core (BoolTensorBuiltin (..), Quantifier (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Tensor (EqualityOp (..), OrderOp (..), TensorBuiltin (..))
import Vehicle.Data.Builtin.Tensor qualified as T
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (Value (..), WHNFArg, WHNFBinder, WHNFBoundEnv, WHNFClosure (..), WHNFSpine, WHNFValue, boundContextToEnv, extendEnvWithBound, traverseSpine)
import Vehicle.Data.Tensor (Tensor (..), foldMapTensor)

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

getLogicField :: (MonadLogic m) => TensorDifferentiableLogicField -> m (WHNFValue LossTensorBuiltin)
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
  WHNFBoundEnv TensorBuiltin ->
  Expr TensorBuiltin ->
  m (WHNFValue LossTensorBuiltin)
convertExpr env expr = convertValue =<< eval mempty env expr

convertValue ::
  forall m.
  (MonadLogic m) =>
  WHNFValue TensorBuiltin ->
  m (WHNFValue LossTensorBuiltin)
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
    VPi binder body -> do
      binder' <- traverse convertValue binder
      body' <- addNameToContext binder $ convertValue body
      return $ VPi binder' body'
    VLam binder closure -> do
      binder' <- traverse convertValue binder
      closure' <- traverseClosure convertValue mempty binder closure
      return $ VLam binder' closure'
  showExit result
  return result

convertBuiltinToLoss ::
  forall m.
  (MonadLogic m) =>
  TensorBuiltin ->
  WHNFSpine TensorBuiltin ->
  m (WHNFValue LossTensorBuiltin)
convertBuiltinToLoss b spine = case b of
  T.TensorRat r -> unchangedBuiltin (LossTensorRat r)
  T.TensorDimData s -> unchangedBuiltin (LossTensorDimData s)
  T.TensorDimType s -> unchangedBuiltin (LossTensorDimType s)
  T.TensorBool op -> case op of
    --------------
    -- Booleans --
    --------------
    T.BoolType -> return $ VBuiltin (LossTensorRat RatType) []
    T.BoolTensor t -> translateConstant t
    T.BoolLiteral True -> changedBuiltin L.TruthityElement
    T.BoolLiteral False -> changedBuiltin L.FalsityElement
    T.NotBoolTensor -> changedBuiltin L.PointwiseNegation
    T.AndBoolTensor -> changedBuiltin L.PointwiseConjunction
    T.OrBoolTensor -> changedBuiltin L.PointwiseDisjunction
    T.EqualsRatTensor Eq -> changedBuiltin L.PointwiseEq
    T.EqualsRatTensor Neq -> changedBuiltin L.PointwiseNe
    T.OrderRatTensor Lt -> changedBuiltin L.PointwiseLt
    T.OrderRatTensor Le -> changedBuiltin L.PointwiseLe
    T.OrderRatTensor Gt -> changedBuiltin L.PointwiseGt
    T.OrderRatTensor Ge -> changedBuiltin L.PointwiseGe
    T.QuantifyRatTensor q -> translateQuantifier q spine
    T.ReduceAndTensor -> changedBuiltin L.ReduceConjunction
    T.ReduceOrTensor -> changedBuiltin L.ReduceDisjunction
  where
    changedBuiltin :: TensorDifferentiableLogicField -> m (WHNFValue LossTensorBuiltin)
    changedBuiltin field = substField field =<< traverseSpine convertValue spine

    unchangedBuiltin :: LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
    unchangedBuiltin op = VBuiltin op <$> traverseSpine convertValue spine

substField :: (MonadLogic m) => TensorDifferentiableLogicField -> WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
substField field spine = do
  fn <- getLogicField field
  logDebug MaxDetail $ "subst-field" <+> pretty field <> ":" <+> prettyFriendlyEmptyCtx fn
  evalApp mempty fn spine

translateConstant :: (MonadLogic m) => Tensor Bool -> m (WHNFValue LossTensorBuiltin)
translateConstant tensor = do
  trueExpr <- getLogicField L.TruthityElement
  falseExpr <- getLogicField L.FalsityElement
  let convertBool b = if b then trueExpr else falseExpr
  let foldLayer shape elems = IDimensionDataOp (StackTensor (head shape)) (explicit <$> elems)
  return $ foldMapTensor convertBool foldLayer tensor

translateQuantifier :: (MonadLogic m) => Quantifier -> WHNFSpine TensorBuiltin -> m (WHNFValue LossTensorBuiltin)
translateQuantifier q = \case
  [dims, argExpr -> VLam binder (WHNFClosure env body)] -> do
    -- Normalise the body
    lv <- getBinderDepth
    let newEnv = extendEnvWithBound lv binder env
    bodyValue <- eval mempty newEnv body

    -- Translate the dimensions
    lossDims <- traverse convertValue dims

    case q of
      Forall -> translateForall dims lossDims binder bodyValue
      Exists -> translateExists lossDims binder bodyValue
  args -> unexpectedExprError currentPass (prettyVerbose $ IBoolTensorOp (QuantifyRatTensor q) args)

translateForall ::
  (MonadLogic m) =>
  WHNFArg TensorBuiltin ->
  WHNFArg LossTensorBuiltin ->
  WHNFBinder TensorBuiltin ->
  WHNFValue TensorBuiltin ->
  m (WHNFValue LossTensorBuiltin)
translateForall dims lossDims binder body = do
  let newBody = IBoolTensorOp NotBoolTensor [dims, explicit body]
  result <- translateExists lossDims binder newBody
  substField L.PointwiseNegation [lossDims, explicit result]

translateExists ::
  (MonadLogic m) =>
  WHNFArg LossTensorBuiltin ->
  WHNFBinder TensorBuiltin ->
  WHNFValue TensorBuiltin ->
  m (WHNFValue LossTensorBuiltin)
translateExists lossDims binder bodyValue = logCompilerSection MaxDetail "convert-exists" $ do
  boundCtx <- getBinderContext
  let lv = boundCtxLv boundCtx

  -- Convert the binder and the dimensions.
  lossBinder <- traverse convertValue binder

  -- Generate the operation for doing the reduction
  genericReductionOp <- getLogicField ReduceDisjunction
  reductionOp <- evalApp mempty genericReductionOp [implicit (dimSingleton 1)]

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
  let lossPredicate = VLam lossBinder (WHNFClosure finalEnv lossBody)

  let newArgs = lossDims : (explicit <$> [reductionOp, lossLowerBounds, lossUpperBounds, lossPredicate])

  return $ IRatTensorOp SearchRatTensor newArgs

--------------------------------------------------------------------------------
-- Utils

currentPass :: CompilerPass
currentPass = "logic translation"

showEntry :: (MonadLogger m, MonadNameContext m) => WHNFValue TensorBuiltin -> m ()
showEntry e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ doc <+> ":" <+> prettyVerbose e
  logDebug MaxDetail $ "enter-loss" <+> ":" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLogger m, MonadNameContext m) => WHNFValue LossTensorBuiltin -> m ()
showExit e = do
  ctx <- getNameContext
  decrCallDepth
  logDebug MaxDetail $ "exit-loss" <+> ": " <+> prettyFriendly (WithContext e ctx)
