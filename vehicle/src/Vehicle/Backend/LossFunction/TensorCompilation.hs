module Vehicle.Backend.LossFunction.TensorCompilation
  ( convertExprToTensorValue,
    runMonadTensorT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Ratio
import Vehicle.Backend.LossFunction.Core
import Vehicle.Backend.LossFunction.LogicCompilation
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyFriendly, prettyFriendly)
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Loss qualified as L
import Vehicle.Data.Builtin.Tensor (TensorBuiltin)
import Vehicle.Data.Builtin.Tensor qualified as T
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdForeachIndex))
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Builtin

--------------------------------------------------------------------------------
-- Monad

type MonadTensorCtx =
  ( DifferentiableLogicID,
    DifferentiableLogicImplementation,
    DeclProvenance,
    FreeEnv (WHNFClosure Builtin) Builtin,
    MixedFreeEnv,
    GenericBoundCtx MixedLossBinder
  )

type MonadTensor m =
  ( MonadCompile m,
    MonadReader MonadTensorCtx m
  )

runMonadTensorT ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DeclProvenance ->
  DifferentiableLogicImplementation ->
  FreeEnv (WHNFClosure Builtin) Builtin ->
  MixedFreeEnv ->
  ReaderT MonadTensorCtx m a ->
  m a
runMonadTensorT logicID origin logic standardEnv lossEnv =
  flip runReaderT (logicID, logic, origin, standardEnv, lossEnv, mempty)

switchToMonadLogic ::
  (MonadTensor m) =>
  ReaderT MonadLogicCtx m a ->
  m a
switchToMonadLogic comp = do
  (logicID, logic, declProv, freeEnv, mixedEnv, boundCtx) <- ask
  runMonadLogicT logicID logic (Left declProv) freeEnv mixedEnv boundCtx comp

getDeclProvenance :: (MonadTensor m) => m DeclProvenance
getDeclProvenance = do
  (_, _, prov, _, _, _) <- ask
  return prov

getNamedBoundCtx :: (MonadTensor m) => m NamedBoundCtx
getNamedBoundCtx = do
  (_, _, _, _, _, ctx) <- ask
  return $ fmap nameOf ctx

getCurrentLv :: (MonadTensor m) => m Lv
getCurrentLv = Lv . length <$> getNamedBoundCtx

addLossBinderToContext :: (MonadTensor m) => MixedLossBinder -> m a -> m a
addLossBinderToContext binder cont = do
  local
    ( \(logicID, declProv, logic, standardEnv, freeEnv, ctx) ->
        (logicID, declProv, logic, standardEnv, freeEnv, binder : ctx)
    )
    cont

--------------------------------------------------------------------------------
-- Public method

convertExprToTensorValue ::
  (MonadTensor m) =>
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (NFValue TensorBuiltin)
convertExprToTensorValue env expr = do
  x <- switchToMonadLogic $ normStandardExprToLoss env expr
  convertLossToTensorValue x

convertLossToTensorValue ::
  (MonadTensor m) =>
  MixedLossValue ->
  m (NFValue TensorBuiltin)
convertLossToTensorValue e = do
  showEntry "tensor-enter" e
  result <- case e of
    VMeta {} ->
      unexpectedExprError currentPass "VMeta"
    VUniverse l ->
      return $ VUniverse l
    VFreeVar v spine -> do
      VFreeVar v <$> traverseArgs convertLossToTensorValue spine
    VBoundVar v spine -> do
      VBoundVar v <$> traverseArgs convertLossToTensorValue spine
    VBuiltin b spine ->
      convertBuiltins b spine
    VPi binder body -> do
      binder' <- traverse convertLossToTensorValue binder
      body' <- addLossBinderToContext binder $ convertLossToTensorValue body
      return $ VPi binder' body'
    VLam binder closure -> do
      lv <- getCurrentLv
      binder' <- traverse convertLossToTensorValue binder
      body' <- addLossBinderToContext binder $ convertClosure lv binder closure
      return $ VLam binder' (NFClosure body')
  showExit "tensor-exit" result
  return result

convertClosure ::
  (MonadTensor m) =>
  Lv ->
  MixedLossBinder ->
  MixedClosure ->
  m (NFValue TensorBuiltin)
convertClosure lv binder closure = case closure of
  StandardClos (WHNFClosure env standardExpr) -> do
    let newEnv = extendEnvWithBound lv binder env
    convertExprToTensorValue newEnv standardExpr
  LossClos (LossClosure env lossExpr) -> do
    let newEnv = extendEnvWithBound lv binder env
    normLossExpr <- switchToMonadLogic $ normLossExprToLoss newEnv lossExpr
    convertLossToTensorValue normLossExpr

convertBuiltins :: (MonadTensor m) => LossBuiltin -> MixedLossSpine -> m (NFValue TensorBuiltin)
convertBuiltins b args = do
  let normArgs = traverseArgs convertLossToTensorValue args
  case b of
    -----------
    -- Types --
    -----------
    L.NatType -> VBuiltin T.NatType <$> normArgs
    L.RatType -> VBuiltin T.RatTensorType <$> normArgs
    L.IndexType -> VBuiltin T.IndexType <$> normArgs
    L.ListType -> VBuiltin T.ListType <$> normArgs
    L.VectorType -> convertVectorType =<< normArgs
    --------------
    -- Literals --
    --------------
    L.NilList -> VBuiltin T.NilList <$> normArgs
    L.ConsList -> VBuiltin T.ConsList <$> normArgs
    L.Index i -> VBuiltin (T.Index i) <$> normArgs
    L.Bool v -> return $ T.VBoolTensor (Tensor [] [v])
    L.Nat v -> VBuiltin (T.Nat v) <$> normArgs
    L.Rat v -> return $ constRatTensor (T.convertRat v)
    L.Vector -> convertVector =<< normArgs
    ----------------
    -- Operations --
    ----------------
    L.Neg NegRat -> VBuiltin T.NegRatTensor <$> normArgs
    L.Add AddNat -> unsupportedTypeError Nat
    L.Add AddRat -> VBuiltin T.AddRatTensor <$> normArgs
    L.Sub SubRat -> VBuiltin T.SubRatTensor <$> normArgs
    L.Mul MulNat -> unsupportedTypeError Nat
    L.Mul MulRat -> VBuiltin T.MulRatTensor <$> normArgs
    L.Div DivRat -> VBuiltin T.DivRatTensor <$> normArgs
    L.PowRat -> VBuiltin T.PowRatTensor <$> normArgs
    L.MinRat -> VBuiltin T.MinRatTensor <$> normArgs
    L.MaxRat -> VBuiltin T.MaxRatTensor <$> normArgs
    L.FoldVector -> convertFoldVector =<< normArgs
    L.MapVector -> convertMapVector =<< normArgs
    L.ZipWithVector -> convertZipWith =<< normArgs
    L.Indices -> VBuiltin T.Indices <$> normArgs
    L.LookupVector -> VBuiltin T.LookupRatTensor <$> normArgs
    L.FoldList -> VBuiltin T.FoldList <$> normArgs
    L.MapList -> VBuiltin T.MapList <$> normArgs
    L.ForeachIndex -> convertForeachIndex =<< normArgs
    ----------------------
    -- Other operations --
    ----------------------
    L.Minimise -> do
      let op = T.MinimiseRatTensor
      boundCtx <- getNamedBoundCtx
      let namedCtx = fmap (fromMaybe "<nameless>") boundCtx
      VBuiltin (op namedCtx) <$> normArgs
    L.Maximise -> do
      let op = T.MaximiseRatTensor
      boundCtx <- getNamedBoundCtx
      let namedCtx = fmap (fromMaybe "<nameless>") boundCtx
      VBuiltin (op namedCtx) <$> normArgs
  where
    unsupportedTypeError op = compilerDeveloperError $ "Conversion of" <+> pretty op <+> "not yet supported"

convertVectorType :: (MonadTensor m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertVectorType = \case
  [argExpr -> elemType, argExpr -> size] -> do
    let maybeResult = case elemType of
          VBuiltin b _args -> case b of
            T.BoolTensorType -> Just T.BoolTensorType
            T.RatTensorType -> Just T.RatTensorType
            T.IndexType -> Just T.IndexTensorType
            T.IndexTensorType -> Just T.IndexTensorType
            _ -> Nothing
          _ -> Nothing

    case maybeResult of
      Just result -> return $ VBuiltin result []
      Nothing -> do
        declProv <- getDeclProvenance
        boundCtx <- getNamedBoundCtx
        let vecType = VFreeVar (Identifier User "Vector") [Arg mempty Explicit Relevant elemType, Arg mempty Explicit Relevant size]
        throwError $ HigherOrderVectors declProv boundCtx vecType elemType
  _ -> unexpectedExprError currentPass "Vector has incorrect number of arguments"

convertVector :: (MonadTensor m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertVector args = case args of
  [] -> compilerDeveloperError "Malformed LVec found."
  [_t] -> compilerDeveloperError "0-dimensional tensor found"
  _t : a : as -> do
    let vs = a :| as
    return $ case argExpr a of
      T.VBoolTensor {} -> comp T.VBoolTensor T.getBoolTensor vs
      T.VRatTensor {} -> comp T.VRatTensor T.getRatTensor vs
      _ -> VBuiltin (T.StackRatTensor (length (a : as))) args
    where
      comp ::
        (Tensor a -> NFValue TensorBuiltin) ->
        (NFValue TensorBuiltin -> Maybe (Tensor a)) ->
        NonEmpty (NFArg TensorBuiltin) ->
        NFValue TensorBuiltin
      comp mk f xs = case traverse (f . argExpr) xs of
        Just constantTensors -> mk $ stack constantTensors
        Nothing -> VBuiltin (T.StackRatTensor (length (a : as))) args

constRatTensor :: T.Rat -> NFValue TensorBuiltin
constRatTensor v = VBuiltin (T.ConstRatTensor v) [explicit (VBuiltin T.NilList [])]

extendConstRatTensor :: T.Rat -> NFValue TensorBuiltin -> NFArg TensorBuiltin -> NFValue TensorBuiltin
extendConstRatTensor x dim dims = do
  let newDims = VBuiltin T.ConsList [explicit dim, dims]
  VBuiltin (T.ConstRatTensor x) [explicit newDims]

type HigherOrderFunctionConversion =
  forall m.
  (MonadTensor m) =>
  Lv ->
  [NFArg TensorBuiltin] ->
  m (Maybe (NFValue TensorBuiltin))

convertHigherOrderFunction ::
  (MonadTensor m) =>
  HigherOrderFunctionConversion ->
  LossBuiltin ->
  (NFSpine TensorBuiltin -> NFValue TensorBuiltin) ->
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertHigherOrderFunction convertFn origFn mkDefault args = do
  let defaultExpr = mkDefault args
  showEntry ("enter-" <> pretty origFn) defaultExpr
  lv <- getCurrentLv
  maybeResult <- convertFn lv args
  result <- case maybeResult of
    Just expr -> return expr
    Nothing -> do
      (ident, _) <- getDeclProvenance
      boundCtx <- getNamedBoundCtx
      logWarning $ InefficientTensorCode (nameOf ident) origFn boundCtx defaultExpr
      return defaultExpr

  showExit ("exit-" <> pretty origFn) result
  return result

convertFoldVector :: (MonadTensor m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertFoldVector = convertHigherOrderFunction go L.MapVector (VBuiltin T.MapRatTensor)
  where
    go :: HigherOrderFunctionConversion
    go lv = \case
      [_, _, _, f, argExpr -> e, xs] -> case f of
        ExplicitArg _ _ (getSimpleBinaryOp lv -> Just bop) -> return $ case bop of
          T.AddRatTensor -> Just $ evalAdd (VBuiltin T.ReduceSumRatTensor [xs]) e
          _ -> Nothing
        _ -> return Nothing
      _ -> return Nothing

convertZipWith :: (MonadTensor m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertZipWith = convertHigherOrderFunction go L.ZipWithVector (VBuiltin T.ZipWithRatTensor)
  where
    go :: HigherOrderFunctionConversion
    go lv = \case
      [a, b, c, n, argExpr -> VLam2 binder1 binder2 body, xs, ys] ->
        case body of
          VBuiltin op [e1] | isLiftableTensorOp op -> do
            let mkArgs f = [a, b, c, n, explicit f, xs, ys]
            let args1 = mkArgs (VLam2 binder1 binder2 (argExpr e1))
            xs' <- convertZipWith args1
            return $ Just $ VBuiltin op [explicit xs']
          VBuiltin op [e1, e2] | isLiftableTensorOp op -> do
            let mkArgs f = [a, b, c, n, explicit f, xs, ys]
            let args1 = mkArgs (VLam2 binder1 binder2 (argExpr e1))
            let args2 = mkArgs (VLam2 binder1 binder2 (argExpr e2))
            xs' <- convertZipWith args1
            ys' <- convertZipWith args2
            return $ Just $ VBuiltin op [explicit xs', explicit ys']
          VBuiltin (T.ConstRatTensor r) [dims] ->
            return $ Just $ extendConstRatTensor r (argExpr n) dims
          VBoundVar v []
            | v == lv ->
                return $ Just $ argExpr xs
          VBoundVar v []
            | v == lv + 1 ->
                return $ Just $ argExpr ys
          _ -> return Nothing
      _ -> return Nothing

convertMapVector :: (MonadTensor m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertMapVector = convertHigherOrderFunction go L.MapVector (VBuiltin T.MapRatTensor)
  where
    go :: HigherOrderFunctionConversion
    go _lv _args = return Nothing

convertForeachIndex :: (MonadTensor m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertForeachIndex = convertHigherOrderFunction go L.ForeachIndex (VFreeVar (identifierOf StdForeachIndex))
  where
    go :: HigherOrderFunctionConversion
    go lv = \case
      [t, size, f@(argExpr -> VLam binder (NFClosure body))] -> case body of
        -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
        -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
        VBuiltin b [e1, e2] | isLiftableTensorOp b -> do
          let mk newBody = convertForeachIndex [t, size, explicit (VLam binder (NFClosure newBody))]
          e1' <- mk $ argExpr e1
          e2' <- mk $ argExpr e2
          return $ Just $ VBuiltin b [explicit e1', explicit e2']
        -- Eliminate `forall i . xs ! i` into `xs`
        VBuiltin T.LookupRatTensor (reverse -> (argExpr -> VBoundVar lv1 _) : xs : _)
          | lv1 == lv -> return $ Just $ argExpr xs
        -- Eliminate `forall i . c` into `const c`
        T.VRatTensor (Tensor _ [x]) -> do
          let nil = VBuiltin T.NilList []
          let dims = VBuiltin T.ConsList (Arg mempty Explicit Relevant <$> [argExpr size, nil])
          return $ Just $ VBuiltin (T.ConstRatTensor x) [Arg mempty Explicit Relevant dims]
        VBuiltin (T.ConstRatTensor x) [dims] ->
          return $ Just $ extendConstRatTensor x (argExpr size) dims
        _ -> do
          let indexType = VBuiltin T.IndexType [size]
          let indices = VBuiltin T.Indices [size]
          result <- convertMapVector [implicit indexType, t, size, f, explicit indices]
          return $ Just result
      _ -> unexpectedExprError currentPass "'foreachIndex'"

pattern VLam2 :: NFBinder TensorBuiltin -> NFBinder TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
pattern VLam2 binder1 binder2 body = VLam binder1 (NFClosure (VLam binder2 (NFClosure body)))

getSimpleBinaryOp :: Lv -> NFValue TensorBuiltin -> Maybe TensorBuiltin
getSimpleBinaryOp lv e = case e of
  VLam2 _ _ (VBuiltin b [argExpr -> VBoundVar lv1 [], argExpr -> VBoundVar lv2 []]) | lv1 == lv && lv2 == lv + 1 -> Just b
  _ -> Nothing

evalAdd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAdd (T.VRatTensor xs) y | all (\r -> numerator r == 0) xs = y
evalAdd x (T.VRatTensor ys) | all (\r -> numerator r == 0) ys = x
evalAdd x y = VBuiltin T.AddRatTensor (Arg mempty Explicit Relevant <$> [x, y])

isLiftableTensorOp :: TensorBuiltin -> Bool
isLiftableTensorOp = \case
  T.NegRatTensor -> True
  T.EqRatTensor -> True
  T.NeRatTensor -> True
  T.LeRatTensor -> True
  T.LtRatTensor -> True
  T.GeRatTensor -> True
  T.GtRatTensor -> True
  T.AddRatTensor -> True
  T.SubRatTensor -> True
  T.MulRatTensor -> True
  T.DivRatTensor -> True
  T.MaxRatTensor -> True
  _ -> False

currentPass :: CompilerPass
currentPass = "conversion to tensors"

showEntry :: (MonadTensor m, PrettyFriendly (Contextualised a NamedBoundCtx)) => Doc a -> a -> m ()
showEntry doc e = do
  ctx <- getNamedBoundCtx
  logDebug MaxDetail $ doc <+> ":" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadTensor m) => Doc a -> NFValue TensorBuiltin -> m ()
showExit doc e = do
  ctx <- getNamedBoundCtx
  decrCallDepth
  logDebug MaxDetail $ doc <+> ": " <+> prettyFriendly (WithContext e ctx)
