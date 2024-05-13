module Vehicle.Backend.LossFunction.Convert
  ( convertToTensors,
    MonadTensorProperty,
  )
where

import Control.Applicative qualified as Applicative (liftA2)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, maybeToList)
import Data.Ratio
import Data.Set as Set (Set, fromList)
import Vehicle.Backend.LossFunction (MonadTensor, MonadTensorProperty, convertLossExpr)
import Vehicle.Backend.LossFunction.Core (TensorBuiltin)
import Vehicle.Backend.LossFunction.Core qualified as T
import Vehicle.Backend.LossFunction.Logics (implementationOf)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Context.Var
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote qualified as Quote
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.NormalisedExpr
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Builtin

convertToTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertToTensors logicID prog =
  logCompilerPass MinDetail currentPass $ do
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshBoundContextT (Proxy @Builtin) $
        convertProg logicID prog

convertProg ::
  (MonadTensor m) =>
  DifferentiableLogicID ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertProg logicID (Main ds) = Main <$> convertDecls logicID ds

convertDecls ::
  (MonadTensor m) =>
  DifferentiableLogicID ->
  [Decl Ix Builtin] ->
  m [Decl Ix TensorBuiltin]
convertDecls logicID = \case
  [] -> return []
  decl : decls -> do
    maybeDecl <-
      flip runReaderT ((identifierOf decl, provenanceOf decl), logicID) $
        case decl of
          DefAbstract p i s t -> do
            Just . DefAbstract p i s <$> convertDeclType t
          DefFunction p i anns t e
            | isProperty anns -> do
                logCompilerPass MinDetail ("property" <+> quotePretty i) $ do
                  hideStdLibDecls (Proxy @Builtin) preservedStdLibOps $ do
                    t' <- convertDeclType t
                    e' <- normAndConvertExpr mempty e
                    e'' <- unnormaliseNF e'
                    return $ Just $ DefFunction p i anns t' e''
            | otherwise -> return Nothing

    addDeclToContext decl $ do
      decls' <- convertDecls logicID decls
      return $ maybeToList maybeDecl ++ decls'

convertDeclType ::
  (MonadTensorProperty m) =>
  Type Ix Builtin ->
  m (Type Ix TensorBuiltin)
convertDeclType t = do
  type'' <- normAndConvertExpr mempty t
  unnormaliseNF type''

normAndConvertExpr ::
  (MonadTensorProperty m) =>
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (NFValue TensorBuiltin)
normAndConvertExpr env expr = do
  whnfExpr <- normaliseInEnv env expr
  convertExpr whnfExpr

convertExpr ::
  (MonadTensorProperty m) =>
  WHNFValue Builtin ->
  m (NFValue TensorBuiltin)
convertExpr expr = do
  (_, logicID) <- ask
  preprocessedExpr <- convertLossExpr (implementationOf logicID) expr
  logDebugM MaxDetail $ do
    ctx <- getNamedBoundCtx (Proxy @Builtin)
    return $ "Converted to loss functions:" <+> prettyFriendly (WithContext preprocessedExpr ctx)
  convertedExpr <- convertValue preprocessedExpr
  return convertedExpr

convertValue ::
  (MonadTensorProperty m) =>
  WHNFValue Builtin ->
  m (NFValue TensorBuiltin)
convertValue e = do
  showEntry e
  result <- case e of
    VMeta {} -> unexpectedExprError currentPass "VMeta"
    VUniverse l -> return $ VUniverse l
    VPi binder body -> do
      binder' <- convertBinder binder
      unnormBinder <- traverse unnormalise binder
      body' <- addBinderToContext unnormBinder $ convertValue body
      return $ VPi binder' body'
    VFreeVar v spine -> case findStdLibFunction v of
      Just StdForeachIndex -> convertForeachIndex spine
      _ -> do
        spine' <- convertSpine spine
        return $ VFreeVar v spine'
    VBoundVar v spine -> do
      spine' <- convertSpine spine
      return $ VBoundVar v spine'
    VBuiltin b spine -> do
      spine' <- convertSpine spine
      convertBuiltins b spine'
    VLam binder (WHNFBody env body) -> do
      binder' <- convertBinder binder
      unnormBinder <- traverse unnormalise binder
      lv <- getCurrentLv (Proxy @Builtin)
      body' <-
        addBinderToContext unnormBinder $
          normAndConvertExpr (extendEnvWithBound lv unnormBinder env) body
      return $ VLam binder' (NFBody body')
  showExit result
  return result

convertSpine ::
  (MonadTensorProperty m) =>
  WHNFSpine Builtin ->
  m [NFArg TensorBuiltin]
convertSpine = traverse (traverse convertValue)

convertBinder ::
  (MonadTensorProperty m) =>
  WHNFBinder Builtin ->
  m (NFBinder TensorBuiltin)
convertBinder = traverse convertValue

convertBuiltins :: (MonadTensorProperty m) => Builtin -> [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertBuiltins b args = case b of
  BuiltinType t -> convertBuiltinType t args
  BuiltinConstructor c -> convertBuiltinConstructor c args
  BuiltinFunction f -> convertBuiltinFunction f args
  TypeClass {} -> unexpectedExprError currentPass "TypeClass"
  TypeClassOp {} -> unexpectedExprError currentPass "TypeClassOp"
  NatInDomainConstraint -> unexpectedExprError currentPass "NatInDomainConstraint"

convertBuiltinType ::
  (MonadTensorProperty m) =>
  BuiltinType ->
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertBuiltinType t args = case t of
  Unit -> unexpectedExprError currentPass "Unit"
  Bool -> return $ mkBuiltin T.BoolTensorType args
  Nat -> return $ mkBuiltin T.NatType args
  Rat -> return $ mkBuiltin T.RatTensorType args
  Index -> return $ mkBuiltin T.IndexType args
  List -> return $ mkBuiltin T.ListType args
  Vector -> case args of
    [elemType, size] -> convertVectorType (argExpr elemType) (argExpr size)
    _ -> unexpectedExprError currentPass "Vector has incorrect number of arguments"

convertVectorType :: (MonadTensorProperty m) => NFValue TensorBuiltin -> NFValue TensorBuiltin -> m (NFValue TensorBuiltin)
convertVectorType elemType size = do
  let maybeResult = case elemType of
        VBuiltin b _args -> case b of
          T.BoolTensorType -> Just T.BoolTensorType
          T.RatTensorType -> Just T.RatTensorType
          T.IndexType -> Just T.IndexTensorType
          T.IndexTensorType -> Just T.IndexTensorType
          _ -> Nothing
        _ -> Nothing

  case maybeResult of
    Just result -> return $ mkBuiltin result []
    Nothing -> do
      (declProv, _) <- ask
      boundCtx <- getNamedBoundCtx (Proxy @Builtin)
      let vecType = VFreeVar (Identifier User "Vector") [Arg mempty Explicit Relevant elemType, Arg mempty Explicit Relevant size]
      throwError $ HigherOrderVectors declProv boundCtx vecType elemType

convertBuiltinConstructor :: (MonadTensorProperty m) => BuiltinConstructor -> [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertBuiltinConstructor c args = case c of
  Nil -> return $ mkBuiltin T.NilList []
  Cons -> return $ mkBuiltin T.ConsList []
  LUnit -> return $ mkBuiltin T.Unit []
  LIndex i -> return $ mkBuiltin (T.Index i) []
  LBool v -> return $ T.VBoolTensor (Tensor [] [v])
  LNat v -> return $ mkBuiltin (T.Nat v) []
  LRat v -> return $ T.VRatTensor (Tensor [] [T.convertRat v])
  LVec n -> case args of
    _t : xs -> convertVector n xs
    _ -> compilerDeveloperError "Malformed LVec found."

convertVector :: (MonadTensorProperty m) => Int -> [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertVector n args = case args of
  [] -> compilerDeveloperError "0-dimensional tensor found"
  a : as -> do
    let vs = a :| as
    return $ case argExpr a of
      T.VBoolTensor {} -> comp T.VBoolTensor T.getBoolTensor vs
      T.VRatTensor {} -> comp T.VRatTensor T.getRatTensor vs
      _ -> mkBuiltin (T.StackRatTensor n) args
    where
      comp ::
        (Tensor a -> NFValue TensorBuiltin) ->
        (NFValue TensorBuiltin -> Maybe (Tensor a)) ->
        NonEmpty (NFArg TensorBuiltin) ->
        NFValue TensorBuiltin
      comp mk f xs = case traverse (f . argExpr) xs of
        Just constantTensors -> mk $ stack constantTensors
        Nothing -> mkBuiltin (T.StackRatTensor n) args

convertBuiltinFunction ::
  (MonadTensorProperty m) =>
  BuiltinFunction ->
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertBuiltinFunction t args = case t of
  --------------------------
  --  operations --
  --------------------------
  Not -> return $ mkBuiltin T.NotBoolTensor args
  And -> return $ mkBuiltin T.AndBoolTensor args
  Or -> return $ mkBuiltin T.OrBoolTensor args
  Neg NegRat -> return $ mkBuiltin T.NegRatTensor args
  Add AddNat -> return $ mkBuiltin T.AddRatTensor args
  Add AddRat -> return $ mkBuiltin T.AddRatTensor args
  Sub SubRat -> return $ mkBuiltin T.SubRatTensor args
  Mul MulNat -> return $ mkBuiltin T.MulRatTensor args
  Mul MulRat -> return $ mkBuiltin T.MulRatTensor args
  Div DivRat -> return $ mkBuiltin T.DivRatTensor args
  PowRat -> return $ mkBuiltin T.PowRatTensor args
  MinRat -> return $ mkBuiltin T.MinRatTensor args
  MaxRat -> return $ mkBuiltin T.MaxRatTensor args
  Equals EqIndex Eq -> return $ mkBuiltin T.EqIndex args
  Equals EqNat Eq -> unsupportedTypeError t
  Equals EqRat Eq -> return $ mkBuiltin T.EqRatTensor args
  Equals EqIndex Neq -> return $ mkBuiltin T.NeIndex args
  Equals EqNat Neq -> unsupportedTypeError t
  Equals EqRat Neq -> return $ mkBuiltin T.NeRatTensor args
  Order OrderIndex Le -> return $ mkBuiltin T.LeIndex args
  Order OrderNat Le -> unsupportedTypeError t
  Order OrderRat Le -> return $ mkBuiltin T.LeRatTensor args
  Order OrderIndex Lt -> return $ mkBuiltin T.LtIndex args
  Order OrderNat Lt -> unsupportedTypeError t
  Order OrderRat Lt -> return $ mkBuiltin T.LtRatTensor args
  Order OrderIndex Ge -> return $ mkBuiltin T.GeIndex args
  Order OrderNat Ge -> unsupportedTypeError t
  Order OrderRat Ge -> return $ mkBuiltin T.GeRatTensor args
  Order OrderIndex Gt -> return $ mkBuiltin T.GtIndex args
  Order OrderNat Gt -> unsupportedTypeError t
  Order OrderRat Gt -> return $ mkBuiltin T.GtRatTensor args
  -----------------------
  -- Vector operations --
  -----------------------
  FoldVector -> convertHigherOrderFunction convertFoldVector FoldVector T.ReduceRatTensor args
  MapVector -> convertHigherOrderFunction convertMapVector MapVector T.MapRatTensor args
  ZipWithVector -> convertHigherOrderFunction convertZipWith ZipWithVector T.ZipWithRatTensor args
  Indices -> return $ mkBuiltin T.Indices args
  At -> return $ mkBuiltin T.LookupRatTensor args
  ---------------------
  -- List operations --
  ---------------------
  FoldList -> return $ mkBuiltin T.FoldList args
  MapList -> return $ mkBuiltin T.MapList args
  ----------------------
  -- Other operations --
  ----------------------
  Quantifier Forall -> return $ mkBuiltin T.Forall args
  Quantifier Exists -> return $ mkBuiltin T.Exists args
  If -> return $ mkBuiltin T.If args
  Optimise minimise -> do
    let op = if minimise then T.MinimiseRatTensor else T.MaximiseRatTensor
    boundCtx <- getBoundCtx (Proxy @Builtin)
    let namedCtx = fmap (\b -> fromMaybe "<nameless>" (nameOf b)) boundCtx
    return $ mkBuiltin (op namedCtx) args
  Implies -> unexpectedExprError currentPass "Implies"
  FromNat {} -> unexpectedExprError currentPass "FromNat"
  FromRat {} -> unexpectedExprError currentPass "FromRat"
  where
    unsupportedTypeError b = compilerDeveloperError $ "Conversion of" <+> pretty b <+> "not yet supported"

convertForeachIndex ::
  (MonadTensorProperty m) =>
  WHNFSpine Builtin ->
  m (NFValue TensorBuiltin)
convertForeachIndex spine = do
  lv <- getCurrentLv (Proxy @Builtin)
  spine' <- convertSpine spine
  case spine' of
    [_, argExpr -> size, argExpr -> VLam binder (NFBody expr)] -> do
      logDebug MaxDetail $ "Trying to convert" <+> quotePretty StdForeachIndex
      result <- distributeForeachIndex spine size (nameOf binder) lv expr
      case result of
        Just x -> return x
        Nothing -> do
          reducedExpr <- appHiddenStdlibDef StdForeachIndex spine
          convertExpr reducedExpr
    _ -> compilerDeveloperError "Unexpected expr while converting ForeachIndex"

distributeForeachIndex ::
  (MonadTensorProperty m) =>
  WHNFSpine Builtin ->
  NFValue TensorBuiltin ->
  Maybe Name ->
  Lv ->
  NFValue TensorBuiltin ->
  m (Maybe (NFValue TensorBuiltin))
distributeForeachIndex originalSpine size varName lv = \case
  -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
  -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
  VBuiltin b [e1, e2] | isLiftableTensorOp b -> do
    logCompilerSection MaxDetail ("Distributing `foreach` over" <+> pretty b) $ do
      let recurse = distributeForeachIndex originalSpine size varName lv
      e1' <- sequence <$> traverse recurse e1
      e2' <- sequence <$> traverse recurse e2
      return $ Applicative.liftA2 (\x y -> VBuiltin b [x, y]) e1' e2'
  -- Eliminate `forall i . xs ! i` into `xs`
  VBuiltin T.LookupRatTensor (reverse -> (argExpr -> VBoundVar lv1 _) : xs : _)
    | lv1 == lv ->
        return $ Just $ argExpr xs
  -- Eliminate `forall i . c` into `const c`
  T.VRatTensor (Tensor _ [x]) -> do
    let nil = mkBuiltin T.NilList []
    let dims = mkBuiltin T.ConsList (Arg mempty Explicit Relevant <$> [size, nil])
    return $ Just $ mkBuiltin (T.ConstRatTensor x) [Arg mempty Explicit Relevant dims]
  VBuiltin (T.ConstRatTensor x) [dims] -> do
    let newDims = mkBuiltin T.ConsList [dims]
    return $ Just $ mkBuiltin (T.ConstRatTensor x) [Arg mempty Explicit Relevant newDims]
  body -> do
    ctx <- getNamedBoundCtx (Proxy @Builtin)
    logDebug MaxDetail $ "Failed to convert:" <+> prettyFriendly (WithContext body (varName : ctx))
    return Nothing

type HigherOrderFunctionConversion =
  forall m.
  (MonadTensorProperty m) =>
  Lv ->
  [NFArg TensorBuiltin] ->
  m (Maybe (NFValue TensorBuiltin))

convertHigherOrderFunction ::
  (MonadTensorProperty m) =>
  HigherOrderFunctionConversion ->
  BuiltinFunction ->
  TensorBuiltin ->
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertHigherOrderFunction convertFn origFn newFun args = do
  logDebug MaxDetail $ "Trying to convert" <+> quotePretty origFn
  lv <- getCurrentLv (Proxy @Builtin)
  result <- convertFn lv args
  case result of
    Nothing -> do
      logDebug MaxDetail "Failed"
      ((ident, _), _) <- ask
      boundCtx <- getNamedBoundCtx (Proxy @Builtin)
      let expr = mkBuiltin newFun args
      logWarning $ InefficientTensorCode (nameOf ident) origFn boundCtx expr
      return expr
    Just expr -> do
      logDebug MaxDetail $ "Converted to" <+> prettyVerbose expr
      return expr

convertFoldVector :: HigherOrderFunctionConversion
convertFoldVector lv = \case
  [_, _, _, f, argExpr -> e, xs] -> case f of
    ExplicitArg _ _ (getBinaryOp lv -> Just bop) -> return $ case bop of
      T.AndBoolTensor -> Just $ evalAnd (mkBuiltin T.ReduceAndBoolTensor [xs]) e
      T.OrBoolTensor -> Just $ evalOr (mkBuiltin T.ReduceOrBoolTensor [xs]) e
      T.AddRatTensor -> Just $ evalAdd (mkBuiltin T.ReduceSumRatTensor [xs]) e
      _ -> Nothing
    _ -> return Nothing
  _ -> return Nothing

convertZipWith :: HigherOrderFunctionConversion
convertZipWith lv = \case
  [_, _, _, _, argExpr -> f, xs, ys] -> do
    return $ case getBinaryOp lv f of
      Just b | isLiftableTensorOp b -> Just $ mkBuiltin b [xs, ys]
      _ -> Nothing
  _ -> return Nothing

convertMapVector :: HigherOrderFunctionConversion
convertMapVector _lv _args = return Nothing

getBinaryOp :: Lv -> NFValue TensorBuiltin -> Maybe TensorBuiltin
getBinaryOp lv = \case
  VLam
    _
    ( NFBody
        ( VLam
            _
            ( NFBody
                ( VBuiltin
                    b
                    [ RelevantExplicitArg _ (VBoundVar lv1 []),
                      RelevantExplicitArg _ (VBoundVar lv2 [])
                      ]
                  )
              )
          )
      ) | lv1 == lv && lv2 == lv + 1 -> Just b
  _ -> Nothing

unnormaliseNF :: (MonadTensorProperty m) => NFValue TensorBuiltin -> m (Expr Ix TensorBuiltin)
unnormaliseNF e = do
  lv <- getCurrentLv (Proxy @Builtin)
  return $ Quote.unnormalise lv e

currentPass :: CompilerPass
currentPass = "conversion to tensors"

mkBuiltin ::
  TensorBuiltin ->
  [NFArg TensorBuiltin] ->
  NFValue TensorBuiltin
mkBuiltin = VBuiltin

evalAnd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAnd (T.VBoolTensor xs) y | and xs = y
evalAnd x (T.VBoolTensor ys) | and ys = x
evalAnd x y = mkBuiltin T.AndBoolTensor (Arg mempty Explicit Relevant <$> [x, y])

evalOr :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalOr (T.VBoolTensor xs) y | all not xs = y
evalOr x (T.VBoolTensor ys) | all not ys = x
evalOr x y = mkBuiltin T.OrBoolTensor (Arg mempty Explicit Relevant <$> [x, y])

evalAdd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAdd (T.VRatTensor xs) y | all (\r -> numerator r == 0) xs = y
evalAdd x (T.VRatTensor ys) | all (\r -> numerator r == 0) ys = x
evalAdd x y = mkBuiltin T.AddRatTensor (Arg mempty Explicit Relevant <$> [x, y])

-- | Standard library operations that we don't want to normalise.
preservedStdLibOps :: Set StdLibFunction
preservedStdLibOps =
  Set.fromList
    [ StdForeachIndex
    ]

isLiftableTensorOp :: TensorBuiltin -> Bool
isLiftableTensorOp = \case
  T.AndBoolTensor -> True
  T.OrBoolTensor -> True
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
  _ -> False

showEntry :: (MonadTensorProperty m) => WHNFValue Builtin -> m ()
showEntry e = do
  ctx <- getNamedBoundCtx (Proxy @Builtin)
  logDebug MaxDetail $ "tensor-enter:" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadTensorProperty m) => NFValue TensorBuiltin -> m ()
showExit e = do
  ctx <- getNamedBoundCtx (Proxy @Builtin)
  decrCallDepth
  logDebug MaxDetail $ "tensor-exit: " <+> prettyFriendly (WithContext e ctx)
