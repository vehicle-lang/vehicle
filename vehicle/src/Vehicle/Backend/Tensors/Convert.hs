module Vehicle.Backend.Tensors.Convert
  ( convertToTensors,
    MonadTensorProperty,
    TensorPreprocessingStep (..),
    noPreprocessing,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Vehicle.Backend.Tensors.Core (TensorBuiltin)
import Vehicle.Backend.Tensors.Core qualified as T
import Vehicle.Compile.Context.Var
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote qualified as Quote
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Data.Tensor
import Vehicle.Prelude.Warning
import Vehicle.Syntax.Builtin

type MonadTensor m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadBoundContext Builtin m
  )

type MonadTensorProperty m =
  ( MonadTensor m,
    MonadReader (DeclProvenance) m
  )

newtype TensorPreprocessingStep
  = TensorPreprocessingStep
      (forall m. (MonadTensorProperty m) => WHNFValue Builtin -> m (WHNFValue Builtin))

noPreprocessing :: TensorPreprocessingStep
noPreprocessing = TensorPreprocessingStep return

convertToTensors ::
  (MonadCompile m) =>
  TensorPreprocessingStep ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertToTensors preprocess prog =
  logCompilerPass MinDetail currentPass $ do
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshBoundContextT (Proxy @Builtin) $
        convertProg preprocess prog

convertProg ::
  (MonadTensor m) =>
  TensorPreprocessingStep ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertProg preprocess (Main ds) = Main <$> convertDecls preprocess ds

convertDecls ::
  (MonadTensor m) =>
  TensorPreprocessingStep ->
  [Decl Ix Builtin] ->
  m [Decl Ix TensorBuiltin]
convertDecls preprocess = \case
  [] -> return []
  decl : decls -> do
    maybeDecl <-
      flip runReaderT (identifierOf decl, provenanceOf decl) $
        case decl of
          DefAbstract p i s t ->
            Just . DefAbstract p i s <$> convertDeclType t
          DefFunction p i anns t e
            | isProperty anns -> do
                logCompilerPass MinDetail ("property" <+> quotePretty i) $ do
                  t' <- convertDeclType t
                  e' <- runReaderT (convertExpr preprocess mempty e) (i, p)
                  e'' <- unnormaliseNF e'
                  return $ Just $ DefFunction p i anns t' e''
            | otherwise -> return Nothing

    addDeclToContext decl $ do
      decls' <- convertDecls preprocess decls
      return $ maybeToList maybeDecl ++ decls'

convertDeclType ::
  (MonadTensorProperty m) =>
  Type Ix Builtin ->
  m (Type Ix TensorBuiltin)
convertDeclType t = do
  type'' <- convertExpr noPreprocessing mempty t
  unnormaliseNF type''

convertExpr ::
  (MonadTensorProperty m) =>
  TensorPreprocessingStep ->
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (NFValue TensorBuiltin)
convertExpr preprocess@(TensorPreprocessingStep pre) env expr = do
  whnfExpr <- normaliseInEnv env expr
  preprocessedExpr <- pre whnfExpr
  logDebugM MaxDetail $ do
    ctx <- getNamedBoundCtx (Proxy @Builtin)
    return $ "Pre-processed to" <+> prettyFriendly (WithContext preprocessedExpr ctx)
  convertedExpr <- convertValue preprocess preprocessedExpr
  return convertedExpr

convertValue ::
  (MonadTensorProperty m) =>
  TensorPreprocessingStep ->
  WHNFValue Builtin ->
  m (NFValue TensorBuiltin)
convertValue preprocess e = do
  showEntry e
  result <- case e of
    VMeta {} -> unexpectedExprError currentPass "VMeta"
    VUniverse l -> return $ VUniverse l
    VPi binder body -> do
      binder' <- convertBinder preprocess binder
      unnormBinder <- traverse unnormalise binder
      body' <- addBinderToContext unnormBinder $ convertValue preprocess body
      return $ VPi binder' body'
    VFreeVar v spine -> do
      spine' <- convertSpine preprocess spine
      return $ VFreeVar v spine'
    VBoundVar v spine -> do
      spine' <- convertSpine preprocess spine
      return $ VBoundVar v spine'
    VBuiltin b spine -> do
      spine' <- convertSpine preprocess spine
      convertBuiltins b spine'
    VLam binder (WHNFBody env body) -> do
      binder' <- convertBinder preprocess binder
      unnormBinder <- traverse unnormalise binder
      lv <- getCurrentLv (Proxy @Builtin)
      body' <-
        addBinderToContext unnormBinder $
          convertExpr preprocess (extendEnvWithBound lv unnormBinder env) body
      return $ VLam binder' (NFBody body')
  showExit result
  return result

convertSpine ::
  (MonadTensorProperty m) =>
  TensorPreprocessingStep ->
  WHNFSpine Builtin ->
  m [NFArg TensorBuiltin]
convertSpine preprocess = traverse (traverse (convertValue preprocess))

convertBinder ::
  (MonadTensorProperty m) =>
  TensorPreprocessingStep ->
  WHNFBinder Builtin ->
  m (NFBinder TensorBuiltin)
convertBinder preprocess = traverse (convertValue preprocess)

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
  Nat -> return $ mkBuiltin T.NatTensorType args
  Int -> return $ mkBuiltin T.IntTensorType args
  Rat -> return $ mkBuiltin T.RatTensorType args
  Index -> return $ mkBuiltin T.IndexType args
  List -> return $ mkBuiltin T.ListType args
  Vector -> convertVectorType args

convertVectorType :: (MonadTensorProperty m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertVectorType args = do
  maybeResult <- return $ case args of
    [RelevantExplicitArg _ (VBuiltin b _args), _] -> case b of
      T.BoolTensorType -> Just T.BoolTensorType
      T.NatTensorType -> Just T.NatTensorType
      T.IntTensorType -> Just T.IntTensorType
      T.RatTensorType -> Just T.RatTensorType
      T.IndexType -> Just T.NatTensorType
      _ -> Nothing
    _ -> Nothing

  case maybeResult of
    Just result -> return $ mkBuiltin result []
    Nothing -> do
      declProv <- ask
      boundCtx <- getNamedBoundCtx (Proxy @Builtin)
      let typ = VFreeVar (Identifier User "Vector") args
      throwError $ HigherOrderVectors declProv boundCtx typ

convertBuiltinConstructor :: (MonadTensorProperty m) => BuiltinConstructor -> [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertBuiltinConstructor c args = case c of
  LUnit -> unexpectedExprError currentPass "LUnit"
  Nil -> return $ mkBuiltin T.NilList []
  Cons -> return $ mkBuiltin T.ConsList []
  LIndex i -> return $ mkBuiltin (T.Index i) []
  LBool v -> return $ T.VBoolTensor (Tensor [] [v])
  LNat v -> return $ T.VNatTensor (Tensor [] [v])
  LInt v -> return $ T.VIntTensor (Tensor [] [v])
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
      T.VNatTensor {} -> comp T.VNatTensor T.getNatTensor vs
      T.VIntTensor {} -> comp T.VIntTensor T.getIntTensor vs
      T.VRatTensor {} -> comp T.VRatTensor T.getRatTensor vs
      _ -> mkBuiltin (T.Stack n) args
    where
      comp ::
        (Tensor a -> NFValue TensorBuiltin) ->
        (NFValue TensorBuiltin -> Maybe (Tensor a)) ->
        NonEmpty (NFArg TensorBuiltin) ->
        NFValue TensorBuiltin
      comp mk f xs = case traverse (f . argExpr) xs of
        Just constantTensors -> mk $ stack constantTensors
        Nothing -> mkBuiltin (T.Stack n) args

convertBuiltinFunction ::
  (MonadTensorProperty m) =>
  BuiltinFunction ->
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertBuiltinFunction t args = case t of
  --------------------------
  -- Pointwise operations --
  --------------------------
  Not -> return $ mkBuiltin T.PointwiseNot args
  And -> return $ mkBuiltin T.PointwiseAnd args
  Or -> return $ mkBuiltin T.PointwiseOr args
  Neg NegInt -> return $ mkBuiltin T.PointwiseNeg args
  Neg NegRat -> return $ mkBuiltin T.PointwiseNeg args
  Add AddNat -> return $ mkBuiltin T.PointwiseAdd args
  Add AddInt -> return $ mkBuiltin T.PointwiseAdd args
  Add AddRat -> return $ mkBuiltin T.PointwiseAdd args
  Sub SubInt -> return $ mkBuiltin T.PointwiseSub args
  Sub SubRat -> return $ mkBuiltin T.PointwiseSub args
  Mul MulNat -> return $ mkBuiltin T.PointwiseMul args
  Mul MulInt -> return $ mkBuiltin T.PointwiseMul args
  Mul MulRat -> return $ mkBuiltin T.PointwiseMul args
  Div DivRat -> return $ mkBuiltin T.PointwiseDiv args
  PowRat -> return $ mkBuiltin T.PowRat args
  MinRat -> return $ mkBuiltin T.MinRat args
  MaxRat -> return $ mkBuiltin T.MaxRat args
  Equals EqIndex Eq -> return $ mkBuiltin T.EqIndex args
  Equals EqNat Eq -> return $ mkBuiltin T.PointwiseEq args
  Equals EqInt Eq -> return $ mkBuiltin T.PointwiseEq args
  Equals EqRat Eq -> return $ mkBuiltin T.PointwiseEq args
  Equals EqIndex Neq -> return $ mkBuiltin T.NeIndex args
  Equals EqNat Neq -> return $ mkBuiltin T.PointwiseNe args
  Equals EqInt Neq -> return $ mkBuiltin T.PointwiseNe args
  Equals EqRat Neq -> return $ mkBuiltin T.PointwiseNe args
  Order OrderIndex Le -> return $ mkBuiltin T.LeIndex args
  Order OrderNat Le -> return $ mkBuiltin T.PointwiseLe args
  Order OrderInt Le -> return $ mkBuiltin T.PointwiseLe args
  Order OrderRat Le -> return $ mkBuiltin T.PointwiseLe args
  Order OrderIndex Lt -> return $ mkBuiltin T.LtIndex args
  Order OrderNat Lt -> return $ mkBuiltin T.PointwiseLt args
  Order OrderInt Lt -> return $ mkBuiltin T.PointwiseLt args
  Order OrderRat Lt -> return $ mkBuiltin T.PointwiseLt args
  Order OrderIndex Ge -> return $ mkBuiltin T.GeIndex args
  Order OrderNat Ge -> return $ mkBuiltin T.PointwiseGe args
  Order OrderInt Ge -> return $ mkBuiltin T.PointwiseGe args
  Order OrderRat Ge -> return $ mkBuiltin T.PointwiseGe args
  Order OrderIndex Gt -> return $ mkBuiltin T.GtIndex args
  Order OrderNat Gt -> return $ mkBuiltin T.PointwiseGt args
  Order OrderInt Gt -> return $ mkBuiltin T.PointwiseGt args
  Order OrderRat Gt -> return $ mkBuiltin T.PointwiseGt args
  -----------------------
  -- Vector operations --
  -----------------------
  FoldVector -> convertHigherOrderFunction convertFoldVector FoldVector args
  MapVector -> convertHigherOrderFunction convertMapVector MapVector args
  ZipWithVector -> convertHigherOrderFunction convertZipWith ZipWithVector args
  Indices -> return $ mkBuiltin T.Indices args
  At -> return $ mkBuiltin T.Lookup args
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
  Optimise b -> return $ mkBuiltin (T.Optimise b) args
  Implies -> unexpectedExprError currentPass "Implies"
  FromNat {} -> unexpectedExprError currentPass "FromNat"
  FromRat {} -> unexpectedExprError currentPass "FromRat"
  Ann {} -> case args of
    [_t, e] -> return $ argExpr e
    _ -> compilerDeveloperError "Malformed Ann expression"

convertFoldVector :: HigherOrderFunctionConversion
convertFoldVector lv = \case
  [_, _, _, f, argExpr -> e, xs] -> case f of
    ExplicitArg _ _ (getBinaryOp lv -> Just bop) -> return $ case bop of
      T.PointwiseAnd -> Just $ evalAnd (mkBuiltin T.ReduceAnd [xs]) e
      T.PointwiseOr -> Just $ evalOr (mkBuiltin T.ReduceOr [xs]) e
      T.PointwiseAdd -> Just $ evalAdd (mkBuiltin T.ReduceSum [xs]) e
      _ -> Nothing
    _ -> return Nothing
  _ -> return Nothing

convertZipWith :: HigherOrderFunctionConversion
convertZipWith lv = \case
  VZipWithVectorArgs f xs ys -> do
    return $ case getBinaryOp lv f of
      Just T.PointwiseAnd -> Just $ mkBuiltin T.PointwiseAnd [xs, ys]
      Just T.PointwiseOr -> Just $ mkBuiltin T.PointwiseOr [xs, ys]
      Just T.PointwiseEq -> Just $ mkBuiltin T.PointwiseEq [xs, ys]
      Just T.PointwiseNe -> Just $ mkBuiltin T.PointwiseNe [xs, ys]
      Just T.PointwiseAdd -> Just $ mkBuiltin T.PointwiseAdd [xs, ys]
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
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertHigherOrderFunction convertFn fn args = do
  logDebug MaxDetail $ "Trying to convert" <+> quotePretty fn
  lv <- getCurrentLv (Proxy @Builtin)
  result <- convertFn lv args
  case result of
    Nothing -> do
      logDebug MaxDetail "Failed"
      (ident, _) <- ask
      boundCtx <- getNamedBoundCtx (Proxy @Builtin)
      let expr = mkBuiltin T.ZipWithVector args
      logWarning $ InefficientTensorCode (nameOf ident) fn boundCtx expr
      return expr
    Just expr -> do
      logDebug MaxDetail $ "Converted to" <+> prettyVerbose expr
      return $ expr

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

evalAnd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAnd (T.VBoolTensor xs) y | all id xs = y
evalAnd x (T.VBoolTensor ys) | all id ys = x
evalAnd x y = mkBuiltin T.PointwiseAnd (Arg mempty Explicit Relevant <$> [x, y])

evalOr :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalOr (T.VBoolTensor xs) y | all not xs = y
evalOr x (T.VBoolTensor ys) | all not ys = x
evalOr x y = mkBuiltin T.PointwiseOr (Arg mempty Explicit Relevant <$> [x, y])

evalAdd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAdd (T.VRatTensor xs) y | all (\(n, _d) -> n == 0) xs = y
evalAdd x (T.VRatTensor ys) | all (\(n, _d) -> n == 0) ys = x
evalAdd x y = mkBuiltin T.PointwiseAdd (Arg mempty Explicit Relevant <$> [x, y])
