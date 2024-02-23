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
import Data.Maybe (maybeToList)
import Data.Ratio
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
  ctx <- getNamedBoundCtx (Proxy @Builtin)
  logDebug MaxDetail $ pretty ctx
  logDebug MaxDetail $ prettyVerbose env
  logDebug MaxDetail $ "Output:" <+> prettyVerbose expr
  whnfExpr <- normaliseInEnv env expr
  logDebug MaxDetail $ "Norm output:" <+> prettyVerbose whnfExpr
  preprocessedExpr <- pre whnfExpr
  logDebug MaxDetail $ "Preproc output:" <+> prettyVerbose preprocessedExpr
  logDebug MaxDetail $ "Quoted preproc:" <+> prettyVerbose (Quote.unnormalise @(WHNFValue Builtin) @(Expr Ix Builtin) (Lv $ length ctx) preprocessedExpr)
  logDebug MaxDetail $ "Preproc output:" <+> prettyFriendly (WithContext preprocessedExpr ctx)
  convertedExpr <- convertValue preprocess preprocessedExpr
  logDebug MaxDetail $ "Convert output:" <+> prettyVerbose preprocessedExpr
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
      body' <- addBinderToContext unnormBinder $ do
        ctx <- getBoundCtx (Proxy @Builtin)
        logDebug MaxDetail $ "Ctx" <+> prettyVerbose ctx
        logDebug MaxDetail $ "Level" <+> pretty lv
        convertExpr preprocess (extendEnvWithDefined (VBoundVar lv []) unnormBinder env) body
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
  Bool -> return $ mkBuiltin T.TensorType args
  Nat -> return $ mkBuiltin T.TensorType args
  Int -> return $ mkBuiltin T.TensorType args
  Rat -> return $ mkBuiltin T.TensorType args
  Index -> return $ mkBuiltin T.IndexType args
  List -> return $ mkBuiltin T.ListType args
  Vector -> convertVectorType args

convertVectorType :: (MonadTensorProperty m) => [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertVectorType args = do
  maybeResult <- case args of
    [RelevantExplicitArg _ t@(VBuiltin b []), _] -> case b of
      T.TensorType -> return $ Just t
      _ -> return Nothing
    _ -> return Nothing

  case maybeResult of
    Just result -> return result
    Nothing -> do
      declProv <- ask
      boundCtx <- getNamedBoundCtx (Proxy @Builtin)
      let typ = VFreeVar (Identifier User "Vector") args
      throwError $ HigherOrderVectors declProv boundCtx typ

convertBuiltinConstructor :: (MonadTensorProperty m) => BuiltinConstructor -> [NFArg TensorBuiltin] -> m (NFValue TensorBuiltin)
convertBuiltinConstructor c args = case c of
  LUnit -> unexpectedExprError currentPass "LUnit"
  Nil -> return $ mkBuiltin T.NilList args
  Cons -> return $ mkBuiltin T.ConsList args
  LIndex i -> return $ mkBuiltin (T.Index i) args
  LBool v -> return $ mkBuiltin (T.BoolTensor v) args
  LNat v -> return $ mkBuiltin (T.NatTensor v) args
  LInt v -> return $ mkBuiltin (T.IntTensor v) args
  LRat v -> do
    r <- convertRat v
    return $ mkBuiltin r args
  LVec n -> return $ mkBuiltin (T.Stack n) args

convertRat :: (MonadTensorProperty m) => Rational -> m TensorBuiltin
convertRat r = do
  num <- toInt $ numerator r
  denom <- toInt $ denominator r
  return $ T.RatTensor num denom
  where
    toInt x
      | x < toInteger (minBound :: Int) = compilerDeveloperError $ "Underflow converting" <+> pretty x <+> "to `Int`"
      | x > toInteger (maxBound :: Int) = compilerDeveloperError $ "Overflow converting" <+> pretty x <+> "to `Int`"
      | otherwise = return $ fromInteger x

convertBuiltinFunction ::
  (MonadTensorProperty m) =>
  BuiltinFunction ->
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertBuiltinFunction t args = case t of
  --------------------------
  -- Pointwise operations --
  --------------------------
  Not -> return $ mkBuiltin T.Not args
  And -> return $ mkBuiltin T.And args
  Or -> return $ mkBuiltin T.Or args
  Neg NegInt -> return $ mkBuiltin T.Neg args
  Neg NegRat -> return $ mkBuiltin T.Neg args
  Add AddNat -> return $ mkBuiltin T.Add args
  Add AddInt -> return $ mkBuiltin T.Add args
  Add AddRat -> return $ mkBuiltin T.Add args
  Sub SubInt -> return $ mkBuiltin T.Sub args
  Sub SubRat -> return $ mkBuiltin T.Sub args
  Mul MulNat -> return $ mkBuiltin T.Mul args
  Mul MulInt -> return $ mkBuiltin T.Mul args
  Mul MulRat -> return $ mkBuiltin T.Mul args
  Div DivRat -> return $ mkBuiltin T.Div args
  PowRat -> return $ mkBuiltin T.PowRat args
  MinRat -> return $ mkBuiltin T.MinRat args
  MaxRat -> return $ mkBuiltin T.MaxRat args
  Equals EqIndex Eq -> return $ mkBuiltin T.EqIndex args
  Equals EqNat Eq -> return $ mkBuiltin T.EqTensor args
  Equals EqInt Eq -> return $ mkBuiltin T.EqTensor args
  Equals EqRat Eq -> return $ mkBuiltin T.EqTensor args
  Equals EqIndex Neq -> return $ mkBuiltin T.NeIndex args
  Equals EqNat Neq -> return $ mkBuiltin T.NeTensor args
  Equals EqInt Neq -> return $ mkBuiltin T.NeTensor args
  Equals EqRat Neq -> return $ mkBuiltin T.NeTensor args
  Order OrderIndex Le -> return $ mkBuiltin T.LeIndex args
  Order OrderNat Le -> return $ mkBuiltin T.LeTensor args
  Order OrderInt Le -> return $ mkBuiltin T.LeTensor args
  Order OrderRat Le -> return $ mkBuiltin T.LeTensor args
  Order OrderIndex Lt -> return $ mkBuiltin T.LtIndex args
  Order OrderNat Lt -> return $ mkBuiltin T.LtTensor args
  Order OrderInt Lt -> return $ mkBuiltin T.LtTensor args
  Order OrderRat Lt -> return $ mkBuiltin T.LtTensor args
  Order OrderIndex Ge -> return $ mkBuiltin T.GeIndex args
  Order OrderNat Ge -> return $ mkBuiltin T.GeTensor args
  Order OrderInt Ge -> return $ mkBuiltin T.GeTensor args
  Order OrderRat Ge -> return $ mkBuiltin T.GeTensor args
  Order OrderIndex Gt -> return $ mkBuiltin T.GtIndex args
  Order OrderNat Gt -> return $ mkBuiltin T.GtTensor args
  Order OrderInt Gt -> return $ mkBuiltin T.GtTensor args
  Order OrderRat Gt -> return $ mkBuiltin T.GtTensor args
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
  [_, _, f, argExpr -> e, xs] -> case f of
    ExplicitArg _ _ (getBinaryOp lv -> Just bop) -> return $ case bop of
      T.And -> Just $ evalAnd (mkBuiltin T.ReduceAnd [xs]) e
      T.Or -> Just $ evalOr (mkBuiltin T.ReduceOr [xs]) e
      T.Add -> Just $ evalAdd (mkBuiltin T.ReduceSum [xs]) e
      _ -> Nothing
    _ -> return Nothing
  _ -> return Nothing

{-
case maybeReducedOp of
  Just (op, unit, reduceOp) -> _
    let reduceExpr = mkBuiltin reduceOp
    return $ Just $ if _ then _ else _ --argExpr e == VBuiltin unit []
          --then _ -- (reduceOp, [xs])
          --else _ -- (op, [Arg mempty Explicit Relevant reduceExpr, e])
  -}
convertZipWith :: HigherOrderFunctionConversion
convertZipWith lv = \case
  VZipWithVectorArgs f xs ys -> do
    return $ case getBinaryOp lv f of
      Just T.And -> Just $ mkBuiltin T.And [xs, ys]
      Just T.Or -> Just $ mkBuiltin T.Or [xs, ys]
      Just T.EqTensor -> Just $ mkBuiltin T.EqTensor [xs, ys]
      Just T.NeTensor -> Just $ mkBuiltin T.NeTensor [xs, ys]
      Just T.Add -> Just $ mkBuiltin T.Add [xs, ys]
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
  logDebug MaxDetail $ "Trying to convert" <+> quotePretty ZipWithVector
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

pattern VBoolTensor :: Bool -> NFValue TensorBuiltin
pattern VBoolTensor b = VBuiltin (T.BoolTensor b) []

pattern VRatTensor :: Int -> Int -> NFValue TensorBuiltin
pattern VRatTensor n d <- VBuiltin (T.RatTensor n d) []

evalAnd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAnd (VBoolTensor True) y = y
evalAnd x (VBoolTensor True) = x
evalAnd x y = mkBuiltin T.And (Arg mempty Explicit Relevant <$> [x, y])

evalOr :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalOr (VBoolTensor False) y = y
evalOr x (VBoolTensor False) = x
evalOr x y = mkBuiltin T.And (Arg mempty Explicit Relevant <$> [x, y])

evalAdd :: NFValue TensorBuiltin -> NFValue TensorBuiltin -> NFValue TensorBuiltin
evalAdd (VRatTensor 0 _) y = y
evalAdd x (VRatTensor 0 _) = x
evalAdd x y = mkBuiltin T.Add (Arg mempty Explicit Relevant <$> [x, y])
