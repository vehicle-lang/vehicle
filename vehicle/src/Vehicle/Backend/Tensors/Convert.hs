module Vehicle.Backend.Tensors.Convert
  ( convertToTensors,
    MonadTensorProperty,
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
import Vehicle.Compile.Normalise.NBE (defaultNBEOptions, nfEval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.NormalisedExpr
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Syntax.Builtin

type MonadTensor m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadBoundContext TensorBuiltin m
  )

type MonadTensorProperty m =
  ( MonadTensor m,
    MonadReader DeclProvenance m
  )

type TensorPreprocessingStep =
  forall m. (MonadTensorProperty m) => NFValue Builtin -> m (NFValue Builtin)

convertToTensors ::
  (MonadCompile m) =>
  TensorPreprocessingStep ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertToTensors preprocess prog =
  logCompilerPass MinDetail currentPass $ do
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshBoundContextT (Proxy @TensorBuiltin) (convertProg preprocess prog)

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
                  e' <- runReaderT (convertProperty preprocess e) (i, p)
                  return $ Just $ DefFunction p i anns t' e'
            | otherwise -> return Nothing

    addDeclToContext decl $ do
      decls' <- convertDecls preprocess decls
      return $ maybeToList maybeDecl ++ decls'

convertDeclType ::
  (MonadTensorProperty m) =>
  Type Ix Builtin ->
  m (Type Ix TensorBuiltin)
convertDeclType t = do
  t' <- nfEval defaultNBEOptions emptyEnv t
  type'' <- convertExpr t'
  unnormalise type''

convertProperty ::
  (MonadTensorProperty m) =>
  TensorPreprocessingStep ->
  Expr Ix Builtin ->
  m (Expr Ix TensorBuiltin)
convertProperty preprocess expr = do
  nfExpr <- nfEval defaultNBEOptions emptyEnv expr
  processedExpr <- preprocess nfExpr
  convertedExpr <- convertExpr processedExpr
  result <- unnormalise convertedExpr
  return result

convertExpr :: (MonadTensorProperty m) => NFValue Builtin -> m (NFValue TensorBuiltin)
convertExpr e = do
  showEntry e
  result <- case e of
    VMeta {} -> unexpectedExprError currentPass "VMeta"
    VUniverse l -> return $ VUniverse l
    VPi binder body -> do
      binder' <- convertBinder binder
      unnormBinder <- traverse unnormalise binder'
      body' <- addBinderToContext unnormBinder $ convertExpr body
      return $ VPi binder' body'
    VFreeVar v spine -> do
      spine' <- convertSpine spine
      return $ VFreeVar v spine'
    VBoundVar v spine -> do
      spine' <- convertSpine spine
      return $ VBoundVar v spine'
    VBuiltin b spine -> do
      spine' <- convertSpine spine
      convertBuiltins b spine'
    VLam binder (NFBody body) -> do
      binder' <- convertBinder binder
      unnormBinder <- traverse unnormalise binder'
      body' <- addBinderToContext unnormBinder $ convertExpr body
      return $ VLam binder' (NFBody body')
  showExit result
  return result

convertSpine :: (MonadTensorProperty m) => NFSpine Builtin -> m [NFArg TensorBuiltin]
convertSpine = traverse (traverse convertExpr)

convertBinder :: (MonadTensorProperty m) => NFBinder Builtin -> m (NFBinder TensorBuiltin)
convertBinder = traverse convertExpr

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
    [RelevantExplicitArg _ t@(VBuiltin b [])] -> case b of
      T.TensorType -> return $ Just t
      _ -> return Nothing
    _ -> return Nothing

  case maybeResult of
    Just result -> return result
    Nothing -> do
      declProv <- ask
      boundCtx <- getBoundCtx (Proxy @TensorBuiltin)
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
  FoldVector -> convertFoldVector args
  MapVector -> convertMapVector args
  ZipWithVector -> convertZipWith args
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

convertFoldVector ::
  (MonadTensorProperty m) =>
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertFoldVector args = do
  lv <- boundCtxLv <$> getBoundCtx (Proxy @TensorBuiltin)
  maybeReducedOp <- case args of
    [_, _, u, e, xs] -> case u of
      ExplicitArg _ _ (getBinaryOp lv -> Just bop) -> do
        case bop of
          T.And -> return $ Just (T.And, T.BoolTensor True, T.ReduceAnd, e, xs)
          T.Or -> return $ Just (T.Or, T.BoolTensor False, T.ReduceOr, e, xs)
          T.Add -> return $ Just (T.Add, T.RatTensor 0 1, T.ReduceSum, e, xs)
          _ -> return Nothing
      _ -> return Nothing
    _ -> return Nothing

  case maybeReducedOp of
    Nothing -> inefficientCodeWarning FoldVector (mkBuiltin T.FoldVector args)
    Just (op, unit, reduceOp, baseCase, vec) -> do
      let reduceExpr = mkBuiltin reduceOp [vec]
      return $
        if argExpr baseCase == VBuiltin unit []
          then reduceExpr
          else mkBuiltin op [Arg mempty Explicit Relevant reduceExpr, baseCase]

convertZipWith ::
  (MonadTensorProperty m) =>
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertZipWith args = do
  lv <- boundCtxLv <$> getBoundCtx (Proxy @TensorBuiltin)
  maybeReducedOp <- case args of
    [_, _, _, ExplicitArg _ _ (getBinaryOp lv -> Just bop), xs, ys] -> do
      let maybeOtherOp = case bop of
            T.And -> Just T.And
            T.Or -> Just T.Or
            T.EqTensor -> Just T.EqTensor
            T.NeTensor -> Just T.NeTensor
            T.Add -> Just T.Add
            _ -> Nothing
      return $ (\b -> Just (b, xs, ys)) =<< maybeOtherOp
    _ -> return Nothing

  case maybeReducedOp of
    Nothing -> inefficientCodeWarning ZipWithVector (mkBuiltin T.ZipWithVector args)
    Just (op, xs, ys) -> return $ mkBuiltin op [xs, ys]

convertMapVector ::
  (MonadTensorProperty m) =>
  [NFArg TensorBuiltin] ->
  m (NFValue TensorBuiltin)
convertMapVector args = do
  -- TODO
  inefficientCodeWarning MapVector (mkBuiltin T.MapVector args)

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

inefficientCodeWarning ::
  (MonadTensorProperty m) =>
  BuiltinFunction ->
  NFValue TensorBuiltin ->
  m (NFValue TensorBuiltin)
inefficientCodeWarning builtin expr = do
  (ident, _) <- ask
  boundCtx <- getBoundCtx (Proxy @TensorBuiltin)
  logDebug MaxDetail $ prettyVerbose expr
  logDebug MaxDetail $ prettyVerbose boundCtx
  logWarning $ InefficientTensorCode (nameOf ident) builtin boundCtx expr
  return expr

currentPass :: CompilerPass
currentPass = "conversion to tensors"

mkBuiltin ::
  TensorBuiltin ->
  [NFArg TensorBuiltin] ->
  NFValue TensorBuiltin
mkBuiltin = VBuiltin

showEntry :: (MonadTensorProperty m) => NFValue Builtin -> m ()
showEntry e = do
  logDebug MaxDetail $ "enter:" <+> prettyVerbose e
  incrCallDepth

showExit :: (MonadTensorProperty m) => NFValue TensorBuiltin -> m ()
showExit e = do
  decrCallDepth
  logDebug MaxDetail $ "exit: " <+> prettyVerbose e
