{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.JSON
  ( convertToJSONProg,
    convertToJSONSearchProg,
    convertFromJSONProg,
    convertFromJSONSearchProg,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.List (elemIndex)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Backend.LossSearch qualified as L (SearchTree (..))
import Vehicle.Compile.Arity
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Prelude (Ix (..), getBinderName)
import Vehicle.Compile.Prelude qualified as S (Binder, Decl, Expr (..), GenericDecl (..), GenericProg (..), Prog)
import Vehicle.Compile.Prelude.Utils (getNamedBinderInfo)
import Vehicle.Compile.Print
import Vehicle.Data.AST.Decl
  ( DefAbstractSort (..),
    DefFunctionSort (..),
    FunctionDeclAnnotation (..),
    ParameterSort (..),
  )
import Vehicle.Data.AST.Expr.Scoped (normAppList)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard.Core (Builtin (..), BuiltinConstructor, BuiltinFunction, BuiltinType)
import Vehicle.Data.Builtin.Standard.Core qualified as B
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.BooleanExpr qualified as P
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor (ExtendedRatTensor, Tensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, runFreshFreeContextT)
import Vehicle.Prelude (Doc, GenericArg (..), HasName (..), HasType (..), Identifier (..), Name, Provenance, explicit, indent, jsonOptions, line, mkExplicitBinder, resolutionError, squotes, stdlibIdentifier, userModulePath)
import Vehicle.Prelude.Error (developerError)
import Vehicle.Prelude.Logging.Class
import Vehicle.Verify.Specification (QuerySet (..))

--------------------------------------------------------------------------------
-- Public method
--------------------------------------------------------------------------------

convertToJSONProg :: (MonadCompile m) => S.Prog Builtin -> m JProg
convertToJSONProg prog =
  logCompilerSection2 MinDetail currentPass $ do
    -- relevantProg <- removeIrrelevantCodeFromProg prog
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshNameBoundContextT $
        convertProg prog

convertToJSONSearchProg :: (MonadCompile m) => ([L.SearchTree], S.Prog Builtin) -> m JSearchProg
convertToJSONSearchProg (searchTrees, prog) =
  logCompilerSection2 MinDetail currentPass $ do
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshNameBoundContextT $
        convertSearchProg searchTrees prog

convertFromJSONProg :: JProg -> S.Prog Builtin
convertFromJSONProg = fromJProg

convertFromJSONSearchProg :: JSearchProg -> ([L.SearchTree], S.Prog Builtin)
convertFromJSONSearchProg = fromJSearchProg

--------------------------------------------------------------------------------
-- The AST exported to JSON
--------------------------------------------------------------------------------

newtype JProg
  = Main [JDecl]
  deriving (Generic)

data JSearchProg = SearchMain
  { searchTrees :: [JSearchTree],
    prog :: JProg
  }
  deriving (Generic)

data JSearchTree
  = SearchTree Provenance Name JBooleanExpr
  deriving (Generic)

data JBooleanExpr
  = Conjunct (P.ConjunctAll JBooleanExpr)
  | Disjunct (P.DisjunctAll JBooleanExpr)
  | Query (QuerySet Name)
  deriving (Generic)

data JDecl
  = DefFunction Provenance Name JType JExpr
  | DefAbstract Provenance Name JSort JType
  deriving (Generic)

data JBinder
  = Binder Provenance Name JType
  deriving (Show, Generic)

data JType
  = Pi JType JType
  | BoolType
  | RatType
  | TensorType JType
  | VectorType JType
  | DimensionType
  | DimensionsType
  | DimensionIndexType
  | TypeVar Name [JExpr]
  deriving (Show, Generic)

data JSort
  = Network
  | Dataset
  | Parameter
  | Builtin
  deriving (Show, Generic)

data JExpr
  = -- Types
    Lam JBinder JExpr
  | Var Name [JExpr]
  | BoolTensor (Tensor Bool)
  | BoolNot JExpr
  | BoolAnd JExpr JExpr
  | BoolOr JExpr JExpr
  | BoolImplies JExpr JExpr
  | BoolCompareIndex B.ComparisonOp JExpr JExpr
  | BoolCompareNat B.ComparisonOp JExpr JExpr
  | BoolCompareRatTensor B.ComparisonOp JExpr JExpr JExpr JExpr
  | BoolReduceAnd JExpr
  | BoolReduceOr JExpr
  | BoolIf JExpr JExpr JExpr
  | -- Rational tensors
    RatTensor ExtendedRatTensor
  | NegRatTensor JExpr
  | LogRatTensor JExpr
  | ExpRatTensor JExpr
  | AddRatTensor JExpr JExpr
  | SubRatTensor JExpr JExpr
  | MulRatTensor JExpr JExpr
  | DivRatTensor JExpr JExpr
  | MinRatTensor JExpr JExpr
  | MaxRatTensor JExpr JExpr
  | PowRatTensor JExpr JExpr
  | ReduceAddRatTensor JExpr
  | ReduceMulRatTensor JExpr
  | ReduceMinRatTensor JExpr
  | ReduceMaxRatTensor JExpr
  | ConstTensor JExpr JExpr
  | StackTensor [JExpr]
  | ForeachTensor JExpr JExpr
  | AtTensor JExpr JExpr
  | SearchRatTensor Name JExpr JExpr JExpr JExpr -- (Dims, LowerBound, UpperBound, SearchLambda)
  | -- Vector
    VectorLiteral [JExpr]
  | AtVector JExpr JExpr
  | ForeachVector JExpr JExpr
  | -- Dimensions
    Dimension Int
  | DimensionNil
  | DimensionCons JExpr JExpr
  | DimensionIndex Int
  | Transpose JExpr
  deriving (Show, Generic)

instance ToJSON JProg where
  toJSON = genericToJSON jsonOptions

instance ToJSON JSearchProg where
  toJSON = genericToJSON jsonOptions

instance ToJSON JDecl where
  toJSON = genericToJSON jsonOptions

instance ToJSON JSearchTree where
  toJSON = genericToJSON jsonOptions

instance ToJSON JBooleanExpr where
  toJSON = genericToJSON jsonOptions

instance ToJSON JExpr where
  toJSON = genericToJSON jsonOptions

instance ToJSON JType where
  toJSON = genericToJSON jsonOptions

instance ToJSON JSort where
  toJSON = genericToJSON jsonOptions

instance ToJSON JBinder where
  toJSON = genericToJSON jsonOptions

instance ToJSON B.ComparisonOp where
  toJSON = toJSON . show

--------------------------------------------------------------------------------
-- Conversion to JExpr
--------------------------------------------------------------------------------

currentPass :: Doc a
currentPass = "conversion to JSON"

type MonadJSON m =
  ( MonadCompile m,
    MonadNameContext m,
    MonadFreeContext Builtin m
  )

unsupportedError :: (MonadJSON m, Pretty a) => a -> m b
unsupportedError a = throwError $ UnsupportedLossOperation (stdlibIdentifier "unknown", mempty) (pretty a)

dependentTypesError :: (Pretty a) => a -> b
dependentTypesError b = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

--------------------------------------------------------------------------------
-- Programs and declarations

convertProg :: (MonadJSON m) => S.Prog Builtin -> m JProg
convertProg (S.Main decls) = Main <$> traverse convertDecl decls

convertDecl :: (MonadJSON m) => S.Decl Builtin -> m JDecl
convertDecl = \case
  S.DefAbstract p ident sort typ -> do
    typ' <- convertType emptyBoundEnv typ
    case sort of
      NetworkDef -> return $ DefAbstract p (nameOf ident) Network typ'
      DatasetDef -> return $ DefAbstract p (nameOf ident) Dataset typ'
      ParameterDef _ -> return $ DefAbstract p (nameOf ident) Parameter typ'
      BuiltinDef -> developerError "DefAbstractSort BuiltinDef is not yet implemented"
  S.DefRecord {} -> developerError "Found record when converting to JSON"
  S.DefFunction p ident _ typ body -> do
    typ' <- convertType emptyBoundEnv typ
    expr' <- convertExpr emptyBoundEnv body
    return $ DefFunction p (nameOf ident) typ' expr'

convertSearchProg :: (MonadJSON m) => [L.SearchTree] -> S.Prog Builtin -> m JSearchProg
convertSearchProg searchTrees prog = do
  searchTrees' <- traverse convertSearchTree searchTrees
  prog' <- convertProg prog
  return $ SearchMain searchTrees' prog'

convertSearchTree :: (MonadJSON m) => L.SearchTree -> m JSearchTree
convertSearchTree = \case
  L.SearchTree p ident (NonTrivial expr) -> do
    expr' <- convertBooleanExpr expr
    return $ SearchTree p (nameOf ident) expr'
  L.SearchTree _ _ (Trivial _) -> developerError "Empty search tree"

--------------------------------------------------------------------------------
-- Types

convertType ::
  (MonadJSON m) =>
  BoundEnv Builtin ->
  S.Expr Builtin ->
  m JType
convertType env body = convertTypeValue $ Unforced env body

convertTypeValue :: (MonadJSON m) => UnforcedType Builtin -> m JType
convertTypeValue expr = do
  showEntry expr
  forcedExpr <- forceThunk expr
  result <- case forcedExpr of
    VFreeVar {} -> resolutionError currentPass "VFreeVar"
    VUniverse {} -> resolutionError currentPass "Universe"
    VRecord {} -> resolutionError currentPass "VRecord"
    VRecordAcc {} -> resolutionError currentPass "VRecordAcc"
    VLam {} -> dependentTypesError ("VLam" :: String)
    VPi binder closure -> do
      typ' <- convertTypeValue (typeOf binder)
      closure' <- convertClosure convertType binder closure
      return $ Pi typ' closure'
    VBuiltin b spine -> convertBuiltinType b spine
    VBoundVar v spine -> do
      name <- lvToProperName mempty v
      spine' <- traverse (convertValue . argExpr) spine
      return $ TypeVar name spine'
  showExit result
  return result

convertBuiltinType :: (MonadJSON m) => Builtin -> UnforcedSpine Builtin -> m JType
convertBuiltinType b spine = case b of
  BuiltinType op -> case op of
    B.UnitType -> unsupportedError b
    B.BoolType -> convertNullaryOp b BoolType spine
    B.IndexType -> convertIndexType spine
    B.NatType -> convertNullaryOp b DimensionType spine
    B.RatType -> convertNullaryOp b RatType spine
    B.ListType -> convertListType spine
    B.TensorType -> convertTensorType spine
    B.VectorType -> convertVectorType spine
  _ -> dependentTypesError b

convertIndexType :: (MonadJSON m) => UnforcedSpine Builtin -> m JType
convertIndexType spine = case spine of
  (fmap argExpr -> [_t]) -> return DimensionIndexType
  _ -> arityError B.IndexType 1 spine

convertTensorType :: (MonadJSON m) => UnforcedSpine Builtin -> m JType
convertTensorType spine = case spine of
  (fmap argExpr -> [t, _ds]) -> TensorType <$> convertTypeValue t
  _ -> arityError B.TensorType 2 spine

convertVectorType :: (MonadJSON m) => UnforcedSpine Builtin -> m JType
convertVectorType spine = case spine of
  (fmap argExpr -> [t, _d]) -> VectorType <$> convertTypeValue t
  _ -> arityError B.VectorType 2 spine

convertListType :: (MonadJSON m) => UnforcedSpine Builtin -> m JType
convertListType spine = case spine of
  (fmap argExpr -> [_t]) -> return DimensionsType
  _ -> arityError B.ListType 1 spine

--------------------------------------------------------------------------------
-- Expressions

convertBooleanExpr :: (MonadJSON m) => P.BooleanExpr (QuerySet Name) -> m JBooleanExpr
convertBooleanExpr = \case
  P.Conjunct es -> do
    es' <- traverse convertBooleanExpr es
    return $ Conjunct es'
  P.Disjunct es -> do
    es' <- traverse convertBooleanExpr es
    return $ Disjunct es'
  P.Query (QuerySet negated disjuncts) -> do
    return $ Query (QuerySet negated disjuncts)

convertExpr :: (MonadJSON m) => BoundEnv Builtin -> S.Expr Builtin -> m JExpr
convertExpr env body = do
  let normBody = Unforced env body
  convertValue normBody

convertValue :: (MonadJSON m) => Thunk Builtin -> m JExpr
convertValue expr = do
  showEntry expr
  forcedValue <- forceThunk expr
  result <- case forcedValue of
    VUniverse {} -> resolutionError currentPass "Universe"
    VRecord {} -> resolutionError currentPass "VRecord"
    VRecordAcc {} -> resolutionError currentPass "VRecordAcc"
    VPi {} -> resolutionError currentPass "VPi"
    VLam binder closure -> do
      binder' <- convertBinder binder
      closure' <- convertClosure convertExpr binder closure
      return $ Lam binder' closure'
    VBuiltin b spine -> convertBuiltin b spine
    VFreeVar v spine -> do
      let name = nameOf v
      spine' <- traverse (convertValue . argExpr) spine
      return $ Var name spine'
    VBoundVar v spine -> do
      name <- lvToProperName mempty v
      spine' <- traverse (convertValue . argExpr) spine
      return $ Var name spine'
  showExit result
  return result

convertBinder :: (MonadJSON m) => UnforcedBinder Builtin -> m JBinder
convertBinder binder = do
  let (name, p) = getNamedBinderInfo binder
  typ' <- convertTypeValue (typeOf binder)
  return $ Binder p name typ'

convertClosure ::
  (MonadJSON m) =>
  (BoundEnv Builtin -> S.Expr Builtin -> m a) ->
  UnforcedBinder Builtin ->
  Closure Builtin ->
  m a
convertClosure f binder (Closure env body) = do
  lv <- getBinderDepth
  let newEnv = extendEnvWithBound lv binder env
  addNameToContext binder $ do
    debugFriendly body
    f newEnv body

convertBuiltin :: (MonadJSON m) => Builtin -> UnforcedSpine Builtin -> m JExpr
convertBuiltin b spine = case b of
  BuiltinType op -> resolutionError currentPass (pretty op)
  BuiltinConstructor op -> case op of
    B.Nil -> convertNil spine
    B.Cons -> convertCons spine
    B.UnitLiteral -> unsupportedError b
    B.BoolTensorLiteral t -> convertNullaryOp b (BoolTensor t) spine
    B.NatTensorLiteral _ -> unsupportedError b
    B.IndexLiteral i -> convertNullaryOp b (DimensionIndex i) []
    B.NatLiteral x -> convertNullaryOp b (Dimension x) spine
    B.RatTensorLiteral t -> convertNullaryOp b (RatTensor t) spine
    B.VectorLiteral -> convertVectorLiteral spine
  BuiltinFunction op -> case op of
    B.Not -> convertTensorOp1 b BoolNot spine
    B.And -> convertTensorOp2 b BoolAnd spine
    B.Or -> convertTensorOp2 b BoolOr spine
    B.Implies -> convertTensorOp2 b BoolImplies spine
    B.QuantifyRatTensor {} -> developerError "QuantifyRatTensor should not have reached JSON conversion"
    B.QuantifyRecord {} -> developerError "QuantifyRecord should not have reached JSON conversion"
    B.If -> convertIf spine
    B.CompareIndex cmp -> convertCompareIndex cmp spine
    B.CompareNat cmp -> convertCompareNat cmp spine
    B.CompareRatTensor cmp -> convertCompareRatTensor cmp spine
    B.ReduceAndTensor -> convertTensorReduction b BoolReduceAnd spine
    B.ReduceOrTensor -> convertTensorReduction b BoolReduceOr spine
    B.Neg B.NegRatTensor -> convertTensorOp1 b NegRatTensor spine
    B.Add B.AddRatTensor -> convertTensorOp2 b AddRatTensor spine
    B.Mul B.MulRatTensor -> convertTensorOp2 b MulRatTensor spine
    B.Sub B.SubRatTensor -> convertTensorOp2 b SubRatTensor spine
    B.Div B.DivRatTensor -> convertTensorOp2 b DivRatTensor spine
    B.Min B.MinRatTensor -> convertTensorOp2 b MinRatTensor spine
    B.Max B.MaxRatTensor -> convertTensorOp2 b MaxRatTensor spine
    B.Pow B.PowRatTensor -> convertTensorOp2 b PowRatTensor spine
    B.Log B.LogRatTensor -> convertTensorOp1 b LogRatTensor spine
    B.Exp B.ExpRatTensor -> convertTensorOp1 b ExpRatTensor spine
    B.ReduceAddRatTensor -> convertTensorReduction b ReduceAddRatTensor spine
    B.ReduceMulRatTensor -> convertTensorReduction b ReduceMulRatTensor spine
    B.ReduceMinRatTensor -> convertTensorReduction b ReduceMinRatTensor spine
    B.ReduceMaxRatTensor -> convertTensorReduction b ReduceMaxRatTensor spine
    B.AtTensor -> convertAtTensor spine
    B.ForeachTensor -> convertForeachTensor spine
    B.StackTensor -> convertStackTensor spine
    B.ConstTensor -> convertConstTensor spine
    B.Transpose -> convertTranspose convertValue spine
    B.ForeachVector -> convertForeachVector spine
    B.AtVector -> convertAtVector spine
    B.SearchRatTensor -> convertSearch spine
    -- Dimension operations, not yet converted
    B.Add B.AddNat -> unsupportedError b
    B.Mul B.MulNat -> unsupportedError b
    B.MapList -> unsupportedError b
    B.FoldList -> unsupportedError b
    B.ReverseList -> unsupportedError b
    B.AppendList -> unsupportedError b
    B.Iterate -> unsupportedError b
  _ -> dependentTypesError b

convertNullaryOp :: (MonadJSON m) => Builtin -> a -> UnforcedSpine Builtin -> m a
convertNullaryOp b fn = \case
  [] -> return fn
  spine -> arityError b 0 spine

convertNonNullaryOp ::
  (MonadJSON m, IsArgs args, Pretty fn) =>
  fn ->
  Arity ->
  (args (Thunk Builtin) -> m a) ->
  UnforcedSpine Builtin ->
  m a
convertNonNullaryOp op arity f spine =
  case getExpr accessSpine spine of
    Just args -> f args
    Nothing -> arityError op arity spine

convertNil :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertNil = convertNonNullaryOp B.Nil 1 $
  \NilArgs {} ->
    return DimensionNil

convertCons :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertCons = convertNonNullaryOp B.Cons 4 $
  \(ConsArgs _t v ds) ->
    DimensionCons <$> convertValue v <*> convertValue ds

convertTensorOp1 ::
  (MonadJSON m) =>
  Builtin ->
  (JExpr -> JExpr) ->
  UnforcedSpine Builtin ->
  m JExpr
convertTensorOp1 b fn = convertNonNullaryOp b 1 $
  \(TensorOp1Args _ x) ->
    fn <$> convertValue x

convertTensorOp2 ::
  (MonadJSON m) =>
  Builtin ->
  (JExpr -> JExpr -> JExpr) ->
  UnforcedSpine Builtin ->
  m JExpr
convertTensorOp2 b fn = convertNonNullaryOp b 2 $
  \(TensorOp2Args _ x y) ->
    fn <$> convertValue x <*> convertValue y

convertTensorReduction ::
  (MonadJSON m) =>
  Builtin ->
  (JExpr -> JExpr) ->
  UnforcedSpine Builtin ->
  m JExpr
convertTensorReduction b fn = convertNonNullaryOp b 1 $
  \(TensorReductionArgs _ xs) ->
    fn <$> convertValue xs

convertAtTensor ::
  (MonadJSON m) =>
  UnforcedSpine Builtin ->
  m JExpr
convertAtTensor = convertNonNullaryOp B.AtTensor 5 $
  \(AtTensorArgs _t _d _ds xs i) ->
    AtTensor <$> convertValue xs <*> convertValue i

convertStackTensor :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertStackTensor = convertNonNullaryOp B.StackTensor 4 $
  \(StackTensorArgs _t _d _ds xs) ->
    StackTensor <$> traverse convertValue xs

convertConstTensor :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertConstTensor = convertNonNullaryOp B.ConstTensor 4 $
  \(ConstTensorArgs _t v ds) ->
    ConstTensor <$> convertValue v <*> convertValue ds

convertForeachTensor :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertForeachTensor = convertNonNullaryOp B.ForeachTensor 4 $
  \(ForeachTensorArgs _t d _ds fn) ->
    ForeachTensor <$> convertValue d <*> convertValue fn

convertVectorLiteral :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertVectorLiteral = convertNonNullaryOp B.VectorLiteral 4 $
  \(VectorLitArgs _t _ xs) ->
    VectorLiteral <$> traverse convertValue xs

convertAtVector ::
  (MonadJSON m) =>
  UnforcedSpine Builtin ->
  m JExpr
convertAtVector = convertNonNullaryOp B.AtVector 4 $
  \(AtVectorArgs _t _d xs i) ->
    AtVector <$> convertValue xs <*> convertValue i

convertForeachVector :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertForeachVector = convertNonNullaryOp B.ForeachVector 4 $
  \(ForeachVectorArgs _t d fn) ->
    ForeachVector <$> convertValue d <*> convertValue fn

convertIf :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertIf = convertNonNullaryOp B.If 4 $
  \(IfArgs _t c x y) ->
    BoolIf <$> convertValue c <*> convertValue x <*> convertValue y

convertCompareIndex :: (MonadJSON m) => B.ComparisonOp -> UnforcedSpine Builtin -> m JExpr
convertCompareIndex op = convertNonNullaryOp (B.CompareIndex op) 4 $
  \(IndexComparisonArgs _n1 _n2 x y) ->
    BoolCompareIndex op <$> convertValue x <*> convertValue y

convertCompareNat :: (MonadJSON m) => B.ComparisonOp -> UnforcedSpine Builtin -> m JExpr
convertCompareNat op = convertNonNullaryOp (B.CompareNat op) 2 $
  \(Op2Args x y) ->
    BoolCompareNat op <$> convertValue x <*> convertValue y

convertCompareRatTensor :: (MonadJSON m) => B.ComparisonOp -> UnforcedSpine Builtin -> m JExpr
convertCompareRatTensor op = convertNonNullaryOp (B.CompareRatTensor op) 4 $
  \(TensorComparisonArgs pDims rDims x y) ->
    BoolCompareRatTensor op <$> convertValue pDims <*> convertValue rDims <*> convertValue x <*> convertValue y

convertTranspose ::
  (MonadJSON m) =>
  (Thunk Builtin -> m JExpr) ->
  UnforcedSpine Builtin ->
  m JExpr
convertTranspose convert spine = case getExpr accessSpine spine of
  Just (TransposeTensorArgs _t _ds xs) -> Transpose <$> convert xs
  Nothing -> arityError B.Transpose 3 spine

convertSearch :: (MonadJSON m) => UnforcedSpine Builtin -> m JExpr
convertSearch = convertNonNullaryOp B.SearchRatTensor 4 $
  \(SearchRatTensorArgs dims lowerBound upperBound fn) -> do
    let name = case fn of
          Forced (VLam binder _) -> getBinderName binder
          Unforced _ (S.Lam _ binder _) -> getBinderName binder
          _ -> developerError "Malformed search operation"

    SearchRatTensor name <$> convertValue dims <*> convertValue lowerBound <*> convertValue upperBound <*> convertValue fn

arityError :: (MonadCompile m, Pretty fn) => fn -> Arity -> UnforcedSpine Builtin -> m a
arityError fun arity explicitArgs =
  compilerDeveloperError $
    "Number of args is different from expected arity:"
      <> line
      <> indent
        2
        ( "fun:"
            <+> pretty fun
            <> line
            <> "fun-arity:"
            <+> pretty arity
            <> line
            <> "args-len:"
            <+> prettyVerbose (length explicitArgs)
            <> line
            <> "args:"
            <+> prettyVerbose explicitArgs
        )

showEntry :: (MonadJSON m) => Thunk Builtin -> m ()
showEntry e = do
  logDebug MaxDetail $ "json-enter:" <+> prettyVerbose e
  incrCallDepth

showExit :: (MonadJSON m) => a -> m ()
showExit _e = do
  logDebug MaxDetail "json-exit"
  decrCallDepth

--------------------------------------------------------------------------------
-- Conversion back (for printing purposes)
--------------------------------------------------------------------------------

fromJProg :: JProg -> S.Prog Builtin
fromJProg = \case
  Main decls -> S.Main (fmap fromJDecl decls)

fromJSearchProg :: JSearchProg -> ([L.SearchTree], S.Prog Builtin)
fromJSearchProg = \case
  SearchMain searchTrees prog ->
    let searchTrees' = fmap fromJSearchTree searchTrees
        prog' = fromJProg prog
     in (searchTrees', prog')

fromJSearchTree :: JSearchTree -> L.SearchTree
fromJSearchTree = \case
  SearchTree p name expr ->
    let ident = Identifier userModulePath name
        expr' = fromJBooleanExpr expr
     in L.SearchTree p ident (NonTrivial expr')

fromJBooleanExpr :: JBooleanExpr -> P.BooleanExpr (QuerySet Name)
fromJBooleanExpr = \case
  Conjunct es -> P.Conjunct (fmap fromJBooleanExpr es)
  Disjunct es -> P.Disjunct (fmap fromJBooleanExpr es)
  Query (QuerySet negated es) -> P.Query (QuerySet negated es)

fromJDecl :: JDecl -> S.Decl Builtin
fromJDecl = \case
  DefFunction p name typ body ->
    runFreshNameBoundContext $ do
      typ' <- fromJType typ
      body' <- fromJExpr body
      let ident = Identifier userModulePath name
      let sort = FunctionDecl 0 (Just AnnProperty)
      return $ S.DefFunction p ident sort typ' body'
  DefAbstract p name sort typ ->
    runFreshNameBoundContext $ do
      typ' <- fromJType typ
      let ident = Identifier userModulePath name
      case sort of
        Network -> return $ S.DefAbstract p ident NetworkDef typ'
        Dataset -> return $ S.DefAbstract p ident DatasetDef typ'
        Parameter -> return $ S.DefAbstract p ident (ParameterDef Inferable) typ'
        Builtin -> developerError "DefAbstractSort BuiltinDef is not yet implemented"

fromJType :: (MonadNameContext m) => JType -> m (S.Expr Builtin)
fromJType = \case
  Pi input output -> do
    input' <- fromJType input
    let binder' = mkExplicitBinder input' Nothing
    S.Pi mempty binder' <$> fromJType output
  BoolType -> toType B.BoolType []
  RatType -> toType B.RatType []
  TensorType t -> toType B.TensorType [t]
  VectorType t -> toType B.VectorType [t]
  DimensionType -> toType B.NatType []
  DimensionsType -> toType B.ListType [DimensionType]
  DimensionIndexType -> toType B.IndexType []
  TypeVar name spine -> do
    nameCtx <- getNameContext
    let ix = maybe (developerError ("ill-scoped JExpr, no variable" <+> squotes (pretty name))) Ix (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList (S.BoundVar mempty ix) (fmap explicit spine')

toType :: (MonadNameContext m) => BuiltinType -> [JType] -> m (S.Expr Builtin)
toType op = toExpr fromJType (BuiltinType op)

fromJExpr :: (MonadNameContext m) => JExpr -> m (S.Expr Builtin)
fromJExpr = \case
  Lam binder body -> do
    binder' <- fromJBinder binder
    body' <- addNameToContext binder' (fromJExpr body)
    return $ S.Lam mempty binder' body'
  Var name spine -> do
    nameCtx <- getNameContext
    let maybeIx = elemIndex (Just name) nameCtx
    let fun = maybe (S.FreeVar mempty (Identifier userModulePath name)) (S.BoundVar mempty . Ix) maybeIx
    spine' <- traverse fromJExpr spine
    return $ normAppList fun (fmap explicit spine')
  BoolTensor t -> toConstructor (B.BoolTensorLiteral t) []
  BoolNot e -> toFunction B.Not [e]
  BoolAnd e1 e2 -> toFunction B.And [e1, e2]
  BoolOr e1 e2 -> toFunction B.Or [e1, e2]
  BoolImplies e1 e2 -> toFunction B.Implies [e1, e2]
  BoolCompareIndex op e1 e2 -> toFunction (B.CompareIndex op) [e1, e2]
  BoolCompareNat op e1 e2 -> toFunction (B.CompareNat op) [e1, e2]
  BoolCompareRatTensor op pDims rDims e1 e2 -> toFunction (B.CompareRatTensor op) [pDims, rDims, e1, e2]
  BoolReduceAnd xs -> toFunction B.ReduceAndTensor [xs]
  BoolReduceOr xs -> toFunction B.ReduceOrTensor [xs]
  BoolIf c e1 e2 -> toFunction B.If [c, e1, e2]
  RatTensor t -> toConstructor (B.RatTensorLiteral t) []
  NegRatTensor e -> toFunction (B.Neg B.NegRatTensor) [e]
  LogRatTensor e -> toFunction (B.Log B.LogRatTensor) [e]
  ExpRatTensor e -> toFunction (B.Exp B.ExpRatTensor) [e]
  AddRatTensor e1 e2 -> toFunction (B.Add B.AddRatTensor) [e1, e2]
  SubRatTensor e1 e2 -> toFunction (B.Sub B.SubRatTensor) [e1, e2]
  MulRatTensor e1 e2 -> toFunction (B.Mul B.MulRatTensor) [e1, e2]
  DivRatTensor e1 e2 -> toFunction (B.Div B.DivRatTensor) [e1, e2]
  MinRatTensor e1 e2 -> toFunction (B.Min B.MinRatTensor) [e1, e2]
  MaxRatTensor e1 e2 -> toFunction (B.Max B.MaxRatTensor) [e1, e2]
  PowRatTensor e1 e2 -> toFunction (B.Pow B.PowRatTensor) [e1, e2]
  ReduceAddRatTensor xs -> toFunction B.ReduceAddRatTensor [xs]
  ReduceMulRatTensor xs -> toFunction B.ReduceMulRatTensor [xs]
  ReduceMinRatTensor xs -> toFunction B.ReduceMinRatTensor [xs]
  ReduceMaxRatTensor xs -> toFunction B.ReduceMaxRatTensor [xs]
  SearchRatTensor _name dims lower upper lambda -> toFunction B.SearchRatTensor [dims, lower, upper, lambda]
  Dimension d -> toConstructor (B.NatLiteral d) []
  DimensionNil -> toConstructor B.Nil []
  DimensionCons e1 e2 -> toConstructor B.Cons [e1, e2]
  DimensionIndex i -> toConstructor (B.IndexLiteral i) []
  AtTensor xs i -> toFunction B.AtTensor [xs, i]
  ForeachTensor _n fn -> toFunction B.ForeachTensor [fn]
  ConstTensor c ds -> toFunction B.ConstTensor [c, ds]
  StackTensor xs -> toFunction B.StackTensor xs
  Transpose xs -> toFunction B.Transpose [xs]
  VectorLiteral xs -> toConstructor B.VectorLiteral xs
  ForeachVector _n fn -> toFunction B.ForeachVector [fn]
  AtVector xs i -> toFunction B.AtVector [xs, i]

fromJBinder :: (MonadNameContext m) => JBinder -> m (S.Binder Builtin)
fromJBinder (Binder p name typ) = do
  typ' <- fromJType typ
  return $ mkExplicitBinder typ' (Just (p, name))

toExpr :: (MonadNameContext m) => (a -> m (S.Expr Builtin)) -> Builtin -> [a] -> m (S.Expr Builtin)
toExpr f op args = do
  args' <- traverse f args
  return $ normAppList (S.Builtin mempty op) (fmap explicit args')

toConstructor :: (MonadNameContext m) => BuiltinConstructor -> [JExpr] -> m (S.Expr Builtin)
toConstructor op = toExpr fromJExpr (BuiltinConstructor op)

toFunction :: (MonadNameContext m) => BuiltinFunction -> [JExpr] -> m (S.Expr Builtin)
toFunction op = toExpr fromJExpr (BuiltinFunction op)
