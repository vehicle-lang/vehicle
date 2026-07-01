{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.JSON
  ( convertToJSONProg,
    convertFromJSONProg,
    JProg (..),
    JDecl (..),
    JFieldType (..),
  )
where

import Data.Aeson (ToJSON (..), genericToJSON)
import Data.List (elemIndex)
import Data.Map.Ordered qualified as OMap
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Compile.Arity
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval, evalDecl)
import Vehicle.Compile.Prelude (Ix (..))
import Vehicle.Compile.Prelude qualified as S (Binder, Decl, Expr (..), GenericDecl (..), GenericProg (..), Prog, Type)
import Vehicle.Compile.Prelude.Utils (getNamedBinderInfo)
import Vehicle.Compile.Print
import Vehicle.Data.AST.Decl
  ( DefFunctionSort (..),
    DefRecordSort (..),
    FunctionDeclAnnotation (..),
    isAnnotatedAsTensor,
  )
import Vehicle.Data.AST.Expr.Scoped (normAppList)
import Vehicle.Data.AST.Record (FieldName (..))
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss (LossBuiltin (..), LossBuiltinConstructor, LossBuiltinExtraFunction, LossBuiltinFunction, LossBuiltinType)
import Vehicle.Data.Builtin.Loss qualified as L
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (ExtendedRatTensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)
import Vehicle.Prelude (Doc, GenericArg (..), HasName (..), HasType (..), Identifier (..), Name, Provenance, explicit, indent, jsonOptions, line, mkExplicitBinder, resolutionError, squotes, userModulePath)
import Vehicle.Prelude.Error (developerError)
import Vehicle.Prelude.Logging.Class

--------------------------------------------------------------------------------
-- Public method
--------------------------------------------------------------------------------

convertToJSONProg :: (MonadCompile m) => S.Prog LossBuiltin -> m JProg
convertToJSONProg prog =
  logCompilerSection2 MinDetail currentPass $ do
    -- Free context is needed so NBE can resolve refs to earlier decls
    -- (e.g. @tensor record names in later network signatures).
    runFreshFreeContextT (Proxy @LossBuiltin) $
      runFreshNameBoundContextT $
        convertProg prog

convertFromJSONProg :: JProg -> S.Prog LossBuiltin
convertFromJSONProg = fromJProg

--------------------------------------------------------------------------------
-- The AST exported to JSON
--------------------------------------------------------------------------------

newtype JProg
  = Main [JDecl]
  deriving (Generic)

data JDecl
  = DefFunction Provenance Name JType JExpr
  | DefRecordSchema Provenance Name [(Name, JFieldType)]
  deriving (Generic)

data JFieldType
  = JFieldScalarReal
  | JFieldTensorReal [Either Int Name]
  | JFieldRecordRef Name
  deriving (Show, Generic)

data JBinder
  = Binder Provenance Name JType
  deriving (Show, Generic)

data JType
  = Pi JType JType
  | RatType
  | TensorType JType
  | VectorType JType
  | DimensionType
  | DimensionsType
  | DimensionIndexType
  | TypeVar Name [JExpr]
  | RecordType Name -- (Schema)
  deriving (Show, Generic)

data JExpr
  = -- Types
    Lam JBinder JExpr
  | Var Name [JExpr]
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
  | SearchRatTensor Name JExpr JExpr JExpr JExpr JExpr L.LogicDirection -- (Name, ReductionOp, Dims, LowerBound, UpperBound, SearchLambda, Minimise)
  | -- Tensor records
    Record Name [(Name, JExpr)] -- (Schema, Fields)
  | RecordAcc JExpr Name -- (Record, FieldName)
  | -- Vector
    VectorLiteral [JExpr]
  | AtVector JExpr JExpr
  | ForeachVector JExpr JExpr
  | -- Dimensions
    Dimension Int
  | DimensionNil
  | DimensionCons JExpr JExpr
  | DimensionIndex Int
  deriving (Show, Generic)

instance ToJSON JProg where
  toJSON = genericToJSON jsonOptions

instance ToJSON JDecl where
  toJSON = genericToJSON jsonOptions

instance ToJSON JFieldType where
  toJSON = genericToJSON jsonOptions

instance ToJSON JExpr where
  toJSON = genericToJSON jsonOptions

instance ToJSON JType where
  toJSON = genericToJSON jsonOptions

instance ToJSON JBinder where
  toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- Conversion to JExpr
--------------------------------------------------------------------------------

currentPass :: Doc a
currentPass = "conversion to JSON"

type MonadJSON m =
  ( MonadCompile m,
    MonadNameContext m,
    MonadFreeContext LossBuiltin m
  )

unsupportedError :: (Pretty a) => a -> b
unsupportedError b = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

dependentTypesError :: (Pretty a) => a -> b
dependentTypesError b = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

--------------------------------------------------------------------------------
-- Programs and declarations

convertProg :: (MonadJSON m) => S.Prog LossBuiltin -> m JProg
convertProg (S.Main decls) = Main <$> goDecls decls
  where
    goDecls [] = return []
    goDecls (d : ds) = do
      jdecl <- convertDecl d
      normDecl <- evalDecl d
      jdecls <- addDeclEntryToContext normDecl (goDecls ds)
      return (jdecl : jdecls)

convertDecl :: (MonadJSON m) => S.Decl LossBuiltin -> m JDecl
convertDecl = \case
  S.DefAbstract {} -> developerError "Found abstract definition when converting to JSON"
  S.DefRecord p ident anns _telescope fields _ops
    | isAnnotatedAsTensor anns -> do
        fieldSchemas <- traverse convertSchemaField fields
        return $ DefRecordSchema p (nameOf ident) fieldSchemas
    | otherwise -> developerError "Non-@tensor record reached JSON emission; Loss.hs:71 should have dropped it"
  S.DefFunction p ident _ typ body -> do
    typ' <- convertType emptyBoundEnv typ
    expr' <- convertExpr emptyBoundEnv body
    return $ DefFunction p (nameOf ident) typ' expr'

convertSchemaField ::
  (MonadJSON m) =>
  (FieldName, S.Type LossBuiltin) ->
  m (Name, JFieldType)
convertSchemaField (fieldName, fieldType) = do
  ft <- convertJFieldType fieldType
  return (nameOf fieldName, ft)

convertJFieldType ::
  (MonadJSON m) =>
  S.Type LossBuiltin ->
  m JFieldType
convertJFieldType typ = do
  typVal <- eval mempty emptyBoundEnv typ
  case typVal of
    VBuiltin (L.LossBuiltinType L.RatType) _ -> return JFieldScalarReal
    -- empty dims means scalar Real (Vehicle desugars `Real` to `Tensor Rat []`).
    VBuiltin (L.LossBuiltinType L.TensorType) (fmap argExpr -> [_elemTyp, dimsValue]) -> do
      dims <- extractDims dimsValue
      return $ case dims of
        [] -> JFieldScalarReal
        _ -> JFieldTensorReal dims
    VFreeVar refIdent _ ->
      return $ JFieldRecordRef (nameOf refIdent)
    _ -> developerError $ "Unsupported @tensor record field type:" <+> prettyVerbose typVal

extractDims ::
  (MonadJSON m) =>
  Value LossBuiltin ->
  m [Either Int Name]
extractDims = go
  where
    go = \case
      VBuiltin (L.LossBuiltinConstructor L.Nil) _ -> return []
      VBuiltin (L.LossBuiltinConstructor L.Cons) (fmap argExpr -> [_, d, rest]) -> do
        dim <- extractOneDim d
        ds <- go rest
        return (dim : ds)
      v -> developerError $ "Unexpected dims spine in @tensor record field:" <+> prettyVerbose v

extractOneDim ::
  (MonadJSON m) =>
  Value LossBuiltin ->
  m (Either Int Name)
extractOneDim = \case
  VBuiltin (L.LossBuiltinConstructor (L.NatLiteral n)) _ -> return $ Left n
  VFreeVar ident _ -> return $ Right (nameOf ident)
  VBoundVar _ _ -> return $ Right "_"
  v -> developerError $ "Unexpected dimension expression in @tensor record field:" <+> prettyVerbose v

--------------------------------------------------------------------------------
-- Types

convertType :: (MonadJSON m) => BoundEnv LossBuiltin -> S.Expr LossBuiltin -> m JType
convertType env body = convertTypeValue =<< eval mempty env body

convertTypeValue :: (MonadJSON m) => VType LossBuiltin -> m JType
convertTypeValue expr = do
  showEntry expr
  result <- case expr of
    VMeta {} -> resolutionError currentPass "VMeta"
    VFreeVar ident _ -> return $ RecordType (nameOf ident)
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

convertBuiltinType :: (MonadJSON m) => LossBuiltin -> Spine LossBuiltin -> m JType
convertBuiltinType b spine = case b of
  LossBuiltinType op -> case op of
    L.UnitType -> unsupportedError b
    L.IndexType -> convertIndexType spine
    L.NatType -> convertNullaryOp b DimensionType spine
    L.RatType -> convertNullaryOp b RatType spine
    L.ListType -> convertListType spine
    L.TensorType -> convertTensorType spine
    L.VectorType -> convertVectorType spine
  _ -> dependentTypesError b

convertIndexType :: (MonadJSON m) => Spine LossBuiltin -> m JType
convertIndexType spine = case spine of
  (fmap argExpr -> [_t]) -> return DimensionIndexType
  _ -> arityError L.IndexType 1 spine

convertTensorType :: (MonadJSON m) => Spine LossBuiltin -> m JType
convertTensorType spine = case spine of
  (fmap argExpr -> [t, _ds]) -> TensorType <$> convertTypeValue t
  _ -> arityError L.TensorType 2 spine

convertVectorType :: (MonadJSON m) => Spine LossBuiltin -> m JType
convertVectorType spine = case spine of
  (fmap argExpr -> [t, _d]) -> VectorType <$> convertTypeValue t
  _ -> arityError L.VectorType 2 spine

convertListType :: (MonadJSON m) => Spine LossBuiltin -> m JType
convertListType spine = case spine of
  (fmap argExpr -> [_t]) -> return DimensionsType
  _ -> arityError L.ListType 1 spine

--------------------------------------------------------------------------------
-- Expressions

convertExpr :: (MonadJSON m) => BoundEnv LossBuiltin -> S.Expr LossBuiltin -> m JExpr
convertExpr env body = do
  normBody <- eval mempty env body
  debugFriendly normBody
  convertValue normBody

convertValue :: (MonadJSON m) => Value LossBuiltin -> m JExpr
convertValue expr = do
  showEntry expr
  result <- case expr of
    VMeta {} -> resolutionError currentPass "VMeta"
    VFreeVar {} -> resolutionError currentPass "VFreeVar"
    VUniverse {} -> resolutionError currentPass "Universe"
    VRecord typ fields -> do
      let schemaName = recordSchemaFromTypeMarker typ
      fields' <- traverse (\(k, v) -> (nameOf k,) <$> convertValue v) (OMap.assocs fields)
      return $ Record schemaName fields'
    VRecordAcc _typ recordVal field _spine -> do
      record' <- convertValue recordVal
      return $ RecordAcc record' (nameOf field)
    VPi {} -> resolutionError currentPass "VPi"
    VLam binder closure -> do
      binder' <- convertBinder binder
      closure' <- convertClosure convertExpr binder closure
      return $ Lam binder' closure'
    VBuiltin b spine -> convertBuiltin b spine
    VBoundVar v spine -> do
      name <- lvToProperName mempty v
      spine' <- traverse (convertValue . argExpr) spine
      return $ Var name spine'
  showExit result
  return result

recordSchemaFromTypeMarker :: VType LossBuiltin -> Name
recordSchemaFromTypeMarker = \case
  VFreeVar ident _ -> nameOf ident
  v -> developerError $ "Expected FreeVar type on VRecord/VRecordAcc, got:" <+> prettyVerbose v

convertBinder :: (MonadJSON m) => VBinder LossBuiltin -> m JBinder
convertBinder binder = do
  let (name, p) = getNamedBinderInfo binder
  typ' <- convertTypeValue (typeOf binder)
  return $ Binder p name typ'

convertClosure ::
  (MonadJSON m) =>
  (BoundEnv LossBuiltin -> S.Expr LossBuiltin -> m a) ->
  VBinder LossBuiltin ->
  Closure LossBuiltin ->
  m a
convertClosure f binder (Closure env body) = do
  lv <- getBinderDepth
  let newEnv = extendEnvWithBound lv binder env
  addNameToContext binder $ do
    debugFriendly body
    f newEnv body

convertBuiltin :: (MonadJSON m) => LossBuiltin -> Spine LossBuiltin -> m JExpr
convertBuiltin b spine = case b of
  LossBuiltinType op -> resolutionError currentPass (pretty op)
  LossBuiltinConstructor op -> case op of
    L.Nil -> convertNil spine
    L.Cons -> convertCons spine
    L.UnitLiteral -> unsupportedError b
    L.NatTensorLiteral _ -> unsupportedError b
    L.IndexLiteral i -> convertNullaryOp b (DimensionIndex i) []
    L.NatLiteral x -> convertNullaryOp b (Dimension x) spine
    L.RatTensorLiteral t -> convertNullaryOp b (RatTensor t) spine
    L.VectorLiteral -> convertVectorLiteral spine
  LossBuiltinFunction op -> case op of
    L.Neg L.NegRatTensor -> convertTensorOp1 b NegRatTensor spine
    L.Add L.AddRatTensor -> convertTensorOp2 b AddRatTensor spine
    L.Mul L.MulRatTensor -> convertTensorOp2 b MulRatTensor spine
    L.Sub L.SubRatTensor -> convertTensorOp2 b SubRatTensor spine
    L.Div L.DivRatTensor -> convertTensorOp2 b DivRatTensor spine
    L.Min L.MinRatTensor -> convertTensorOp2 b MinRatTensor spine
    L.Max L.MaxRatTensor -> convertTensorOp2 b MaxRatTensor spine
    L.Pow L.PowRatTensor -> convertTensorOp2 b PowRatTensor spine
    L.Log L.LogRatTensor -> convertTensorOp1 b LogRatTensor spine
    L.Exp L.ExpRatTensor -> convertTensorOp1 b ExpRatTensor spine
    L.ReduceAddRatTensor -> convertTensorReduction b ReduceAddRatTensor spine
    L.ReduceMulRatTensor -> convertTensorReduction b ReduceMulRatTensor spine
    L.ReduceMinRatTensor -> convertTensorReduction b ReduceMinRatTensor spine
    L.ReduceMaxRatTensor -> convertTensorReduction b ReduceMaxRatTensor spine
    L.AtTensor -> convertAtTensor spine
    L.ForeachTensor -> convertForeachTensor spine
    L.StackTensor -> convertStackTensor spine
    L.ConstTensor -> convertConstTensor spine
    L.ForeachVector -> convertForeachVector spine
    L.AtVector -> convertAtVector spine
    -- Dimension operations, not yet converted
    L.Add L.AddNat -> unsupportedError b
    L.Mul L.MulNat -> unsupportedError b
    L.MapList -> unsupportedError b
    L.FoldList -> unsupportedError b
  L.LossBuiltinExtraFunction f -> case f of
    L.SearchRatTensor name minimise -> convertSearch name minimise spine

convertNullaryOp :: (MonadJSON m) => LossBuiltin -> a -> Spine LossBuiltin -> m a
convertNullaryOp b fn = \case
  [] -> return fn
  spine -> arityError b 0 spine

convertNonNullaryOp ::
  (MonadJSON m, IsArgs args, Pretty fn) =>
  fn ->
  Arity ->
  (args (Value LossBuiltin) -> m a) ->
  Spine LossBuiltin ->
  m a
convertNonNullaryOp op arity f spine = case getExpr accessSpine spine of
  Just args -> f args
  Nothing -> arityError op arity spine

convertNil :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertNil = convertNonNullaryOp L.Nil 1 $
  \NilArgs {} ->
    return DimensionNil

convertCons :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertCons = convertNonNullaryOp L.Cons 4 $
  \(ConsArgs _t v ds) ->
    DimensionCons <$> convertValue v <*> convertValue ds

convertTensorOp1 ::
  (MonadJSON m) =>
  LossBuiltin ->
  (JExpr -> JExpr) ->
  Spine LossBuiltin ->
  m JExpr
convertTensorOp1 b fn = convertNonNullaryOp b 1 $
  \(TensorOp1Args _ x) ->
    fn <$> convertValue x

convertTensorOp2 ::
  (MonadJSON m) =>
  LossBuiltin ->
  (JExpr -> JExpr -> JExpr) ->
  Spine LossBuiltin ->
  m JExpr
convertTensorOp2 b fn = convertNonNullaryOp b 2 $
  \(TensorOp2Args _ x y) ->
    fn <$> convertValue x <*> convertValue y

convertTensorReduction ::
  (MonadJSON m) =>
  LossBuiltin ->
  (JExpr -> JExpr) ->
  Spine LossBuiltin ->
  m JExpr
convertTensorReduction b fn = convertNonNullaryOp b 1 $
  \(TensorReductionArgs _ xs) ->
    fn <$> convertValue xs

convertAtTensor ::
  (MonadJSON m) =>
  Spine LossBuiltin ->
  m JExpr
convertAtTensor = convertNonNullaryOp L.AtTensor 5 $
  \(AtTensorArgs _t _d _ds xs i) ->
    AtTensor <$> convertValue xs <*> convertValue i

convertStackTensor :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertStackTensor = convertNonNullaryOp L.StackTensor 4 $
  \(StackTensorArgs _t _d _ds xs) ->
    StackTensor <$> traverse convertValue xs

convertConstTensor :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertConstTensor = convertNonNullaryOp L.ConstTensor 4 $
  \(ConstTensorArgs _t v ds) ->
    ConstTensor <$> convertValue v <*> convertValue ds

convertForeachTensor :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertForeachTensor = convertNonNullaryOp L.ForeachTensor 4 $
  \(ForeachTensorArgs _t d _ds fn) ->
    ForeachTensor <$> convertValue d <*> convertValue fn

convertVectorLiteral :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertVectorLiteral = convertNonNullaryOp L.VectorLiteral 4 $
  \(VectorLitArgs _t _ xs) ->
    VectorLiteral <$> traverse convertValue xs

convertAtVector ::
  (MonadJSON m) =>
  Spine LossBuiltin ->
  m JExpr
convertAtVector = convertNonNullaryOp L.AtVector 4 $
  \(AtVectorArgs _t _d xs i) ->
    AtVector <$> convertValue xs <*> convertValue i

convertForeachVector :: (MonadJSON m) => Spine LossBuiltin -> m JExpr
convertForeachVector = convertNonNullaryOp L.ForeachVector 4 $
  \(ForeachVectorArgs _t d fn) ->
    ForeachVector <$> convertValue d <*> convertValue fn

convertSearch :: (MonadJSON m) => Name -> Bool -> Spine LossBuiltin -> m JExpr
convertSearch name minimise = convertNonNullaryOp (L.SearchRatTensor name minimise) 5 $
  \(SearchRatTensorArgs dims unaryOp lowerBound upperBound fn) ->
    SearchRatTensor name <$> convertValue unaryOp <*> convertValue dims <*> convertValue lowerBound <*> convertValue upperBound <*> convertValue fn <*> pure minimise

arityError :: (MonadCompile m, Pretty fn) => fn -> Arity -> Spine LossBuiltin -> m a
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

showEntry :: (MonadJSON m) => Value LossBuiltin -> m ()
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

fromJProg :: JProg -> S.Prog LossBuiltin
fromJProg = \case
  Main decls -> S.Main (fmap fromJDecl decls)

fromJDecl :: JDecl -> S.Decl LossBuiltin
fromJDecl = \case
  DefFunction p name typ body ->
    runFreshNameBoundContext $ do
      typ' <- fromJType typ
      body' <- fromJExpr body
      let ident = Identifier userModulePath name
      let sort = FunctionDecl 0 (Just AnnProperty)
      return $ S.DefFunction p ident sort typ' body'
  DefRecordSchema p name fields ->
    runFreshNameBoundContext $ do
      let ident = Identifier userModulePath name
      ratTyp <- toType L.RatType []
      let recoverField (fname, ft) =
            (FieldName mempty fname, fieldTypeToExpr ratTyp ft)
      return $ S.DefRecord p ident (Just AnnTensor) [] (fmap recoverField fields) []
  where
    fieldTypeToExpr ratTyp = \case
      JFieldScalarReal -> ratTyp
      JFieldTensorReal _dims -> ratTyp -- shape detail dropped in friendly print
      JFieldRecordRef _name -> ratTyp -- nested-record detail dropped too

fromJType :: (MonadNameContext m) => JType -> m (S.Expr LossBuiltin)
fromJType = \case
  Pi input output -> do
    input' <- fromJType input
    let binder' = mkExplicitBinder input' Nothing
    S.Pi mempty binder' <$> fromJType output
  RatType -> toType L.RatType []
  TensorType t -> toType L.TensorType [t]
  VectorType t -> toType L.VectorType [t]
  DimensionType -> toType L.NatType []
  DimensionsType -> toType L.ListType [DimensionType]
  DimensionIndexType -> toType L.IndexType []
  TypeVar name spine -> do
    nameCtx <- getNameContext
    let ix = maybe (developerError ("ill-scoped JExpr, no variable" <+> squotes (pretty name))) Ix (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList (S.BoundVar mempty ix) (fmap explicit spine')
  RecordType schemaName -> return $ S.FreeVar mempty (Identifier userModulePath schemaName)

toType :: (MonadNameContext m) => LossBuiltinType -> [JType] -> m (S.Expr LossBuiltin)
toType op = toExpr fromJType (LossBuiltinType op)

fromJExpr :: (MonadNameContext m) => JExpr -> m (S.Expr LossBuiltin)
fromJExpr = \case
  Lam binder body -> do
    binder' <- fromJBinder binder
    body' <- addNameToContext binder' (fromJExpr body)
    return $ S.Lam mempty binder' body'
  Var name spine -> do
    nameCtx <- getNameContext
    let ix = maybe (developerError ("ill-scoped JExpr, no variable" <+> squotes (pretty name))) Ix (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList (S.BoundVar mempty ix) (fmap explicit spine')
  RatTensor t -> toConstructor (L.RatTensorLiteral t) []
  NegRatTensor e -> toFunction (L.Neg L.NegRatTensor) [e]
  LogRatTensor e -> toFunction (L.Log L.LogRatTensor) [e]
  ExpRatTensor e -> toFunction (L.Exp L.ExpRatTensor) [e]
  AddRatTensor e1 e2 -> toFunction (L.Add L.AddRatTensor) [e1, e2]
  SubRatTensor e1 e2 -> toFunction (L.Sub L.SubRatTensor) [e1, e2]
  MulRatTensor e1 e2 -> toFunction (L.Mul L.MulRatTensor) [e1, e2]
  DivRatTensor e1 e2 -> toFunction (L.Div L.DivRatTensor) [e1, e2]
  MinRatTensor e1 e2 -> toFunction (L.Min L.MinRatTensor) [e1, e2]
  MaxRatTensor e1 e2 -> toFunction (L.Max L.MaxRatTensor) [e1, e2]
  PowRatTensor e1 e2 -> toFunction (L.Pow L.PowRatTensor) [e1, e2]
  ReduceAddRatTensor xs -> toFunction L.ReduceAddRatTensor [xs]
  ReduceMulRatTensor xs -> toFunction L.ReduceMulRatTensor [xs]
  ReduceMinRatTensor xs -> toFunction L.ReduceMinRatTensor [xs]
  ReduceMaxRatTensor xs -> toFunction L.ReduceMaxRatTensor [xs]
  SearchRatTensor name dims e1 e2 e3 e4 minimise -> toExtraFunction (L.SearchRatTensor name minimise) [dims, e1, e2, e3, e4]
  Record schemaName fields -> do
    let schemaIdent = Identifier userModulePath schemaName
    fieldExprs <- traverse (\(_, v) -> fromJExpr v) fields
    return $ normAppList (S.FreeVar mempty schemaIdent) (fmap explicit fieldExprs)
  RecordAcc recordVal fieldName -> do
    record' <- fromJExpr recordVal
    let fieldIdent = Identifier userModulePath fieldName
    return $ normAppList (S.FreeVar mempty fieldIdent) [explicit record']
  Dimension d -> toConstructor (L.NatLiteral d) []
  DimensionNil -> toConstructor L.Nil []
  DimensionCons e1 e2 -> toConstructor L.Cons [e1, e2]
  DimensionIndex i -> toConstructor (L.IndexLiteral i) []
  AtTensor xs i -> toFunction L.AtTensor [xs, i]
  ForeachTensor _n fn -> toFunction L.ForeachTensor [fn]
  ConstTensor c ds -> toFunction L.ConstTensor [c, ds]
  StackTensor xs -> toFunction L.StackTensor xs
  VectorLiteral xs -> toConstructor L.VectorLiteral xs
  ForeachVector _n fn -> toFunction L.ForeachVector [fn]
  AtVector xs i -> toFunction L.AtVector [xs, i]

fromJBinder :: (MonadNameContext m) => JBinder -> m (S.Binder LossBuiltin)
fromJBinder (Binder p name typ) = do
  typ' <- fromJType typ
  return $ mkExplicitBinder typ' (Just (p, name))

toExpr :: (MonadNameContext m) => (a -> m (S.Expr LossBuiltin)) -> LossBuiltin -> [a] -> m (S.Expr LossBuiltin)
toExpr f op args = do
  args' <- traverse f args
  return $ normAppList (S.Builtin mempty op) (fmap explicit args')

toConstructor :: (MonadNameContext m) => LossBuiltinConstructor -> [JExpr] -> m (S.Expr LossBuiltin)
toConstructor op = toExpr fromJExpr (LossBuiltinConstructor op)

toFunction :: (MonadNameContext m) => LossBuiltinFunction -> [JExpr] -> m (S.Expr LossBuiltin)
toFunction op = toExpr fromJExpr (LossBuiltinFunction op)

toExtraFunction :: (MonadNameContext m) => LossBuiltinExtraFunction -> [JExpr] -> m (S.Expr LossBuiltin)
toExtraFunction op = toExpr fromJExpr (LossBuiltinExtraFunction op)
