{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.JSON
  ( convertToJSONProg,
    convertFromJSONProg,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.List (elemIndex)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Compile.Arity
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude (DeclProvenance, Ix (..), getBinderName)
import Vehicle.Compile.Prelude qualified as S (Arg, Binder, Decl, Expr (..), GenericDecl (..), GenericProg (..), Prog)
import Vehicle.Compile.Prelude.Utils (getNamedBinderInfo)
import Vehicle.Compile.Print
import Vehicle.Data.AST.Decl
  ( DefFunctionSort (..),
    FunctionDeclAnnotation (..),
    isAnnotatedAsProperty,
  )
import Vehicle.Data.AST.Expr.Scoped (Type, normAppList)
import Vehicle.Data.AST.Record (FieldName (..))
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard.Core (Builtin (..), BuiltinConstructor, BuiltinFunction, BuiltinType)
import Vehicle.Data.Builtin.Standard.Core qualified as B
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Tensor (ExtendedRatTensor, Tensor)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Prelude (Doc, GenericArg (..), HasName (..), HasType (..), Identifier (..), Name, Provenance, explicit, indent, jsonOptions, line, mkExplicitBinder, resolutionError, stdlibIdentifier, userModulePath)
import Vehicle.Prelude.Error (developerError)
import Vehicle.Prelude.Logging.Class

--------------------------------------------------------------------------------
-- Public method
--------------------------------------------------------------------------------

convertToJSONProg :: (MonadCompile m) => S.Prog Builtin -> m JProg
convertToJSONProg prog =
  logCompilerSection2 MinDetail currentPass $ do
    runFreshNameBoundContextT $
      convertProg prog

convertFromJSONProg :: JProg -> S.Prog Builtin
convertFromJSONProg = fromJProg

--------------------------------------------------------------------------------
-- The AST exported to JSON
--------------------------------------------------------------------------------

newtype JProg
  = Main [JDecl]
  deriving (Generic)

data JDecl
  = DefFunction Provenance Name Bool JType JExpr
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

data JExpr
  = -- Types
    Lam JBinder JExpr
  | Var Name [JExpr]
  | Let JExpr JBinder JExpr
  | Record [(Name, JExpr)]
  | RecordAcc JExpr Name [JExpr]
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
  | WhereTensor JExpr JExpr JExpr
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

instance ToJSON JDecl where
  toJSON = genericToJSON jsonOptions

instance ToJSON JExpr where
  toJSON = genericToJSON jsonOptions

instance ToJSON JType where
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
    MonadNameContext m
  )

unsupportedError :: (MonadJSON m, Pretty a) => a -> m b
unsupportedError a = throwError $ UnsupportedLossOperation (stdlibIdentifier "unknown", mempty) Nothing (pretty a)

dependentTypesError :: (Pretty a) => a -> b
dependentTypesError b = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

--------------------------------------------------------------------------------
-- Programs and declarations

convertProg :: (MonadJSON m) => S.Prog Builtin -> m JProg
convertProg (S.Main decls) = Main <$> convertDecls decls

convertDecls :: (MonadJSON m) => [S.Decl Builtin] -> m [JDecl]
convertDecls = \case
  [] -> return []
  d : ds -> do
    d' <- convertDecl d
    ds' <- convertDecls ds
    return $ maybe ds' (: ds') d'

convertDecl :: (MonadJSON m) => S.Decl Builtin -> m (Maybe JDecl)
convertDecl = \case
  S.DefAbstract {} -> developerError "Found abstract definition when converting to JSON"
  S.DefRecord {} -> return Nothing
  S.DefFunction p ident sort typ body ->
    flip runReaderT (ident, p) $ do
      typ' <- convertTypeValue typ
      expr' <- convertExpr body
      return $ Just $ DefFunction p (nameOf ident) (isAnnotatedAsProperty sort) typ' expr'

--------------------------------------------------------------------------------
-- General

type MonadJSONExpr m =
  ( MonadCompile m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  )

convertBoundVar ::
  (MonadJSONExpr m) =>
  (S.Expr Builtin -> m jvalue1) ->
  (Name -> [jvalue1] -> jvalue2) ->
  Ix ->
  [S.Arg Builtin] ->
  m jvalue2
convertBoundVar convert toVar v args = do
  name <- ixToProperName mempty v
  spine' <- traverse (convert . argExpr) args
  return $ toVar name spine'

convertFreeVar ::
  (MonadJSONExpr m) =>
  (S.Expr Builtin -> m jvalue1) ->
  (Name -> [jvalue1] -> jvalue2) ->
  Identifier ->
  [S.Arg Builtin] ->
  m jvalue2
convertFreeVar convert toVar v args = do
  let name = nameOf v
  spine' <- traverse (convert . argExpr) args
  return $ toVar name spine'

convertRecordAcc ::
  (MonadJSONExpr m) =>
  Type Builtin ->
  S.Expr Builtin ->
  FieldName ->
  [S.Arg Builtin] ->
  m JExpr
convertRecordAcc _typ record field args = do
  record' <- convertExpr record
  spine' <- traverse (convertExpr . argExpr) args
  return $ RecordAcc record' (nameOf field) spine'

--------------------------------------------------------------------------------
-- Types

convertTypeValue :: (MonadJSONExpr m) => Type Builtin -> m JType
convertTypeValue expr = do
  showEntry expr
  result <- case expr of
    S.Universe {} -> resolutionError currentPass "Universe"
    S.Record {} -> resolutionError currentPass "Record"
    S.RecordProj {} -> resolutionError currentPass "RecordProj"
    S.Hole {} -> resolutionError currentPass "Hole"
    S.Meta {} -> resolutionError currentPass "Meta"
    S.Let {} -> dependentTypesError ("Let" :: String)
    S.Lam {} -> dependentTypesError ("Lam" :: String)
    S.Pi _ binder body -> do
      typ' <- convertTypeValue (typeOf binder)
      closure' <- addNameToContext binder $ convertTypeValue body
      return $ Pi typ' closure'
    S.App fun args -> do
      let args' = NonEmpty.toList args
      case fun of
        S.Builtin _ b -> convertBuiltinType b args'
        S.BoundVar _ v -> convertBoundVar convertExpr TypeVar v args'
        S.FreeVar _ v -> convertFreeVar convertExpr TypeVar v args'
        _ -> developerError "Unexpected type application"
    S.Builtin _ b -> convertBuiltinType b []
    S.BoundVar _ v -> convertBoundVar convertExpr TypeVar v []
    S.FreeVar _ v -> convertFreeVar convertExpr TypeVar v []
  showExit result
  return result

convertBuiltinType :: (MonadJSONExpr m) => Builtin -> [S.Arg Builtin] -> m JType
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

convertIndexType :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JType
convertIndexType spine = case spine of
  (fmap argExpr -> [_t]) -> return DimensionIndexType
  _ -> arityError B.IndexType 1 spine

convertTensorType :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JType
convertTensorType spine = case spine of
  (fmap argExpr -> [t, _ds]) -> TensorType <$> convertTypeValue t
  _ -> arityError B.TensorType 2 spine

convertVectorType :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JType
convertVectorType spine = case spine of
  (fmap argExpr -> [t, _d]) -> VectorType <$> convertTypeValue t
  _ -> arityError B.VectorType 2 spine

convertListType :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JType
convertListType spine = case spine of
  (fmap argExpr -> [_t]) -> return DimensionsType
  _ -> arityError B.ListType 1 spine

--------------------------------------------------------------------------------
-- Expressions

convertExpr :: (MonadJSONExpr m) => S.Expr Builtin -> m JExpr
convertExpr expr = do
  showEntry expr
  result <- case expr of
    S.Universe {} -> resolutionError currentPass "Universe"
    S.Pi {} -> resolutionError currentPass "Pi"
    S.Hole {} -> resolutionError currentPass "Pi"
    S.Meta {} -> resolutionError currentPass "Pi"
    S.Let _ bound binder body -> Let <$> convertExpr bound <*> convertBinder binder <*> convertExpr body
    S.Record _ _typ fields -> do
      fields' <- traverse (\(k, v) -> (nameOf k,) <$> convertExpr v) fields
      return $ Record fields'
    S.RecordProj _ typ recordVal field -> convertRecordAcc typ recordVal field []
    S.Lam _ binder body -> do
      binder' <- convertBinder binder
      closure' <- addNameToContext binder $ convertExpr body
      return $ Lam binder' closure'
    S.App fun args -> do
      let args' = NonEmpty.toList args
      case fun of
        S.Builtin _ b -> convertBuiltin b args'
        S.BoundVar _ v -> convertBoundVar convertExpr Var v args'
        S.FreeVar _ v -> convertFreeVar convertExpr Var v args'
        S.RecordProj _ typ recordVal field -> convertRecordAcc typ recordVal field args'
        _ -> do
          funDoc <- prettyFriendlyInCtx fun
          developerError $ "Unexpected expr application:" <+> funDoc
    S.Builtin _ b -> convertBuiltin b []
    S.BoundVar _ v -> convertBoundVar convertExpr Var v []
    S.FreeVar _ v -> convertFreeVar convertExpr Var v []
  showExit result
  return result

convertBinder :: (MonadJSONExpr m) => S.Binder Builtin -> m JBinder
convertBinder binder = do
  let (name, p) = getNamedBinderInfo binder
  typ' <- convertTypeValue (typeOf binder)
  return $ Binder p name typ'

convertBuiltin :: (MonadJSONExpr m) => Builtin -> [S.Arg Builtin] -> m JExpr
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
    B.Transpose -> convertTranspose convertExpr spine
    B.ForeachVector -> convertForeachVector spine
    B.AtVector -> convertAtVector spine
    B.SearchRatTensor -> convertSearch spine
    B.WhereTensor -> convertWhere spine
    -- Dimension operations, not yet converted
    B.Add B.AddNat -> unsupportedError b
    B.Mul B.MulNat -> unsupportedError b
    B.MapList -> unsupportedError b
    B.FoldList -> unsupportedError b
    B.ReverseList -> unsupportedError b
    B.AppendList -> unsupportedError b
    B.Iterate -> unsupportedError b
  _ -> dependentTypesError b

convertNullaryOp :: (MonadJSONExpr m) => Builtin -> a -> [S.Arg Builtin] -> m a
convertNullaryOp b fn = \case
  [] -> return fn
  spine -> arityError b 0 spine

convertNonNullaryOp ::
  (MonadJSONExpr m, IsArgs args, Pretty fn) =>
  fn ->
  Arity ->
  (args (S.Expr Builtin) -> m a) ->
  [S.Arg Builtin] ->
  m a
convertNonNullaryOp op arity f spine =
  case getExpr accessSpine spine of
    Just args -> f args
    Nothing -> arityError op arity spine

convertNil :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertNil = convertNonNullaryOp B.Nil 1 $
  \(NilArgs _t) ->
    return DimensionNil

convertCons :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertCons = convertNonNullaryOp B.Cons 3 $
  \(ConsArgs _t v ds) ->
    DimensionCons <$> convertExpr v <*> convertExpr ds

convertTensorOp1 ::
  (MonadJSONExpr m) =>
  Builtin ->
  (JExpr -> JExpr) ->
  [S.Arg Builtin] ->
  m JExpr
convertTensorOp1 b fn = convertNonNullaryOp b 2 $
  \(TensorOp1Args _ x) ->
    fn <$> convertExpr x

convertTensorOp2 ::
  (MonadJSONExpr m) =>
  Builtin ->
  (JExpr -> JExpr -> JExpr) ->
  [S.Arg Builtin] ->
  m JExpr
convertTensorOp2 b fn = convertNonNullaryOp b 3 $
  \(TensorOp2Args _ x y) ->
    fn <$> convertExpr x <*> convertExpr y

convertTensorReduction ::
  (MonadJSONExpr m) =>
  Builtin ->
  (JExpr -> JExpr) ->
  [S.Arg Builtin] ->
  m JExpr
convertTensorReduction b fn = convertNonNullaryOp b 2 $
  \(TensorReductionArgs _ xs) ->
    fn <$> convertExpr xs

convertAtTensor ::
  (MonadJSONExpr m) =>
  [S.Arg Builtin] ->
  m JExpr
convertAtTensor = convertNonNullaryOp B.AtTensor 5 $
  \(AtTensorArgs _t _d _ds xs i) ->
    AtTensor <$> convertExpr xs <*> convertExpr i

convertStackTensor :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertStackTensor = convertNonNullaryOp B.StackTensor 4 $
  \(StackTensorArgs _t _d _ds xs) ->
    StackTensor <$> traverse convertExpr xs

convertConstTensor :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertConstTensor = convertNonNullaryOp B.ConstTensor 3 $
  \(ConstTensorArgs _t v ds) ->
    ConstTensor <$> convertExpr v <*> convertExpr ds

convertForeachTensor :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertForeachTensor = convertNonNullaryOp B.ForeachTensor 4 $
  \(ForeachTensorArgs _t d _ds fn) ->
    ForeachTensor <$> convertExpr d <*> convertExpr fn

convertVectorLiteral :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertVectorLiteral = convertNonNullaryOp B.VectorLiteral 3 $
  \(VectorLitArgs _t _ xs) ->
    VectorLiteral <$> traverse convertExpr xs

convertAtVector ::
  (MonadJSONExpr m) =>
  [S.Arg Builtin] ->
  m JExpr
convertAtVector = convertNonNullaryOp B.AtVector 4 $
  \(AtVectorArgs _t _d xs i) ->
    AtVector <$> convertExpr xs <*> convertExpr i

convertForeachVector :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertForeachVector = convertNonNullaryOp B.ForeachVector 3 $
  \(ForeachVectorArgs _t d fn) ->
    ForeachVector <$> convertExpr d <*> convertExpr fn

convertIf :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertIf = convertNonNullaryOp B.If 4 $
  \(IfArgs _t c x y) ->
    BoolIf <$> convertExpr c <*> convertExpr x <*> convertExpr y

convertCompareIndex :: (MonadJSONExpr m) => B.ComparisonOp -> [S.Arg Builtin] -> m JExpr
convertCompareIndex op = convertNonNullaryOp (B.CompareIndex op) 4 $
  \(IndexComparisonArgs _n1 _n2 x y) ->
    BoolCompareIndex op <$> convertExpr x <*> convertExpr y

convertCompareNat :: (MonadJSONExpr m) => B.ComparisonOp -> [S.Arg Builtin] -> m JExpr
convertCompareNat op = convertNonNullaryOp (B.CompareNat op) 2 $
  \(Op2Args x y) ->
    BoolCompareNat op <$> convertExpr x <*> convertExpr y

convertCompareRatTensor :: (MonadJSONExpr m) => B.ComparisonOp -> [S.Arg Builtin] -> m JExpr
convertCompareRatTensor op = convertNonNullaryOp (B.CompareRatTensor op) 4 $
  \(TensorComparisonArgs pDims rDims x y) ->
    BoolCompareRatTensor op <$> convertExpr pDims <*> convertExpr rDims <*> convertExpr x <*> convertExpr y

convertTranspose ::
  (MonadJSONExpr m) =>
  (S.Expr Builtin -> m JExpr) ->
  [S.Arg Builtin] ->
  m JExpr
convertTranspose convert spine = case getExpr accessSpine spine of
  Just (TransposeTensorArgs _t _ds xs) -> Transpose <$> convert xs
  Nothing -> arityError B.Transpose 3 spine

convertSearch :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertSearch = convertNonNullaryOp B.SearchRatTensor 4 $
  \(SearchRatTensorArgs dims lowerBound upperBound fn) -> do
    let name = case fn of
          S.Lam _ binder _ -> getBinderName binder
          _ -> developerError "Malformed search operation"

    SearchRatTensor name <$> convertExpr dims <*> convertExpr lowerBound <*> convertExpr upperBound <*> convertExpr fn

convertWhere :: (MonadJSONExpr m) => [S.Arg Builtin] -> m JExpr
convertWhere = convertNonNullaryOp B.SearchRatTensor 3 $
  \(WhereTensorArgs _dims input cond value) -> do
    WhereTensor <$> convertExpr input <*> convertExpr cond <*> convertExpr value

arityError :: (MonadCompile m, Pretty fn) => fn -> Arity -> [S.Arg Builtin] -> m a
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

showEntry :: (MonadJSONExpr m) => S.Expr Builtin -> m ()
showEntry e = do
  logDebug MaxDetail $ "json-enter:" <+> prettyVerbose e
  incrCallDepth

showExit :: (MonadJSONExpr m) => a -> m ()
showExit _e = do
  logDebug MaxDetail "json-exit"
  decrCallDepth

--------------------------------------------------------------------------------
-- Conversion back (for printing purposes)
--------------------------------------------------------------------------------

fromJProg :: JProg -> S.Prog Builtin
fromJProg = \case
  Main decls -> S.Main (fmap fromJDecl decls)

fromJDecl :: JDecl -> S.Decl Builtin
fromJDecl = \case
  DefFunction p name isProperty typ body ->
    runFreshNameBoundContext $ do
      typ' <- fromJType typ
      body' <- fromJExpr body
      let ident = Identifier userModulePath name
      let sort = FunctionDecl 0 (if isProperty then Just AnnProperty else Nothing)
      return $ S.DefFunction p ident sort typ' body'

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
    let var = maybe (S.FreeVar mempty (Identifier userModulePath name)) (S.BoundVar mempty . Ix) (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList var (fmap explicit spine')

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
    let var = case elemIndex (Just name) nameCtx of
          Nothing -> S.FreeVar mempty (Identifier userModulePath name)
          Just ix -> S.BoundVar mempty $ Ix ix
    spine' <- traverse fromJExpr spine
    return $ normAppList var (fmap explicit spine')
  Record fields -> do
    let fakeRecordType = S.FreeVar mempty $ Identifier userModulePath "?"
    fieldExprs <- traverse (\(n, v) -> (FieldName mempty n,) <$> fromJExpr v) fields
    return $ S.Record mempty fakeRecordType fieldExprs
  RecordAcc record fieldName spine -> do
    let fakeRecordType = S.FreeVar mempty $ Identifier userModulePath "?"
    record' <- fromJExpr record
    let fieldIdent = FieldName mempty fieldName
    spine' <- traverse fromJExpr spine
    return $ normAppList (S.RecordProj mempty fakeRecordType record' fieldIdent) $ fmap explicit spine'
  Let bound binder body -> S.Let mempty <$> fromJExpr bound <*> fromJBinder binder <*> fromJExpr body
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
  WhereTensor input cond value -> toFunction B.WhereTensor [input, cond, value]
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
