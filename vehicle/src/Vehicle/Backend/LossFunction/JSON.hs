{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.JSON
  ( convertToJSONProg,
    convertFromJSONProg,
  )
where

import Data.Aeson (KeyValue (..), ToJSON (..), genericToJSON)
import Data.Aeson.Types (object)
import Data.List (elemIndex)
import Data.Ratio (Ratio, denominator, numerator, (%))
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Compile.Arity
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Prelude (Doc, HasProvenance (..), Ix (..), ModulePath (..), Name, Position, Provenance (..), Range (..), filterOutNonExplicitArgs, getBinderName, mkExplicitBinder, normAppList)
import Vehicle.Compile.Prelude qualified as S (Binder, Decl, Expr (..), GenericDecl (..), GenericProg (..), Prog)
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCodeFromProg)
import Vehicle.Data.Builtin.Loss (LossBuiltin (..), LossBuiltinConstructor, LossBuiltinFunction, LossBuiltinType)
import Vehicle.Data.Builtin.Loss qualified as L
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, mapTensor)
import Vehicle.Prelude (Annotation (..), GenericArg (..), HasName (..), HasType (..), Identifier (..), Position (..), explicit, indent, jsonOptions, line, resolutionError, squotes)
import Vehicle.Prelude.Logging.Class
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Public method
--------------------------------------------------------------------------------

convertToJSONProg :: (MonadCompile m) => S.Prog LossBuiltin -> m JProg
convertToJSONProg prog =
  logCompilerPass MinDetail currentPass $ do
    relevantProg <- removeIrrelevantCodeFromProg prog
    runFreshNameContextT $ convertProg relevantProg

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
  deriving (Generic)

data JBinder
  = Binder Provenance Name JType
  deriving (Show, Generic)

data JType
  = Pi JType JType
  | RatType
  | TensorType JType
  | DimensionType
  | DimensionsType
  | DimensionIndexType
  | TypeVar Name [JExpr]
  deriving (Show, Generic)

data JExpr
  = -- Types
    Lam JBinder JExpr
  | Var Name [JExpr]
  | -- Rational tensors
    RatTensor (Tensor Rat)
  | NegRatTensor JExpr
  | AddRatTensor JExpr JExpr
  | SubRatTensor JExpr JExpr
  | MulRatTensor JExpr JExpr
  | DivRatTensor JExpr JExpr
  | MinRatTensor JExpr JExpr
  | MaxRatTensor JExpr JExpr
  | ReduceAddRatTensor JExpr JExpr
  | ReduceMulRatTensor JExpr JExpr
  | ReduceMinRatTensor JExpr JExpr
  | ReduceMaxRatTensor JExpr JExpr
  | SearchRatTensor JExpr JExpr JExpr JExpr -- (ReductionOp, LowerBound, UpperBound, SearchLambda)
  -- Dimensions
  | Dimension Int
  | DimensionNil
  | DimensionCons JExpr JExpr
  | DimensionIndex Int
  | DimensionLookup JExpr JExpr
  | ConstTensor JExpr JExpr
  | StackTensor [JExpr]
  deriving (Show, Generic)

-- | Tensorflow doesn't support arbitrary precision integers. We should think
-- about this in the more future, about the actual precision the tensor backend
-- can represent rationals in, e.g. Storable.getSize, Haskell int64, etc.
type Rat = Ratio Int

mapRatio :: (Integral b) => (a -> b) -> Ratio a -> Ratio b
mapRatio f r = do
  let num = f $ numerator r
  let denom = f $ denominator r
  num % denom

toRat :: Rational -> Rat
toRat = mapRatio toInt
  where
    toInt x
      | x < toInteger (minBound :: Int) = developerError $ "Underflow converting" <+> pretty x <+> "to `Int`"
      | x > toInteger (maxBound :: Int) = developerError $ "Overflow converting" <+> pretty x <+> "to `Int`"
      | otherwise = fromInteger x

fromRat :: Rat -> Rational
fromRat = mapRatio toInteger

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

instance ToJSON Position where
  toJSON = genericToJSON jsonOptions

instance ToJSON Provenance where
  toJSON (Provenance (Range start end) _) =
    object
      [ "tag" .= toJSON @String "Provenance",
        "contents" .= toJSON @[Int] [posLine start, posColumn start, posLine end, posColumn end]
      ]

--------------------------------------------------------------------------------
-- Conversion to JExpr
--------------------------------------------------------------------------------

currentPass :: Doc a
currentPass = "conversion to JSON"

type MonadJSON m =
  ( MonadCompile m,
    MonadNameContext m
  )

unsupportedError :: (Pretty a) => a -> b
unsupportedError b = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

dependentTypesError :: (Pretty a) => a -> b
dependentTypesError b = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

--------------------------------------------------------------------------------
-- Programs and declarations

convertProg :: (MonadJSON m) => S.Prog LossBuiltin -> m JProg
convertProg (S.Main decls) = Main <$> traverse convertDecl decls

convertDecl :: (MonadJSON m) => S.Decl LossBuiltin -> m JDecl
convertDecl = \case
  S.DefAbstract {} -> compilerDeveloperError "Found abstract definition when converting to JSON"
  S.DefRecord {} -> compilerDeveloperError "Found record when converting to JSON"
  S.DefFunction p ident _ typ body -> do
    typ' <- convertType mempty typ
    expr' <- convertExpr mempty body
    return $ DefFunction p (nameOf ident) typ' expr'

--------------------------------------------------------------------------------
-- Types

convertType :: (MonadJSON m) => BoundEnv LossBuiltin -> S.Expr LossBuiltin -> m JType
convertType env body = convertTypeValue =<< eval mempty env body

convertTypeValue :: (MonadJSON m) => VType LossBuiltin -> m JType
convertTypeValue expr = do
  showEntry expr
  result <- case expr of
    VMeta {} -> resolutionError currentPass "VMeta"
    VFreeVar {} -> resolutionError currentPass "VFreeVar"
    VUniverse {} -> resolutionError currentPass "Universe"
    VRecord {} -> resolutionError currentPass "VRecord"
    VRecordAcc {} -> resolutionError currentPass "VRecordAcc"
    VLam {} -> dependentTypesError ("VLam" :: String)
    VPi binder closure -> do
      typ' <- convertTypeValue (typeOf binder)
      closure' <- convertClosure convertType binder closure
      return $ Pi typ' closure'
    VBuiltin b spine ->
      convertBuiltinType b $ filterOutNonExplicitArgs spine
    VBoundVar v spine -> do
      name <- lvToProperName mempty v
      spine' <- traverse (convertValue . argExpr) spine
      return $ TypeVar name spine'
  showExit result
  return result

convertBuiltinType :: (MonadJSON m) => LossBuiltin -> [Value LossBuiltin] -> m JType
convertBuiltinType b spine = case b of
  LossBuiltinType op -> case op of
    L.UnitType -> unsupportedError b
    L.IndexType -> convertNullaryOp b DimensionIndexType spine
    L.NatType -> convertNullaryOp b DimensionType spine
    L.RatType -> convertNullaryOp b RatType spine
    L.ListType -> convertNullaryOp b DimensionsType spine
    L.TensorType -> convertUnaryOp convertTypeValue b TensorType spine
  _ -> dependentTypesError b

--------------------------------------------------------------------------------
-- Expressions

convertExpr :: (MonadJSON m) => BoundEnv LossBuiltin -> S.Expr LossBuiltin -> m JExpr
convertExpr env body = convertValue =<< eval mempty env body

convertValue :: (MonadJSON m) => Value LossBuiltin -> m JExpr
convertValue expr = do
  showEntry expr
  result <- case expr of
    VMeta {} -> resolutionError currentPass "VMeta"
    VFreeVar {} -> resolutionError currentPass "VFreeVar"
    VUniverse {} -> resolutionError currentPass "Universe"
    VRecord {} -> resolutionError currentPass "VRecord"
    VRecordAcc {} -> resolutionError currentPass "VRecordAcc"
    VPi {} -> resolutionError currentPass "VPi"
    VLam binder closure -> do
      binder' <- convertBinder binder
      closure' <- convertClosure convertExpr binder closure
      return $ Lam binder' closure'
    VBuiltin b spine -> convertBuiltin b $ filterOutNonExplicitArgs spine
    VBoundVar v spine -> do
      name <- lvToProperName mempty v
      spine' <- traverse (convertValue . argExpr) spine
      return $ Var name spine'
  showExit result
  return result

convertBinder :: (MonadJSON m) => VBinder LossBuiltin -> m JBinder
convertBinder binder = do
  let p = provenanceOf binder
  let name = getBinderName binder
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
  addNameToContext binder $ f newEnv body

convertBuiltin :: (MonadJSON m) => LossBuiltin -> [Value LossBuiltin] -> m JExpr
convertBuiltin b spine = case b of
  LossBuiltinType op -> resolutionError currentPass (pretty op)
  LossBuiltinConstructor op -> case op of
    L.Nil -> convertNullaryOp b DimensionNil spine
    L.Cons -> convertBinaryOp convertValue b DimensionCons spine
    L.UnitLiteral -> unsupportedError b
    L.IndexLiteral i -> convertNullaryOp b (DimensionIndex i) spine
    L.NatLiteral x -> convertNullaryOp b (Dimension x) spine
    L.NatTensorLiteral _ -> unsupportedError b
    L.RatTensorLiteral t -> convertNullaryOp b (RatTensor $ mapTensor toRat t) spine
  LossBuiltinFunction op -> case op of
    L.Add L.AddRatTensor -> convertBinaryOp convertValue b AddRatTensor spine
    L.Mul L.MulRatTensor -> convertBinaryOp convertValue b MulRatTensor spine
    L.Neg L.NegRatTensor -> convertUnaryOp convertValue b NegRatTensor spine
    L.Sub L.SubRatTensor -> convertBinaryOp convertValue b SubRatTensor spine
    L.Div L.DivRatTensor -> convertBinaryOp convertValue b DivRatTensor spine
    L.Min L.MinRatTensor -> convertBinaryOp convertValue b MinRatTensor spine
    L.Max L.MaxRatTensor -> convertBinaryOp convertValue b MaxRatTensor spine
    L.PowRat -> unsupportedError b
    L.ReduceAddRatTensor -> convertBinaryOp convertValue b ReduceAddRatTensor spine
    L.ReduceMulRatTensor -> convertBinaryOp convertValue b ReduceMulRatTensor spine
    L.ReduceMinRatTensor -> convertBinaryOp convertValue b ReduceMinRatTensor spine
    L.ReduceMaxRatTensor -> convertBinaryOp convertValue b ReduceMaxRatTensor spine
    L.At -> convertBinaryOp convertValue b DimensionLookup spine
    L.StackTensor -> convertStackTensor spine
    L.ConstTensor -> convertBinaryOp convertValue b ConstTensor spine
    L.SearchRatTensor -> convertSearch spine
    -- Dimension operations, not yet converted
    L.Add L.AddNat -> unsupportedError b
    L.Mul L.MulNat -> unsupportedError b
    L.MapList -> unsupportedError b
    L.FoldList -> unsupportedError b

convertNullaryOp :: (MonadJSON m) => LossBuiltin -> a -> [Value LossBuiltin] -> m a
convertNullaryOp b fn = \case
  [] -> return fn
  spine -> arityError b 0 spine

convertUnaryOp :: (MonadJSON m) => (Value LossBuiltin -> m a) -> LossBuiltin -> (a -> a) -> [Value LossBuiltin] -> m a
convertUnaryOp convert b fn = \case
  [x] -> fn <$> convert x
  spine -> arityError b 1 spine

convertBinaryOp :: (MonadJSON m) => (Value LossBuiltin -> m a) -> LossBuiltin -> (a -> a -> a) -> [Value LossBuiltin] -> m a
convertBinaryOp convert b fn = \case
  [x, y] -> fn <$> convert x <*> convert y
  spine -> arityError b 2 spine

convertStackTensor :: (MonadJSON m) => [Value LossBuiltin] -> m JExpr
convertStackTensor xss = StackTensor <$> traverse convertValue xss

convertSearch :: (MonadJSON m) => [Value LossBuiltin] -> m JExpr
convertSearch = \case
  [unaryOp, lowerBound, upperBound, fn] -> SearchRatTensor <$> convertValue unaryOp <*> convertValue lowerBound <*> convertValue upperBound <*> convertValue fn
  spine -> arityError (show L.SearchRatTensor) 5 spine

arityError :: (MonadCompile m, Pretty fn) => fn -> Arity -> [Value LossBuiltin] -> m a
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
    runFreshNameContext $ do
      typ' <- fromJType typ
      body' <- fromJExpr body
      let ident = Identifier (ModulePath []) name
      return $ S.DefFunction p ident [AnnProperty] typ' body'

fromJType :: (MonadNameContext m) => JType -> m (S.Expr LossBuiltin)
fromJType = \case
  Pi input output -> do
    input' <- fromJType input
    let binder' = mkExplicitBinder input' Nothing
    S.Pi mempty binder' <$> fromJType output
  RatType -> toType L.RatType []
  TensorType t -> toType L.TensorType [t]
  DimensionType -> toType L.NatType []
  DimensionsType -> toType L.ListType [DimensionType]
  DimensionIndexType -> toType L.IndexType []
  TypeVar name spine -> do
    nameCtx <- getNameContext
    let ix = maybe (developerError ("ill-scoped JExpr, no variable" <+> squotes (pretty name))) Ix (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList (S.BoundVar mempty ix) (fmap explicit spine')

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
  RatTensor t -> toConstructor (L.RatTensorLiteral (mapTensor fromRat t)) []
  NegRatTensor e -> toFunction (L.Neg L.NegRatTensor) [e]
  AddRatTensor e1 e2 -> toFunction (L.Add L.AddRatTensor) [e1, e2]
  SubRatTensor e1 e2 -> toFunction (L.Sub L.SubRatTensor) [e1, e2]
  MulRatTensor e1 e2 -> toFunction (L.Mul L.MulRatTensor) [e1, e2]
  DivRatTensor e1 e2 -> toFunction (L.Div L.DivRatTensor) [e1, e2]
  MinRatTensor e1 e2 -> toFunction (L.Min L.MinRatTensor) [e1, e2]
  MaxRatTensor e1 e2 -> toFunction (L.Max L.MaxRatTensor) [e1, e2]
  ReduceAddRatTensor e xs -> toFunction L.ReduceAddRatTensor [e, xs]
  ReduceMulRatTensor e xs -> toFunction L.ReduceMulRatTensor [e, xs]
  ReduceMinRatTensor e xs -> toFunction L.ReduceMinRatTensor [e, xs]
  ReduceMaxRatTensor e xs -> toFunction L.ReduceMaxRatTensor [e, xs]
  SearchRatTensor e1 e2 e3 e4 -> toFunction L.SearchRatTensor [e1, e2, e3, e4]
  Dimension d -> toConstructor (L.NatLiteral d) []
  DimensionNil -> toConstructor L.Nil []
  DimensionCons e1 e2 -> toConstructor L.Cons [e1, e2]
  DimensionIndex i -> toConstructor (L.IndexLiteral i) []
  DimensionLookup xs i -> toFunction L.At [xs, i]
  ConstTensor c ds -> toFunction L.ConstTensor [c, ds]
  StackTensor xs -> toFunction L.StackTensor xs

fromJBinder :: (MonadNameContext m) => JBinder -> m (S.Binder LossBuiltin)
fromJBinder (Binder _ name typ) = do
  typ' <- fromJType typ
  return $ mkExplicitBinder typ' (Just name)

toExpr :: (MonadNameContext m) => (a -> m (S.Expr LossBuiltin)) -> LossBuiltin -> [a] -> m (S.Expr LossBuiltin)
toExpr f op args = do
  args' <- traverse f args
  return $ normAppList (S.Builtin mempty op) (fmap explicit args')

toConstructor :: (MonadNameContext m) => LossBuiltinConstructor -> [JExpr] -> m (S.Expr LossBuiltin)
toConstructor op = toExpr fromJExpr (LossBuiltinConstructor op)

toFunction :: (MonadNameContext m) => LossBuiltinFunction -> [JExpr] -> m (S.Expr LossBuiltin)
toFunction op = toExpr fromJExpr (LossBuiltinFunction op)
