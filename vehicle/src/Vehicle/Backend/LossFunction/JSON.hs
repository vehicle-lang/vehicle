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

convertToJSONProg :: (MonadCompile m) => S.Prog LossBuiltin -> m JProg
convertToJSONProg prog =
  logCompilerPass MinDetail currentPass $ do
    relevantProg <- removeIrrelevantCodeFromProg prog
    runFreshNameContextT $ convertProg relevantProg

convertFromJSONProg :: JProg -> S.Prog LossBuiltin
convertFromJSONProg = fromJProg

--------------------------------------------------------------------------------
-- The AST exported to JSON

newtype JProg
  = Main [JDecl]
  deriving (Generic)

data JDecl
  = DefFunction Provenance Name JExpr JExpr
  deriving (Generic)

data JBinder
  = Binder Provenance Name JExpr
  deriving (Show, Generic)

data JExpr
  = -- Types
    Pi JExpr JExpr
  | Lam JBinder JExpr
  | Var Name [JExpr]
  | RatType
  | TensorType JExpr
  | DimensionType
  | DimensionsType
  | DimensionIndexType
  | -- Rational tensors
    RatTensor (Tensor Rat)
  | NegRatTensor JExpr
  | AddRatTensor JExpr JExpr
  | SubRatTensor JExpr JExpr
  | MulRatTensor JExpr JExpr
  | DivRatTensor JExpr JExpr
  | MinRatTensor JExpr JExpr
  | MaxRatTensor JExpr JExpr
  | ReduceAddRatTensor JExpr
  | ReduceMulRatTensor JExpr
  | ReduceMinRatTensor JExpr
  | ReduceMaxRatTensor JExpr
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

--------------------------------------------------------------------------------
-- JSON instances

instance ToJSON JProg where
  toJSON = genericToJSON jsonOptions

instance ToJSON JDecl where
  toJSON = genericToJSON jsonOptions

instance ToJSON JExpr where
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
-- Conversion of JExpr to JSON

currentPass :: Doc a
currentPass = "conversion to JSON"

type MonadJSON m =
  ( MonadCompile m,
    MonadNameContext m
  )

convertProg :: (MonadJSON m) => S.Prog LossBuiltin -> m JProg
convertProg (S.Main decls) = Main <$> traverse convertDecl decls

convertDecl :: (MonadJSON m) => S.Decl LossBuiltin -> m JDecl
convertDecl = \case
  S.DefAbstract {} -> compilerDeveloperError "Found abstract definition when converting to JSON"
  S.DefFunction p ident _ typ body -> do
    typ' <- convertExpr mempty typ
    expr' <- convertExpr mempty body
    return $ DefFunction p (nameOf ident) typ' expr'

convertExpr :: (MonadJSON m) => BoundEnv LossBuiltin -> S.Expr LossBuiltin -> m JExpr
convertExpr env body = convertValue =<< eval mempty env body

convertValue :: (MonadJSON m) => Value LossBuiltin -> m JExpr
convertValue expr = do
  showEntry expr
  result <- case expr of
    VMeta {} -> resolutionError currentPass "VMeta"
    VFreeVar {} -> resolutionError currentPass "VFreeVar"
    VUniverse {} -> resolutionError currentPass "Universe"
    VLam binder closure -> do
      binder' <- convertBinder binder
      closure' <- convertClosure binder closure
      return $ Lam binder' closure'
    VPi binder closure -> do
      typ' <- convertValue (typeOf binder)
      closure' <- convertClosure binder closure
      return $ Pi typ' closure'
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
  typ' <- convertValue (typeOf binder)
  return $ Binder p name typ'

convertClosure :: (MonadJSON m) => VBinder LossBuiltin -> Closure LossBuiltin -> m JExpr
convertClosure binder (Closure env body) = do
  lv <- getBinderDepth
  let newEnv = extendEnvWithBound lv binder env
  addNameToContext binder $ convertExpr newEnv body

convertBuiltin :: (MonadJSON m) => LossBuiltin -> [Value LossBuiltin] -> m JExpr
convertBuiltin b spine = case b of
  LossBuiltinType op -> case op of
    L.UnitType -> unsupported
    L.IndexType -> convertIndexType spine
    L.NatType -> convertNullaryOp b DimensionType spine
    L.RatType -> convertNullaryOp b RatType spine
    L.ListType -> convertNullaryOp b DimensionsType spine
    L.TensorType -> convertTensorType spine
  LossBuiltinConstructor op -> case op of
    L.Nil -> convertNullaryOp b DimensionNil spine
    L.Cons -> convertBinaryOp b DimensionCons spine
    L.UnitLiteral -> unsupported
    L.IndexLiteral i -> convertNullaryOp b (DimensionIndex i) spine
    L.NatLiteral x -> convertNullaryOp b (Dimension x) spine
    L.NatTensorLiteral _ -> unsupported
    L.RatTensorLiteral t -> convertNullaryOp b (RatTensor $ mapTensor toRat t) spine
  LossBuiltinFunction op -> case op of
    L.Add L.AddRatTensor -> convertBinaryOp b AddRatTensor spine
    L.Mul L.MulRatTensor -> convertBinaryOp b MulRatTensor spine
    L.Neg L.NegRatTensor -> convertUnaryOp b NegRatTensor spine
    L.Sub L.SubRatTensor -> convertBinaryOp b SubRatTensor spine
    L.Div L.DivRatTensor -> convertBinaryOp b DivRatTensor spine
    L.Min L.MinRatTensor -> convertBinaryOp b MinRatTensor spine
    L.Max L.MaxRatTensor -> convertBinaryOp b MaxRatTensor spine
    L.PowRat -> unsupported
    L.ReduceAddRatTensor -> convertUnaryOp b ReduceAddRatTensor spine
    L.ReduceMulRatTensor -> convertUnaryOp b ReduceMulRatTensor spine
    L.ReduceMinRatTensor -> convertUnaryOp b ReduceMinRatTensor spine
    L.ReduceMaxRatTensor -> convertUnaryOp b ReduceMaxRatTensor spine
    L.At -> convertBinaryOp b DimensionLookup spine
    L.StackTensor -> convertStackTensor spine
    L.ConstTensor -> convertBinaryOp b ConstTensor spine
    L.SearchRatTensor -> convertSearch spine
    -- Dimension operations, not yet converted
    L.Add L.AddNat -> unsupported
    L.Mul L.MulNat -> unsupported
    L.MapList -> unsupported
    L.FoldList -> unsupported
  where
    unsupported = developerError $ "Conversion of" <+> pretty b <+> "is not yet implemented"

convertNullaryOp :: (MonadJSON m) => LossBuiltin -> JExpr -> [Value LossBuiltin] -> m JExpr
convertNullaryOp b fn = \case
  [] -> return fn
  spine -> arityError b 0 spine

convertUnaryOp :: (MonadJSON m) => LossBuiltin -> (JExpr -> JExpr) -> [Value LossBuiltin] -> m JExpr
convertUnaryOp b fn = \case
  [x] -> fn <$> convertValue x
  spine -> arityError b 1 spine

convertBinaryOp :: (MonadJSON m) => LossBuiltin -> (JExpr -> JExpr -> JExpr) -> [Value LossBuiltin] -> m JExpr
convertBinaryOp b fn = \case
  [x, y] -> fn <$> convertValue x <*> convertValue y
  spine -> arityError b 2 spine

convertStackTensor :: (MonadJSON m) => [Value LossBuiltin] -> m JExpr
convertStackTensor xss = StackTensor <$> traverse convertValue xss

convertTensorType :: (MonadJSON m) => [Value LossBuiltin] -> m JExpr
convertTensorType = \case
  [tElem, _dims] -> TensorType <$> convertValue tElem
  spine -> arityError L.TensorType 2 spine

convertIndexType :: (MonadJSON m) => [Value LossBuiltin] -> m JExpr
convertIndexType = \case
  [_dim] -> return DimensionIndexType
  spine -> arityError L.IndexType 1 spine

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

showExit :: (MonadJSON m) => JExpr -> m ()
showExit _e = do
  logDebug MaxDetail "json-exit"
  decrCallDepth

--------------------------------------------------------------------------------
-- Conversion back (for printing purposes)

fromJProg :: JProg -> S.Prog LossBuiltin
fromJProg = \case
  Main decls -> S.Main (fmap fromJDecl decls)

fromJDecl :: JDecl -> S.Decl LossBuiltin
fromJDecl = \case
  DefFunction p name typ body ->
    runFreshNameContext $ do
      typ' <- fromJExpr typ
      body' <- fromJExpr body
      let ident = Identifier (ModulePath []) name
      return $ S.DefFunction p ident [AnnProperty] typ' body'

fromJExpr :: (MonadNameContext m) => JExpr -> m (S.Expr LossBuiltin)
fromJExpr = \case
  Lam binder body -> do
    binder' <- fromJBinder binder
    body' <- addNameToContext binder' (fromJExpr body)
    return $ S.Lam mempty binder' body'
  Pi input output -> do
    input' <- fromJExpr input
    let binder' = mkExplicitBinder input' Nothing
    S.Pi mempty binder' <$> fromJExpr output
  Var name spine -> do
    nameCtx <- getNameContext
    let ix = maybe (developerError ("ill-scoped JExpr, no variable" <+> squotes (pretty name))) Ix (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList (S.BoundVar mempty ix) (fmap explicit spine')
  RatType -> toType L.RatType []
  TensorType t -> toType L.TensorType [t]
  DimensionType -> toType L.NatType []
  DimensionsType -> toType L.ListType [DimensionType]
  DimensionIndexType -> toType L.IndexType []
  RatTensor t -> toConstructor (L.RatTensorLiteral (mapTensor fromRat t)) []
  NegRatTensor e -> toFunction (L.Neg L.NegRatTensor) [e]
  AddRatTensor e1 e2 -> toFunction (L.Add L.AddRatTensor) [e1, e2]
  SubRatTensor e1 e2 -> toFunction (L.Sub L.SubRatTensor) [e1, e2]
  MulRatTensor e1 e2 -> toFunction (L.Mul L.MulRatTensor) [e1, e2]
  DivRatTensor e1 e2 -> toFunction (L.Div L.DivRatTensor) [e1, e2]
  MinRatTensor e1 e2 -> toFunction (L.Min L.MinRatTensor) [e1, e2]
  MaxRatTensor e1 e2 -> toFunction (L.Max L.MaxRatTensor) [e1, e2]
  ReduceAddRatTensor e -> toFunction L.ReduceAddRatTensor [e]
  ReduceMulRatTensor e -> toFunction L.ReduceMulRatTensor [e]
  ReduceMinRatTensor e -> toFunction L.ReduceMinRatTensor [e]
  ReduceMaxRatTensor e -> toFunction L.ReduceMaxRatTensor [e]
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
  typ' <- fromJExpr typ
  return $ mkExplicitBinder typ' (Just name)

toExpr :: (MonadNameContext m) => LossBuiltin -> [JExpr] -> m (S.Expr LossBuiltin)
toExpr op args = do
  args' <- traverse fromJExpr args
  return $ normAppList (S.Builtin mempty op) (fmap explicit args')

toType :: (MonadNameContext m) => LossBuiltinType -> [JExpr] -> m (S.Expr LossBuiltin)
toType op = toExpr (LossBuiltinType op)

toConstructor :: (MonadNameContext m) => LossBuiltinConstructor -> [JExpr] -> m (S.Expr LossBuiltin)
toConstructor op = toExpr (LossBuiltinConstructor op)

toFunction :: (MonadNameContext m) => LossBuiltinFunction -> [JExpr] -> m (S.Expr LossBuiltin)
toFunction op = toExpr (LossBuiltinFunction op)
