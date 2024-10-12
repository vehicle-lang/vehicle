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
import Vehicle.Compile.Prelude (Decl, Doc, Expr (..), GenericArg (..), Ix (..), ModulePath (..), Name, Position, Prog, Provenance (..), Range (..), filterOutNonExplicitArgs, getBinderName, mkExplicitBinder, normAppList)
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCodeFromProg)
import Vehicle.Data.Builtin.Loss (DimensionDataBuiltin, DimensionTypeBuiltin, LossTensorBuiltin (..), RatTensorBuiltin)
import Vehicle.Data.Builtin.Loss qualified as L
import Vehicle.Data.Builtin.Tensor ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, mapTensor)
import Vehicle.Prelude (Annotation (..), GenericDecl (..), GenericProg (..), HasName (..), HasType (..), Identifier (..), Position (..), explicit, indent, jsonOptions, line, squotes)
import Vehicle.Prelude.Logging.Class
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Public method

convertToJSONProg :: (MonadCompile m) => Prog LossTensorBuiltin -> m JProg
convertToJSONProg prog =
  logCompilerPass MinDetail currentPass $ do
    relevantProg <- removeIrrelevantCodeFromProg prog
    runFreshNameContextT $ convertProg relevantProg

convertFromJSONProg :: JProg -> Prog LossTensorBuiltin
convertFromJSONProg = fromJProg

--------------------------------------------------------------------------------
-- The AST exported to JSON

newtype JProg
  = JProg [JDecl]
  deriving (Generic)

data JDecl
  = JDecl Name JExpr JExpr
  deriving (Generic)

data JExpr
  = -- Types
    RatType
  | TensorType JExpr
  | DimensionType
  | DimensionsType
  | DimensionIndexType
  | Fun JExpr JExpr
  | -- Rational tensors
    Lambda Name JExpr JExpr
  | Var Name [JExpr]
  | RatTensor (Tensor Rat)
  | RatLiteral Rat
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
  | DimensionIndexTensor (Tensor Int)
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

convertProg :: (MonadJSON m) => Prog LossTensorBuiltin -> m JProg
convertProg (Main decls) = JProg <$> traverse convertDecl decls

convertDecl :: (MonadJSON m) => Decl LossTensorBuiltin -> m JDecl
convertDecl = \case
  DefAbstract {} -> compilerDeveloperError "Found abstract definition when converting to JSON"
  DefFunction _ ident _ typ body -> do
    typ' <- convertExpr mempty typ
    expr' <- convertExpr mempty body
    return $ JDecl (nameOf ident) typ' expr'

convertExpr :: (MonadJSON m) => WHNFBoundEnv LossTensorBuiltin -> Expr LossTensorBuiltin -> m JExpr
convertExpr env body = convertValue =<< eval mempty env body

convertValue :: (MonadJSON m) => WHNFValue LossTensorBuiltin -> m JExpr
convertValue expr = do
  showEntry expr
  result <- case expr of
    VMeta {} -> resolutionError currentPass "VMeta"
    VFreeVar {} -> resolutionError currentPass "VFreeVar"
    VUniverse {} -> resolutionError currentPass "Universe"
    VLam binder (WHNFClosure env body) -> do
      let name = getBinderName binder
      typ' <- convertValue (typeOf binder)
      lv <- getBinderDepth
      let newEnv = extendEnvWithBound lv binder env
      body' <- addNameToContext binder $ convertExpr newEnv body
      return $ Lambda name typ' body'
    VPi binder body -> do
      typ' <- convertValue (typeOf binder)
      body' <- addNameToContext binder $ convertValue body
      return $ Fun typ' body'
    VBuiltin b spine -> convertBuiltin b $ filterOutNonExplicitArgs spine
    VBoundVar v spine -> do
      name <- lvToProperName mempty v
      spine' <- traverse (convertValue . argExpr) spine
      return $ Var name spine'
  showExit result
  return result

convertBuiltin :: (MonadJSON m) => LossTensorBuiltin -> [WHNFValue LossTensorBuiltin] -> m JExpr
convertBuiltin b spine = case b of
  LossTensorRat op -> case op of
    L.RatTensor t -> convertNullaryOp b (RatTensor $ mapTensor toRat t) spine
    L.RatType -> convertNullaryOp b RatType spine
    L.RatLiteral r -> convertNullaryOp b (RatLiteral (toRat r)) spine
    L.NegRatTensor -> convertUnaryOp b NegRatTensor spine
    L.AddRatTensor -> convertBinaryOp b AddRatTensor spine
    L.SubRatTensor -> convertBinaryOp b SubRatTensor spine
    L.MulRatTensor -> convertBinaryOp b MulRatTensor spine
    L.DivRatTensor -> convertBinaryOp b DivRatTensor spine
    L.MinRatTensor -> convertBinaryOp b MinRatTensor spine
    L.MaxRatTensor -> convertBinaryOp b MaxRatTensor spine
    L.ReduceAddRatTensor -> convertUnaryOp b ReduceAddRatTensor spine
    L.ReduceMulRatTensor -> convertUnaryOp b ReduceMulRatTensor spine
    L.ReduceMinRatTensor -> convertUnaryOp b ReduceMinRatTensor spine
    L.ReduceMaxRatTensor -> convertUnaryOp b ReduceMaxRatTensor spine
    L.SearchRatTensor -> convertSearch spine
  LossTensorDimData op -> case op of
    L.Dimension n -> convertNullaryOp b (Dimension n) spine
    L.DimensionNil -> convertNullaryOp b DimensionNil spine
    L.DimensionCons -> convertBinaryOp b DimensionCons spine
    L.DimensionIndex n -> convertNullaryOp b (DimensionIndex n) spine
    L.DimensionIndexTensor t -> convertNullaryOp b (DimensionIndexTensor t) spine
    L.DimensionLookup -> convertBinaryOp b DimensionLookup spine
    L.StackTensor n -> convertNaryOp b (n + 2) StackTensor spine
    L.ConstTensor -> convertBinaryOp b ConstTensor spine
  LossTensorDimType op -> case op of
    L.DimensionType -> convertNullaryOp b DimensionType spine
    L.DimensionsType -> convertNullaryOp b DimensionsType spine
    L.DimensionIndexType -> convertIndexType spine
    L.TensorType -> convertTensorType spine

convertNullaryOp :: (MonadJSON m) => LossTensorBuiltin -> JExpr -> [WHNFValue LossTensorBuiltin] -> m JExpr
convertNullaryOp b fn = \case
  [] -> return fn
  spine -> arityError b 0 spine

convertUnaryOp :: (MonadJSON m) => LossTensorBuiltin -> (JExpr -> JExpr) -> [WHNFValue LossTensorBuiltin] -> m JExpr
convertUnaryOp b fn = \case
  [x] -> fn <$> convertValue x
  spine -> arityError b 1 spine

convertBinaryOp :: (MonadJSON m) => LossTensorBuiltin -> (JExpr -> JExpr -> JExpr) -> [WHNFValue LossTensorBuiltin] -> m JExpr
convertBinaryOp b fn = \case
  [x, y] -> fn <$> convertValue x <*> convertValue y
  spine -> arityError b 2 spine

convertNaryOp :: (MonadJSON m) => LossTensorBuiltin -> Int -> ([JExpr] -> JExpr) -> [WHNFValue LossTensorBuiltin] -> m JExpr
convertNaryOp b n fn spine
  | length spine == n = fn <$> traverse convertValue spine
  | otherwise = arityError b n spine

convertTensorType :: (MonadJSON m) => [WHNFValue LossTensorBuiltin] -> m JExpr
convertTensorType = \case
  [tElem, _dims] -> TensorType <$> convertValue tElem
  spine -> arityError (LossTensorDimType L.TensorType) 2 spine

convertIndexType :: (MonadJSON m) => [WHNFValue LossTensorBuiltin] -> m JExpr
convertIndexType = \case
  [_dim] -> return DimensionIndexType
  spine -> arityError (LossTensorDimType L.DimensionIndexType) 1 spine

convertSearch :: (MonadJSON m) => [WHNFValue LossTensorBuiltin] -> m JExpr
convertSearch = \case
  [unaryOp, lowerBound, upperBound, fn] -> SearchRatTensor <$> convertValue unaryOp <*> convertValue lowerBound <*> convertValue upperBound <*> convertValue fn
  spine -> arityError (show L.SearchRatTensor) 5 spine

arityError :: (MonadCompile m, Pretty fn) => fn -> Arity -> [WHNFValue LossTensorBuiltin] -> m a
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

showEntry :: (MonadJSON m) => WHNFValue LossTensorBuiltin -> m ()
showEntry e = do
  logDebug MaxDetail $ "json-enter:" <+> prettyVerbose e
  incrCallDepth

showExit :: (MonadJSON m) => JExpr -> m ()
showExit _e = do
  logDebug MaxDetail "json-exit"
  decrCallDepth

--------------------------------------------------------------------------------
-- Conversion back (for printing purposes)

fromJProg :: JProg -> Prog LossTensorBuiltin
fromJProg = \case
  JProg decls -> Main (fmap fromJDecl decls)

fromJDecl :: JDecl -> Decl LossTensorBuiltin
fromJDecl = \case
  JDecl name typ body ->
    runFreshNameContext $ do
      typ' <- fromJExpr typ
      body' <- fromJExpr body
      let ident = Identifier (ModulePath []) name
      return $ DefFunction mempty ident [AnnProperty] typ' body'

fromJExpr :: (MonadNameContext m) => JExpr -> m (Expr LossTensorBuiltin)
fromJExpr = \case
  Lambda name typ body -> do
    typ' <- fromJExpr typ
    let binder' = mkExplicitBinder typ' (Just name)
    body' <- addNameToContext binder' (fromJExpr body)
    return $ Lam mempty binder' body'
  Fun input output -> do
    input' <- fromJExpr input
    let binder' = mkExplicitBinder input' Nothing
    Pi mempty binder' <$> fromJExpr output
  Var name spine -> do
    nameCtx <- getNameContext
    let ix = maybe (developerError ("ill-scoped JExpr, no variable" <+> squotes (pretty name))) Ix (elemIndex (Just name) nameCtx)
    spine' <- traverse fromJExpr spine
    return $ normAppList (BoundVar mempty ix) (fmap explicit spine')
  RatType -> fromRatOp L.RatType []
  TensorType t -> fromDimType L.TensorType [t]
  DimensionType -> fromDimType L.DimensionType []
  DimensionsType -> fromDimType L.DimensionsType []
  DimensionIndexType -> fromDimType L.DimensionIndexType []
  RatTensor t -> fromRatOp (L.RatTensor (mapTensor fromRat t)) []
  RatLiteral r -> fromRatOp (L.RatLiteral (fromRat r)) []
  NegRatTensor e -> fromRatOp L.NegRatTensor [e]
  AddRatTensor e1 e2 -> fromRatOp L.AddRatTensor [e1, e2]
  SubRatTensor e1 e2 -> fromRatOp L.SubRatTensor [e1, e2]
  MulRatTensor e1 e2 -> fromRatOp L.MulRatTensor [e1, e2]
  DivRatTensor e1 e2 -> fromRatOp L.DivRatTensor [e1, e2]
  MinRatTensor e1 e2 -> fromRatOp L.MinRatTensor [e1, e2]
  MaxRatTensor e1 e2 -> fromRatOp L.MaxRatTensor [e1, e2]
  ReduceAddRatTensor e -> fromRatOp L.ReduceAddRatTensor [e]
  ReduceMulRatTensor e -> fromRatOp L.ReduceMulRatTensor [e]
  ReduceMinRatTensor e -> fromRatOp L.ReduceMinRatTensor [e]
  ReduceMaxRatTensor e -> fromRatOp L.ReduceMaxRatTensor [e]
  SearchRatTensor e1 e2 e3 e4 -> fromRatOp L.SearchRatTensor [e1, e2, e3, e4]
  Dimension d -> fromDimData (L.Dimension d) []
  DimensionNil -> fromDimData L.DimensionNil []
  DimensionCons e1 e2 -> fromDimData L.DimensionCons [e1, e2]
  DimensionIndex i -> fromDimData (L.DimensionIndex i) []
  DimensionIndexTensor t -> fromDimData (L.DimensionIndexTensor t) []
  DimensionLookup xs i -> fromDimData L.DimensionLookup [xs, i]
  ConstTensor c ds -> fromDimData L.ConstTensor [c, ds]
  StackTensor xs -> fromDimData (L.StackTensor (length xs)) xs

fromRatOp :: (MonadNameContext m) => RatTensorBuiltin -> [JExpr] -> m (Expr LossTensorBuiltin)
fromRatOp op xs = IRatTensorOp op . fmap explicit <$> traverse fromJExpr xs

fromDimType :: (MonadNameContext m) => DimensionTypeBuiltin -> [JExpr] -> m (Expr LossTensorBuiltin)
fromDimType op xs = IDimensionTypeOp op . fmap explicit <$> traverse fromJExpr xs

fromDimData :: (MonadNameContext m) => DimensionDataBuiltin -> [JExpr] -> m (Expr LossTensorBuiltin)
fromDimData op xs = IDimensionDataOp op . fmap explicit <$> traverse fromJExpr xs
