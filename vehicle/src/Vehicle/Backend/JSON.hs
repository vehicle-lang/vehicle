{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.JSON
  ( compileProgToJSON,
    JBuiltin (..),
    ToJBuiltin (..),
  )
where

import Data.Aeson (KeyValue (..), Options (..), ToJSON (..), defaultOptions, genericToJSON)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Types (object)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Data (Proxy (..))
import Data.Hashable (Hashable)
import Data.List (stripPrefix)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import GHC.Generics (Generic)
import Vehicle.Compile.Arity (Arity, arityFromVType, builtinArity, vlamArity)
import Vehicle.Compile.Descope (DescopeNamed (..))
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError, illTypedError, resolutionError)
import Vehicle.Compile.Prelude (BuiltinConstructor, BuiltinFunction, BuiltinType, DefAbstractSort (..), Doc, HasType (..), LoggingLevel (..), developerError, foldLamBinders, getExplicitArg, logCompilerPass, logDebug, pretty, prettyJSONConfig, quotePretty, squotes, (<+>))
import Vehicle.Compile.Prelude.MonadContext
import Vehicle.Compile.Print (PrintableBuiltin (..), prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised (GluedExpr (..), Value (..), normalised)
import Vehicle.Expr.Relevant
import Vehicle.Syntax.AST (Name, Position (..), Provenance (..), UniverseLevel)
import Vehicle.Syntax.AST qualified as V

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  forall m a.
  (MonadCompile m) =>
  StandardProg ->
  m (Doc a)
compileProgToJSON prog = do
  logCompilerPass MinDetail currentPass $ do
    jProg <- runContextT @m @StandardBuiltin $ toJProg prog
    let namedProg = descopeNamed jProg
    let json = toJSON namedProg
    return $ pretty $ unpack $ encodePretty' prettyJSONConfig json

--------------------------------------------------------------------------------
-- Datatype

-- We have a separate datatype for JSON expressions so we
-- can maintain backwards compatability, even when the core
-- Vehicle AST changes.

type JProg var = RelProg var JBuiltin

type JDecl var = RelDecl var JBuiltin

type JExpr var = RelExpr var JBuiltin

type JBinder var = RelBinder var JBuiltin

data JBuiltin
  = NilList
  | ConsList
  | Unit
  | Bool Bool
  | Index Int
  | Nat Int
  | Int Int
  | Rat Int Int
  | Vector Int
  | Not
  | And
  | Or
  | Implies
  | Forall
  | Exists
  | If
  | NegInt
  | NegRat
  | AddNat
  | AddInt
  | AddRat
  | SubInt
  | SubRat
  | MulNat
  | MulInt
  | MulRat
  | DivRat
  | PowRat
  | MinRat
  | MaxRat
  | EqIndex
  | EqNat
  | EqInt
  | EqRat
  | NeIndex
  | NeNat
  | NeInt
  | NeRat
  | LeIndex
  | LeNat
  | LeInt
  | LeRat
  | LtIndex
  | LtNat
  | LtInt
  | LtRat
  | GeIndex
  | GeNat
  | GeInt
  | GeRat
  | GtIndex
  | GtNat
  | GtInt
  | GtRat
  | AtVector
  | ConsVector
  | FoldList
  | FoldVector
  | Indices
  | UnitType
  | BoolType
  | IndexType
  | NatType
  | IntType
  | RatType
  | ListType
  | VectorType
  | Sample Name [Name]
  deriving (Show, Eq, Generic)

instance Hashable JBuiltin

instance PrintableBuiltin JBuiltin where
  convertBuiltin :: Provenance -> JBuiltin -> V.Expr var V.Builtin
  convertBuiltin p b = case b of
    NilList -> V.Builtin p (V.Constructor V.Nil)
    ConsList -> V.Builtin p (V.Constructor V.Cons)
    Unit -> V.Builtin p (V.Constructor V.LUnit)
    Bool x -> V.Builtin p (V.Constructor $ V.LBool x)
    Index i -> V.Builtin p (V.Constructor $ V.LIndex i)
    Nat n -> V.Builtin p (V.Constructor $ V.LNat n)
    Int i -> V.Builtin p (V.Constructor $ V.LInt i)
    Rat n d -> V.Builtin p (V.Constructor $ V.LRat (fromIntegral n % fromIntegral d))
    Vector n -> V.Builtin p (V.Constructor $ V.LVec n)
    Not -> V.Builtin p (V.BuiltinFunction V.Not)
    And -> V.Builtin p (V.BuiltinFunction V.And)
    Or -> V.Builtin p (V.BuiltinFunction V.Or)
    Implies -> V.Builtin p (V.BuiltinFunction V.Implies)
    Forall -> V.Builtin p (V.BuiltinFunction (V.Quantifier V.Forall))
    Exists -> V.Builtin p (V.BuiltinFunction (V.Quantifier V.Exists))
    If -> V.Builtin p (V.BuiltinFunction V.If)
    NegInt -> V.Builtin p (V.BuiltinFunction $ V.Neg V.NegInt)
    NegRat -> V.Builtin p (V.BuiltinFunction $ V.Neg V.NegRat)
    AddNat -> V.Builtin p (V.BuiltinFunction $ V.Add V.AddNat)
    AddInt -> V.Builtin p (V.BuiltinFunction $ V.Add V.AddInt)
    AddRat -> V.Builtin p (V.BuiltinFunction $ V.Add V.AddRat)
    SubInt -> V.Builtin p (V.BuiltinFunction $ V.Sub V.SubInt)
    SubRat -> V.Builtin p (V.BuiltinFunction $ V.Sub V.SubRat)
    MulNat -> V.Builtin p (V.BuiltinFunction $ V.Mul V.MulNat)
    MulInt -> V.Builtin p (V.BuiltinFunction $ V.Mul V.MulInt)
    MulRat -> V.Builtin p (V.BuiltinFunction $ V.Mul V.MulRat)
    DivRat -> V.Builtin p (V.BuiltinFunction $ V.Div V.DivRat)
    PowRat -> V.Builtin p (V.BuiltinFunction V.PowRat)
    MinRat -> V.Builtin p (V.BuiltinFunction V.MinRat)
    MaxRat -> V.Builtin p (V.BuiltinFunction V.MaxRat)
    EqIndex -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqIndex V.Eq)
    EqNat -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqNat V.Eq)
    EqInt -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqInt V.Eq)
    EqRat -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqRat V.Eq)
    NeIndex -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqIndex V.Neq)
    NeNat -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqNat V.Neq)
    NeInt -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqInt V.Neq)
    NeRat -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqRat V.Neq)
    LeIndex -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderIndex V.Le)
    LeNat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderNat V.Le)
    LeInt -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderInt V.Le)
    LeRat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderRat V.Le)
    LtIndex -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderIndex V.Lt)
    LtNat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderNat V.Lt)
    LtInt -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderInt V.Lt)
    LtRat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderRat V.Lt)
    GeIndex -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderIndex V.Ge)
    GeNat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderNat V.Ge)
    GeInt -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderInt V.Ge)
    GeRat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderRat V.Ge)
    GtIndex -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderIndex V.Gt)
    GtNat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderNat V.Gt)
    GtInt -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderInt V.Gt)
    GtRat -> V.Builtin p (V.BuiltinFunction $ V.Order V.OrderRat V.Gt)
    AtVector -> V.Builtin p (V.BuiltinFunction V.At)
    ConsVector -> V.Builtin p (V.BuiltinFunction V.ConsVector)
    FoldList -> V.Builtin p (V.BuiltinFunction $ V.Fold V.FoldList)
    FoldVector -> V.Builtin p (V.BuiltinFunction $ V.Fold V.FoldVector)
    Indices -> V.Builtin p (V.BuiltinFunction V.Indices)
    UnitType -> V.Builtin p (V.BuiltinType V.Unit)
    BoolType -> V.Builtin p (V.BuiltinType V.Bool)
    IndexType -> V.Builtin p (V.BuiltinType V.Index)
    NatType -> V.Builtin p (V.BuiltinType V.Nat)
    IntType -> V.Builtin p (V.BuiltinType V.Int)
    RatType -> V.Builtin p (V.BuiltinType V.Rat)
    ListType -> V.Builtin p (V.BuiltinType V.List)
    VectorType -> V.Builtin p (V.BuiltinType V.Vector)
    Sample n _ctx -> V.FreeVar p $ V.Identifier V.StdLib ("Sample[" <> n <> "]")

--------------------------------------------------------------------------------
-- Conversion to JBuiltins

class ToJBuiltin builtin where
  toJBuiltin :: builtin -> JBuiltin

instance ToJBuiltin JBuiltin where
  toJBuiltin = id

instance ToJBuiltin BuiltinConstructor where
  toJBuiltin = \case
    V.Nil -> NilList
    V.Cons -> ConsList
    V.LUnit -> Unit
    V.LBool b -> Bool b
    V.LIndex i -> Index i
    V.LNat n -> Nat n
    V.LInt i -> Int i
    V.LRat r -> toJBuiltin r
    V.LVec n -> Vector n

instance ToJBuiltin Rational where
  toJBuiltin r = Rat (toInt $ numerator r) (toInt $ denominator r)
    where
      toInt :: Integer -> Int
      toInt x
        | x < toInteger (minBound :: Int) = developerError $ "Underflow converting" <+> pretty x <+> "to `Int`"
        | x > toInteger (maxBound :: Int) = developerError $ "Overflow converting" <+> pretty x <+> "to `Int`"
        | otherwise = fromInteger x

instance ToJBuiltin BuiltinFunction where
  toJBuiltin = \case
    V.Not -> Not
    V.And -> And
    V.Or -> Or
    V.Implies -> Implies
    V.Quantifier V.Forall -> Forall
    V.Quantifier V.Exists -> Exists
    V.If -> If
    V.FromNat {} -> developerError "`FromNat` should have been removed after type-checking."
    V.FromRat {} -> developerError "`FromRat` should have been removed after type-checking."
    V.Neg V.NegInt -> NegInt
    V.Neg V.NegRat -> NegRat
    V.Add V.AddNat -> AddNat
    V.Add V.AddInt -> AddInt
    V.Add V.AddRat -> AddRat
    V.Sub V.SubInt -> SubInt
    V.Sub V.SubRat -> SubRat
    V.Mul V.MulNat -> MulNat
    V.Mul V.MulInt -> MulInt
    V.Mul V.MulRat -> MulRat
    V.Div V.DivRat -> DivRat
    V.PowRat -> PowRat
    V.MinRat -> MinRat
    V.MaxRat -> MaxRat
    V.Equals V.EqIndex V.Eq -> EqRat
    V.Equals V.EqNat V.Eq -> EqNat
    V.Equals V.EqInt V.Eq -> EqInt
    V.Equals V.EqRat V.Eq -> EqRat
    V.Equals V.EqIndex V.Neq -> NeRat
    V.Equals V.EqNat V.Neq -> NeNat
    V.Equals V.EqInt V.Neq -> NeInt
    V.Equals V.EqRat V.Neq -> NeRat
    V.Order V.OrderIndex V.Le -> LeIndex
    V.Order V.OrderNat V.Le -> LtNat
    V.Order V.OrderInt V.Le -> GeInt
    V.Order V.OrderRat V.Le -> GtRat
    V.Order V.OrderIndex V.Lt -> LeIndex
    V.Order V.OrderNat V.Lt -> LtNat
    V.Order V.OrderInt V.Lt -> GeInt
    V.Order V.OrderRat V.Lt -> GtRat
    V.Order V.OrderIndex V.Ge -> LeIndex
    V.Order V.OrderNat V.Ge -> LtNat
    V.Order V.OrderInt V.Ge -> GeInt
    V.Order V.OrderRat V.Ge -> GtRat
    V.Order V.OrderIndex V.Gt -> LeIndex
    V.Order V.OrderNat V.Gt -> LtNat
    V.Order V.OrderInt V.Gt -> GeInt
    V.Order V.OrderRat V.Gt -> GtRat
    V.At -> AtVector
    V.ConsVector -> ConsVector
    V.Fold V.FoldList -> FoldList
    V.Fold V.FoldVector -> FoldVector
    V.Indices -> Indices
    V.Sample n ctx -> Sample n ctx

instance ToJBuiltin BuiltinType where
  toJBuiltin = \case
    V.Unit -> UnitType
    V.Bool -> BoolType
    V.Index -> IndexType
    V.Nat -> NatType
    V.Int -> IntType
    V.Rat -> RatType
    V.List -> ListType
    V.Vector -> VectorType

instance ToJBuiltin StandardBuiltin where
  toJBuiltin = \case
    CConstructor x -> toJBuiltin x
    CFunction x -> toJBuiltin x
    CType x -> case x of
      StandardBuiltinType y -> toJBuiltin y
      StandardTypeClass {} -> typeClassesUnresolvedError
      StandardTypeClassOp {} -> typeClassesUnresolvedError

typeClassesUnresolvedError :: a
typeClassesUnresolvedError =
  developerError
    "Type-classes should have been resolved before calling the JSON backend"

--------------------------------------------------------------------------------
-- Conversion of JExpr to JSON

stripConstructorNameOptions :: String -> Options
stripConstructorNameOptions prefix =
  defaultOptions
    { constructorTagModifier = \name -> fromMaybe name (stripPrefix prefix name)
    }

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True
    }

instance (ToJSON var) => ToJSON (JProg var) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON var) => ToJSON (JDecl var) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON var) => ToJSON (JExpr var) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON var) => ToJSON (JBinder var) where
  toJSON = genericToJSON jsonOptions

instance ToJSON JBuiltin where
  toJSON = genericToJSON jsonOptions

instance ToJSON Position where
  toJSON = genericToJSON jsonOptions

instance ToJSON UniverseLevel where
  toJSON = genericToJSON jsonOptions

instance ToJSON Provenance where
  toJSON (V.Provenance (V.Range start end) _) =
    object
      [ "tag" .= toJSON @String "Provenance",
        "contents" .= toJSON @[Int] [posLine start, posColumn start, posLine end, posColumn end]
      ]

instance ToJSON V.EqualityDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Eq"

instance ToJSON V.OrderDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Order"

instance ToJSON V.AddDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Add"

instance ToJSON V.SubDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Sub"

instance ToJSON V.MulDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Mul"

instance ToJSON V.DivDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Div"

instance ToJSON V.NegDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Neg"

instance ToJSON V.FoldDomain where
  toJSON = genericToJSON $ stripConstructorNameOptions "Fold"

--------------------------------------------------------------------------------
-- Conversion Expr to JExpr

currentPass :: Doc a
currentPass = "conversion to JSON"

type MonadJSON m =
  ( MonadCompile m,
    MonadContext StandardBuiltin m
  )

toJProg :: (MonadJSON m) => StandardProg -> m (JProg Ix)
toJProg (V.Main ds) = Main <$> toJDecls ds

toJDecls :: (MonadJSON m) => [StandardDecl] -> m [JDecl Ix]
toJDecls [] = return []
toJDecls (decl : decls) = do
  logCompilerPass MinDetail (currentPass <+> "of" <+> quotePretty (V.identifierOf decl)) $ do
    decl' <- case decl of
      V.DefAbstract p i s t -> do
        case s of
          NetworkDef -> resourceError s
          DatasetDef -> resourceError s
          ParameterDef {} -> resourceError s
          PostulateDef -> DefPostulate p (V.nameOf i) <$> toJExpr t
      V.DefFunction p i _anns t e -> do
        logDebug MaxDetail $ prettyVerbose t
        t' <- toJExpr t
        logDebug MaxDetail $ prettyVerbose e
        e' <- toJExpr e
        return $ DefFunction p (V.nameOf i) t' e'

    decls' <- addDeclToContext decl (toJDecls decls)
    return $ decl' : decls'

toJExpr :: (MonadJSON m) => StandardExpr -> m (JExpr Ix)
toJExpr expr = case expr of
  V.Hole {} -> resolutionError currentPass "Hole"
  V.Meta {} -> resolutionError currentPass "Meta"
  V.Ann _ e _ -> toJExpr e
  V.Universe p (V.UniverseLevel l) -> return $ Universe p l
  V.Builtin p b -> return $ Builtin p $ toJBuiltin b
  V.BoundVar p v -> return $ BoundVar p v
  V.FreeVar p v -> return $ FreeVar p $ V.nameOf v
  V.App p fun args -> do
    fun' <- toJExpr fun
    args' <- traverse toJExpr (mapMaybe getExplicitArg (NonEmpty.toList args))
    arity <- functionArity fun
    return $ case args' of
      [] -> fun'
      _ : _
        | arity == length args -> App p fun' args'
        | otherwise -> PartialApp p arity fun' args'
  V.Pi p binder body -> Pi p <$> toJBinder binder <*> addBinderToContext binder (toJExpr body)
  V.Lam p _ _ -> do
    let (foldedBinders, body) = foldLamBinders expr
    Lam p <$> toJBinders foldedBinders <*> addBindersToContext foldedBinders (toJExpr body)
  V.Let p bound binder body ->
    Let p <$> toJExpr bound <*> toJBinder binder <*> addBinderToContext binder (toJExpr body)

toJBinder :: (MonadJSON m) => StandardBinder -> m (JBinder Ix)
toJBinder binder = do
  type' <- toJExpr $ V.binderType binder
  let p = V.binderProvenance binder
  let maybeName = case V.namingForm (V.binderDisplayForm binder) of
        V.NameAndType n -> Just n
        V.OnlyName n -> Just n
        V.OnlyType -> Nothing
  return $ Binder p maybeName type'

toJBinders :: (MonadJSON m) => [StandardBinder] -> m [JBinder Ix]
toJBinders = \case
  [] -> return []
  (b : bs) -> do
    b' <- toJBinder b
    bs' <- addBinderToContext b $ toJBinders bs
    return $ b' : bs'

functionArity :: (MonadJSON m) => StandardExpr -> m Arity
functionArity fun = do
  normFun <- normalise fun
  case normFun of
    VUniverse {} -> illTypedError currentPass (prettyVerbose normFun)
    VPi {} -> illTypedError currentPass (prettyVerbose normFun)
    VMeta {} -> illTypedError currentPass (prettyVerbose normFun)
    -- Should be no free-variables left, after having appended resources as lambdas
    -- and having normalised.
    VFreeVar v _ -> do
      decl <- getDecl (Proxy @StandardBuiltin) currentPass v
      return $ arityFromVType $ normalised (typeOf decl)
    VBoundVar v _ -> do
      binder <- getBoundVarByLv (Proxy @StandardBuiltin) currentPass v
      arityFromVType <$> normalise (typeOf binder)
    VBuiltin b spine -> return $ builtinArity b - length spine
    VLam {} -> return $ vlamArity normFun

resourceError :: (MonadJSON m) => DefAbstractSort -> m a
resourceError resourceType =
  compilerDeveloperError $
    "All"
      <+> quotePretty resourceType
      <+> "declarations should have been removed before"
      <+> squotes currentPass
