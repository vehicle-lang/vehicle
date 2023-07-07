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
import Data.Hashable (Hashable)
import Data.List (stripPrefix)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import GHC.Generics (Generic)
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError, resolutionError)
import Vehicle.Compile.Prelude (BuiltinConstructor, BuiltinFunction, BuiltinType, DefAbstractSort (..), Doc, developerError, getExplicitArg, pretty, prettyJSONConfig, quotePretty, squotes, (<+>))
import Vehicle.Compile.Print (PrintableBuiltin (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Syntax.AST (Name, Position (..), Provenance (..), UniverseLevel)
import Vehicle.Syntax.AST qualified as V

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  (MonadCompile m, ToJBuiltin builtin, PrintableBuiltin builtin) =>
  V.Prog Name builtin ->
  m (Doc a)
compileProgToJSON prog = do
  jProg <- toJSON <$> toJProg prog
  return $ pretty $ unpack $ encodePretty' prettyJSONConfig jProg

--------------------------------------------------------------------------------
-- Datatype

-- We have a separate datatype for JSON expressions so we
-- can maintain backwards compatability, even when the core
-- Vehicle AST changes.

newtype JProg
  = Main [JDecl]
  deriving (Generic)

data JDecl
  = DefPostulate Provenance Name JExpr
  | DefFunction Provenance Name JExpr JExpr
  deriving (Generic)

data JExpr
  = Universe Provenance Int
  | App Provenance JExpr [JExpr]
  | Pi Provenance JBinder JExpr
  | BuiltinOp Provenance JBuiltin
  | BoundVar Provenance Name
  | FreeVar Provenance Name
  | Let Provenance JExpr JBinder JExpr
  | Lam Provenance JBinder JExpr
  deriving (Generic)

data JBinder = Binder Provenance (Maybe Name) JExpr
  deriving (Generic)

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
  | Eq
  | Ne
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
    Eq -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqRat V.Eq)
    Ne -> V.Builtin p (V.BuiltinFunction $ V.Equals V.EqRat V.Neq)
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
    V.Equals _dom V.Eq -> Eq
    V.Equals _dom V.Neq -> Ne
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

instance ToJSON JProg where
  toJSON = genericToJSON jsonOptions

instance ToJSON JDecl where
  toJSON = genericToJSON jsonOptions

instance ToJSON JExpr where
  toJSON = genericToJSON jsonOptions

instance ToJSON JBinder where
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

toJProg :: (MonadCompile m, ToJBuiltin builtin) => V.Prog Name builtin -> m JProg
toJProg (V.Main ds) = Main <$> traverse toJDecl ds

toJDecl :: (MonadCompile m, ToJBuiltin builtin) => V.Decl Name builtin -> m JDecl
toJDecl d = case d of
  V.DefAbstract p i s t -> do
    case s of
      NetworkDef -> resourceError s
      DatasetDef -> resourceError s
      ParameterDef {} -> resourceError s
      PostulateDef -> DefPostulate p (V.nameOf i) <$> toJExpr t
  V.DefFunction p i _anns t e -> DefFunction p (V.nameOf i) <$> toJExpr t <*> toJExpr e

toJExpr :: (MonadCompile m, ToJBuiltin builtin) => V.Expr Name builtin -> m JExpr
toJExpr = \case
  V.Hole {} -> resolutionError currentPass "Hole"
  V.Meta {} -> resolutionError currentPass "Meta"
  V.Ann _ e _ -> toJExpr e
  V.Universe p (V.UniverseLevel l) -> return $ Universe p l
  V.Builtin p b -> return $ BuiltinOp p $ toJBuiltin b
  V.BoundVar p v -> return $ BoundVar p v
  V.FreeVar p v -> return $ FreeVar p $ V.nameOf v
  V.App p fun args -> do
    fun' <- toJExpr fun
    args' <- traverse toJExpr (mapMaybe getExplicitArg (NonEmpty.toList args))
    return $ case args' of
      [] -> fun'
      (a : as) -> App p fun' (a : as)
  V.Pi p binder body -> Pi p <$> toJBinder binder <*> toJExpr body
  V.Lam p binder body -> Lam p <$> toJBinder binder <*> toJExpr body
  V.Let p bound binder body ->
    Let p <$> toJExpr bound <*> toJBinder binder <*> toJExpr body

toJBinder :: (MonadCompile m, ToJBuiltin builtin) => V.Binder Name builtin -> m JBinder
toJBinder binder = do
  type' <- toJExpr $ V.binderType binder
  let p = V.binderProvenance binder
  let maybeName = case V.namingForm (V.binderDisplayForm binder) of
        V.NameAndType n -> Just n
        V.OnlyName n -> Just n
        V.OnlyType -> Nothing
  return $ Binder p maybeName type'

resourceError :: (MonadCompile m) => DefAbstractSort -> m a
resourceError resourceType =
  compilerDeveloperError $
    "All"
      <+> quotePretty resourceType
      <+> "declarations should have been removed before"
      <+> squotes currentPass
