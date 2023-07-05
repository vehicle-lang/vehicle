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
import Data.List (stripPrefix)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator)
import GHC.Generics (Generic)
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError, resolutionError)
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Prelude (BuiltinConstructor, BuiltinFunction, BuiltinType, DefAbstractSort (..), Doc, developerError, getExplicitArg, pretty, prettyJSONConfig, quotePretty, squotes, (<+>))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Syntax.AST (Name, Position (..), Provenance (..), UniverseLevel, isExplicit)
import Vehicle.Syntax.AST qualified as V

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  (MonadCompile m, ToJBuiltin builtin) =>
  V.Prog Name builtin ->
  m (Doc a)
compileProgToJSON prog = do
  functionalisedProg <- functionaliseResources prog
  jProg <- toJSON <$> toJProg functionalisedProg
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
  = Nil
  | Cons
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
  | MinRat
  | MaxRat
  | PowRat
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
  | At
  | ConsVector
  | Fold V.FoldDomain
  | Indices
  | UnitType
  | BoolType
  | IndexType
  | NatType
  | IntType
  | RatType
  | ListType
  | VectorType
  | Sample Name
  deriving (Generic)

--------------------------------------------------------------------------------
-- Conversion to JBuiltins

class ToJBuiltin builtin where
  toJBuiltin :: builtin -> JBuiltin

instance ToJBuiltin JBuiltin where
  toJBuiltin = id

instance ToJBuiltin BuiltinConstructor where
  toJBuiltin = \case
    V.Nil -> Nil
    V.Cons -> Cons
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
    V.Quantifier V.Forall _dom -> Forall
    V.Quantifier V.Exists _dom -> Exists
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
    V.At -> At
    V.ConsVector -> ConsVector
    V.Fold dom -> Fold dom
    V.Indices -> Indices

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
  V.Ann {} -> resolutionError currentPass "Ann"
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
  V.Pi p binder body
    | isExplicit binder -> Pi p <$> toJBinder binder <*> toJExpr body
    | otherwise -> toJExpr body
  V.Lam p binder body
    | isExplicit binder -> Lam p <$> toJBinder binder <*> toJExpr body
    | otherwise -> toJExpr body
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
