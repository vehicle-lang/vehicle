module Vehicle.Data.Builtin.Loss.Core where

import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface (ConvertableBuiltin (..), PrintableBuiltin (..))
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Expr.Standard (cheatConvertBuiltin)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdForeachIndex))
import Vehicle.Prelude (Pretty (..))
import Vehicle.Syntax.AST (HasIdentifier (..))
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.Builtin qualified as V

--------------------------------------------------------------------------------
-- Builtin datatype

-- | The builtin types after translation to loss functions (missing all builtins
-- that involve the Bool type).
data LossBuiltin
  = ----------------------
    -- Types operations --
    ----------------------
    IndexType
  | NatType
  | RatType
  | ListType
  | VectorType
  | ------------------
    -- Constructors --
    ------------------
    Index Int
  | Bool Bool
  | Nat Int
  | Rat Rational
  | NilList
  | ConsList
  | Vector
  | ----------------
    -- Operations --
    ----------------
    Neg V.NegDomain
  | Add V.AddDomain
  | Sub V.SubDomain
  | Mul V.MulDomain
  | Div V.DivDomain
  | PowRat
  | MinRat
  | MaxRat
  | FoldList
  | MapList
  | LookupVector
  | FoldVector
  | MapVector
  | ZipWithVector
  | Indices
  | ForeachIndex
  | Search
  deriving (Show, Eq, Generic)

-- instance Hashable TensorBuiltin

instance Pretty LossBuiltin where
  pretty = pretty . show

instance ConvertableBuiltin LossBuiltin V.Builtin where
  convertBuiltin p b = case b of
    IndexType -> builtinType V.Index
    NatType -> builtinType V.Nat
    RatType -> builtinType V.Rat
    VectorType -> builtinType V.Vector
    -- Constructors
    Index i -> builtinConstructor $ V.LIndex i
    Bool vs -> builtinConstructor $ V.LBool vs
    Nat vs -> builtinConstructor $ V.LNat vs
    Rat vs -> builtinConstructor $ V.LRat vs
    NilList -> builtinConstructor V.Nil
    ConsList -> builtinConstructor V.Cons
    Vector -> builtinConstructor (V.LVec (-1))
    -- Numeric operations
    Neg dom -> builtinFunction (V.Neg dom)
    Add dom -> builtinFunction (V.Add dom)
    Sub dom -> builtinFunction (V.Sub dom)
    Mul dom -> builtinFunction (V.Mul dom)
    Div dom -> builtinFunction (V.Div dom)
    PowRat -> builtinFunction V.PowRat
    MinRat -> builtinFunction V.MinRat
    MaxRat -> builtinFunction V.MaxRat
    FoldList -> builtinFunction V.FoldList
    MapList -> builtinFunction V.MapList
    LookupVector -> builtinFunction V.At
    FoldVector -> builtinFunction V.FoldVector
    MapVector -> builtinFunction V.MapVector
    ZipWithVector -> builtinFunction V.ZipWithVector
    Indices -> builtinFunction V.Indices
    ListType -> builtinType V.List
    ForeachIndex -> freeVar StdForeachIndex
    Search {} -> cheatConvertBuiltin mempty "search"
    where
      builtinConstructor = V.Builtin p . V.BuiltinConstructor
      builtinFunction = V.Builtin p . V.BuiltinFunction
      builtinType = V.Builtin p . V.BuiltinType
      freeVar = V.FreeVar p . identifierOf

instance PrintableBuiltin LossBuiltin where
  isCoercion = const False

instance HasIndexLits (Value closure LossBuiltin) where
  getIndexLit e = case e of
    VBuiltin (Index n) [] -> Just (mempty, n)
    _ -> Nothing
  mkIndexLit _p x = VBuiltin (Index x) mempty

instance HasNatLits (Value closure LossBuiltin) where
  getNatLit e = case e of
    VBuiltin (Nat b) [] -> Just (mempty, b)
    _ -> Nothing
  mkNatLit _p x = VBuiltin (Nat x) mempty

instance HasRatLits (Value closure LossBuiltin) where
  getRatLit e = case e of
    VBuiltin (Rat b) [] -> Just (mempty, b)
    _ -> Nothing
  mkRatLit _p x = VBuiltin (Rat x) mempty

instance HasStandardVecLits (Value closure LossBuiltin) where
  mkHomoVector t xs = VBuiltin Vector (t : xs)
  getHomoVector = \case
    VBuiltin Vector (t : xs) -> Just (t, xs)
    _ -> Nothing

instance HasStandardListLits (Value closure LossBuiltin) where
  getNil e = case e of
    VBuiltin NilList [t] -> Just (mempty, t)
    _ -> Nothing
  mkNil t = VBuiltin NilList [t]

  getCons e = case e of
    VBuiltin ConsList [t, x, xs] -> Just (t, x, xs)
    _ -> Nothing
  mkCons t x xs = VBuiltin ConsList [t, x, xs]
