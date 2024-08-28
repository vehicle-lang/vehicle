module Vehicle.Data.Builtin.Loss.Core where

import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard.Core qualified as V
import Vehicle.Data.Code.Expr (ConvertableBuiltin (..), Expr (..), PrintableBuiltin (..), cheatConvertBuiltin)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Prelude (HasIdentifier (..), Pretty (..))

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
  | Vector Int
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

instance BuiltinHasIndexLiterals LossBuiltin where
  getIndexBuiltinLit e = case e of
    Index n -> Just n
    _ -> Nothing
  mkIndexBuiltinLit = Index

instance BuiltinHasNatLiterals LossBuiltin where
  getNatBuiltinLit e = case e of
    Nat b -> Just b
    _ -> Nothing
  mkNatBuiltinLit = Nat

instance BuiltinHasRatLiterals LossBuiltin where
  getRatBuiltinLit e = case e of
    Rat b -> Just b
    _ -> Nothing
  mkRatBuiltinLit = Rat

instance BuiltinHasListLiterals LossBuiltin where
  isBuiltinNil e = case e of
    NilList -> True
    _ -> False
  mkBuiltinNil = NilList

  isBuiltinCons e = case e of
    ConsList -> True
    _ -> False
  mkBuiltinCons = ConsList

instance BuiltinHasVecLiterals LossBuiltin where
  getVecBuiltinLit e = case e of
    Vector n -> Just n
    _ -> Nothing
  mkVecBuiltinLit = Vector

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
    Vector n -> builtinConstructor (V.LVec n)
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
      builtinConstructor = Builtin p . V.BuiltinConstructor
      builtinFunction = Builtin p . V.BuiltinFunction
      builtinType = Builtin p . V.BuiltinType
      freeVar = FreeVar p . identifierOf

instance PrintableBuiltin LossBuiltin where
  isCoercion = const False
