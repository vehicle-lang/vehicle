module Vehicle.Data.Builtin.Loss
  ( module Vehicle.Data.Builtin.Loss,
    module Vehicle.Syntax.Builtin.BasicOperations,
  )
where

import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Builtin.Standard.Core qualified as S
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor (Tensor)
import Vehicle.Prelude (Pretty (..), developerError)
import Vehicle.Syntax.Builtin.BasicOperations

--------------------------------------------------------------------------------
-- Builtin datatype

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data LossBuiltinType
  = UnitType
  | IndexType
  | NatType
  | RatType
  | ListType
  | TensorType
  deriving (Eq, Ord, Show)

instance Pretty LossBuiltinType where
  pretty = \case
    UnitType -> "Unit"
    IndexType -> "Index"
    NatType -> "Nat"
    RatType -> "RatElement"
    ListType -> "List"
    TensorType -> "Tensor"

--------------------------------------------------------------------------------
-- Builtin datatype

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data LossBuiltinConstructor
  = Nil
  | Cons
  | UnitLiteral
  | IndexLiteral Int
  | IndexTensorLiteral (Tensor Int)
  | NatLiteral Int
  | NatTensorLiteral (Tensor Int)
  | RatTensorLiteral (Tensor Rational)
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinConstructor where
  pretty = \case
    Nil -> "nil"
    Cons -> "::"
    UnitLiteral -> "()"
    IndexLiteral x -> pretty x
    IndexTensorLiteral x -> pretty x
    NatLiteral x -> pretty x
    NatTensorLiteral x -> pretty x
    RatTensorLiteral x -> pretty x

--------------------------------------------------------------------------------
-- Functions

data LossBuiltinFunction
  = -- Rat operations
    Add AddDomain
  | Mul MulDomain
  | Neg NegDomain
  | Sub SubDomain
  | Div DivDomain
  | Min MinDomain
  | Max MaxDomain
  | PowRat
  | -- Rat tensor operations
    ReduceAddRatTensor
  | ReduceMulRatTensor
  | ReduceMinRatTensor
  | ReduceMaxRatTensor
  | -- Generic tensor operations
    At
  | StackTensor
  | ConstTensor
  | SearchRatTensor
  | MapList
  | FoldList
  deriving (Eq, Ord, Show, Generic)

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty LossBuiltinFunction where
  pretty = \case
    Add dom -> "add" <> pretty dom
    Mul dom -> "mul" <> pretty dom
    Neg dom -> "neg" <> pretty dom
    Sub dom -> "sub" <> pretty dom
    Div dom -> "div" <> pretty dom
    Min dom -> "min" <> pretty dom
    Max dom -> "max" <> pretty dom
    PowRat -> "**"
    ReduceAddRatTensor -> "reduceAddRatTensor"
    ReduceMulRatTensor -> "reduceMulRatTensor"
    ReduceMinRatTensor -> "reduceMinRatTensor"
    ReduceMaxRatTensor -> "reduceMaxRatTensor"
    At -> "!"
    StackTensor {} -> "stack"
    ConstTensor -> "const"
    SearchRatTensor -> "search"
    MapList -> "mapList"
    FoldList -> "foldList"

--------------------------------------------------------------------------------
-- Builtin datatype

-- | The builtin types after translation to loss functions (missing all builtins
-- that involve the Bool type).
data LossBuiltin
  = LossBuiltinFunction LossBuiltinFunction
  | LossBuiltinType LossBuiltinType
  | LossBuiltinConstructor LossBuiltinConstructor
  deriving (Show, Eq, Generic)

instance Pretty LossBuiltin where
  pretty = pretty . show

typeAccessor :: LossBuiltinType -> Accessor LossBuiltin ()
typeAccessor b =
  Access
    { getExpr = \case
        LossBuiltinType b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> LossBuiltinType b
    }

functionAccessor :: LossBuiltinFunction -> Accessor LossBuiltin ()
functionAccessor b =
  Access
    { getExpr = \case
        LossBuiltinFunction b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> LossBuiltinFunction b
    }

instance BuiltinHasIndexLiterals LossBuiltin where
  accessIndexLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (IndexLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . IndexLiteral
      }

  accessIndexTensorLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (IndexTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . IndexTensorLiteral
      }

instance BuiltinHasNatType LossBuiltin where
  accessNatTypeBuiltin = typeAccessor NatType

instance BuiltinHasNatLiterals LossBuiltin where
  accessNatLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (NatLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . NatLiteral
      }

  accessNatTensorLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (NatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . NatTensorLiteral
      }

  accessAddNatBuiltin = functionAccessor (Add AddNat)
  accessMulNatBuiltin = functionAccessor (Mul MulNat)

instance BuiltinHasListLiterals LossBuiltin where
  accessNilBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor Nil -> Just ()
          _ -> Nothing,
        mkExpr = \() -> LossBuiltinConstructor Nil
      }

  accessConsBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor Cons -> Just ()
          _ -> Nothing,
        mkExpr = \() -> LossBuiltinConstructor Cons
      }

  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList

instance BuiltinHasTensors LossBuiltin where
  accessConstTensorBuiltin = functionAccessor ConstTensor
  accessStackTensorBuiltin = functionAccessor StackTensor
  accessAtTensorBuiltin = functionAccessor At

instance BuiltinHasRatLiterals LossBuiltin where
  accessRatTensorLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (RatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . RatTensorLiteral
      }

  accessNegRatTensorBuiltin = functionAccessor $ Neg NegRatTensor
  accessAddRatTensorBuiltin = functionAccessor $ Add AddRatTensor
  accessMulRatTensorBuiltin = functionAccessor $ Mul MulRatTensor
  accessSubRatTensorBuiltin = functionAccessor $ Sub SubRatTensor
  accessDivRatTensorBuiltin = functionAccessor $ Div DivRatTensor
  accessMinRatTensorBuiltin = functionAccessor $ Min MinRatTensor
  accessMaxRatTensorBuiltin = functionAccessor $ Max MaxRatTensor
  accessPowRatTensorBuiltin = functionAccessor PowRat
  accessReduceAddRatBuiltin = functionAccessor ReduceAddRatTensor
  accessReduceMulRatBuiltin = functionAccessor ReduceMulRatTensor
  accessReduceMinRatBuiltin = functionAccessor ReduceMinRatTensor
  accessReduceMaxRatBuiltin = functionAccessor ReduceMaxRatTensor

--------------------------------------------------------------------------------
-- Normalisation

instance HasPrimitives LossBuiltin where
  tensorLiterals =
    [ Wrapper accessNatTensorLiteral,
      Wrapper accessRatTensorLiteral,
      Wrapper accessIndexTensorLiteral
    ]

  tensorOp1s =
    [ (accessNegRatTensor, evalNegRatTensor)
    ]

  tensorOp2s =
    [ (accessAddRatTensor, evalAddRatTensor),
      (accessMulRatTensor, evalMulRatTensor),
      (accessSubRatTensor, evalSubRatTensor),
      (accessDivRatTensor, evalDivRatTensor),
      (accessMinRatTensor, evalMinRatTensor),
      (accessMaxRatTensor, evalMaxRatTensor)
    ]

instance NormalisableBuiltin LossBuiltin where
  evalScheme = \case
    LossBuiltinFunction f -> case f of
      Add AddNat -> Simple evalAddNat
      Mul MulNat -> Simple evalMulNat
      Neg NegRatTensor -> Simple evalNegRatTensor
      Add AddRatTensor -> Simple evalAddRatTensor
      Sub SubRatTensor -> Simple evalSubRatTensor
      Mul MulRatTensor -> Simple evalMulRatTensor
      Div DivRatTensor -> Simple evalDivRatTensor
      Min MinRatTensor -> Simple evalMinRatTensor
      Max MaxRatTensor -> Simple evalMaxRatTensor
      PowRat -> Simple evalPowRat
      ReduceAddRatTensor -> Simple evalReduceAddRatTensor
      ReduceMulRatTensor -> Simple evalReduceMulRatTensor
      ReduceMinRatTensor -> Simple evalReduceMinRatTensor
      ReduceMaxRatTensor -> Simple evalReduceMaxRatTensor
      At -> Simple evalAt
      StackTensor -> Simple evalStackTensor
      ConstTensor -> Simple evalConstTensor
      FoldList -> NonSimple evalFoldList
      MapList -> NonSimple evalMapList
      SearchRatTensor {} -> None
    _ -> None

  blockingArgs = developerError "Blocking arguments not yet implemented for LossBuiltin"

  isTypeClassOp _ = False

  isCast _ _ = Nothing

--------------------------------------------------------------------------------
-- Printing

instance ConvertableBuiltin LossBuiltinType Builtin where
  convertBuiltin p =
    convertBuiltin p . \case
      UnitType -> S.UnitType
      IndexType -> S.IndexType
      NatType -> S.NatType
      RatType -> S.RatType
      ListType -> S.ListType
      TensorType -> S.TensorType

instance ConvertableBuiltin LossBuiltinConstructor Builtin where
  convertBuiltin p =
    convertBuiltin p . \case
      Nil -> S.Nil
      Cons -> S.Cons
      UnitLiteral -> S.UnitLiteral
      IndexLiteral x -> S.IndexLiteral x
      IndexTensorLiteral x -> S.IndexTensorLiteral x
      NatLiteral x -> S.NatLiteral x
      NatTensorLiteral x -> S.NatTensorLiteral x
      RatTensorLiteral x -> S.RatTensorLiteral x

instance ConvertableBuiltin LossBuiltinFunction Builtin where
  convertBuiltin p b = case b of
    Neg dom -> convertBuiltin p (Neg dom)
    Sub dom -> convertBuiltin p (Sub dom)
    Div dom -> convertBuiltin p (Div dom)
    Min dom -> convertBuiltin p (Min dom)
    Max dom -> convertBuiltin p (Max dom)
    Add dom -> convertBuiltin p (Add dom)
    Mul dom -> convertBuiltin p (Mul dom)
    PowRat -> convertBuiltin p PowRat
    ReduceAddRatTensor -> convertBuiltin p ReduceAddRatTensor
    ReduceMulRatTensor -> convertBuiltin p ReduceMulRatTensor
    ReduceMinRatTensor -> convertBuiltin p ReduceMinRatTensor
    ReduceMaxRatTensor -> convertBuiltin p ReduceMaxRatTensor
    At -> convertBuiltin p At
    StackTensor -> convertBuiltin p StackTensor
    ConstTensor -> convertBuiltin p ConstTensor
    SearchRatTensor -> cheatConvertBuiltin p $ pretty b
    MapList -> convertBuiltin p MapList
    FoldList -> convertBuiltin p FoldList

instance ConvertableBuiltin LossBuiltin Builtin where
  convertBuiltin p b = case b of
    LossBuiltinType op -> convertBuiltin p op
    LossBuiltinConstructor op -> convertBuiltin p op
    LossBuiltinFunction op -> convertBuiltin p op

instance PrintableBuiltin LossBuiltin where
  coercionArgs _ = Nothing
