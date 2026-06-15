module Vehicle.Data.Builtin.Loss
  ( module Vehicle.Data.Builtin.Loss,
    module Vehicle.Data.Builtin.Core.BasicOperations,
  )
where

import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Builtin.Standard.Core qualified as S
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (Value)
import Vehicle.Data.Real
import Vehicle.Data.Tensor (Tensor)
import Vehicle.Prelude (Name, Pretty (..), developerError)

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
  | VectorType
  | TensorType
  | RecordType
  deriving (Eq, Ord, Show)

lossToStandardBuiltinType :: LossBuiltinType -> S.BuiltinType
lossToStandardBuiltinType = \case
  UnitType -> S.UnitType
  IndexType -> S.IndexType
  NatType -> S.NatType
  RatType -> S.RatType
  ListType -> S.ListType
  VectorType -> S.VectorType
  TensorType -> S.TensorType
  RecordType -> developerError "RecordType has no surface-conversion target; it only exists as a JSON-side schema marker"

instance Pretty LossBuiltinType where
  pretty = pretty . lossToStandardBuiltinType

--------------------------------------------------------------------------------
-- Builtin datatype

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data LossBuiltinConstructor
  = Nil
  | Cons
  | UnitLiteral
  | IndexLiteral Int
  | NatLiteral Int
  | VectorLiteral
  | NatTensorLiteral (Tensor Int)
  | RatTensorLiteral (Tensor ExtendedRational)
  deriving (Eq, Ord, Show, Generic)

lossToStandardBuiltinConstructor :: LossBuiltinConstructor -> S.BuiltinConstructor
lossToStandardBuiltinConstructor = \case
  Nil -> S.Nil
  Cons -> S.Cons
  UnitLiteral -> S.UnitLiteral
  IndexLiteral x -> S.IndexLiteral x
  NatLiteral x -> S.NatLiteral x
  VectorLiteral -> S.VectorLiteral
  NatTensorLiteral x -> S.NatTensorLiteral x
  RatTensorLiteral x -> S.RatTensorLiteral x

instance Pretty LossBuiltinConstructor where
  pretty = pretty . lossToStandardBuiltinConstructor

--------------------------------------------------------------------------------
-- Functions

-- | Is [[true]] < [[false]] in the logic, i.e. does the
-- loss value need to be minimised?
type LogicDirection = Bool

data LossBuiltinFunction
  = -- Rat operations
    Add AddDomain
  | Mul MulDomain
  | Neg NegDomain
  | Sub SubDomain
  | Div DivDomain
  | Min MinDomain
  | Max MaxDomain
  | Pow PowDomain
  | Log LogDomain
  | Exp ExpDomain
  | -- Rat tensor operations
    ReduceAddRatTensor
  | ReduceMulRatTensor
  | ReduceMinRatTensor
  | ReduceMaxRatTensor
  | -- Generic tensor operations
    AtTensor
  | StackTensor
  | ConstTensor
  | ForeachTensor
  | -- List
    MapList
  | FoldList
  | -- Vector
    ForeachVector
  | AtVector
  deriving (Eq, Ord, Show, Generic)

lossToStandardBuiltinFunction :: LossBuiltinFunction -> S.BuiltinFunction
lossToStandardBuiltinFunction = \case
  Add dom -> S.Add dom
  Mul dom -> S.Mul dom
  Neg dom -> S.Neg dom
  Sub dom -> S.Sub dom
  Div dom -> S.Div dom
  Min dom -> S.Min dom
  Max dom -> S.Max dom
  Pow dom -> S.Pow dom
  Log dom -> S.Log dom
  Exp dom -> S.Exp dom
  ReduceAddRatTensor -> S.ReduceAddRatTensor
  ReduceMulRatTensor -> S.ReduceMulRatTensor
  ReduceMinRatTensor -> S.ReduceMinRatTensor
  ReduceMaxRatTensor -> S.ReduceMaxRatTensor
  AtTensor -> S.AtTensor
  StackTensor {} -> S.StackTensor {}
  ConstTensor -> S.ConstTensor
  ForeachTensor -> S.ForeachTensor
  MapList -> S.MapList
  FoldList -> S.FoldList
  ForeachVector -> S.ForeachVector
  AtVector -> S.AtVector

instance Pretty LossBuiltinFunction where
  pretty = pretty . lossToStandardBuiltinFunction

--------------------------------------------------------------------------------
-- Extra loss builtin functions

data LossBuiltinExtraFunction
  = SearchRatTensor Name LogicDirection
  | SearchRecord Name LogicDirection
  deriving (Show, Eq, Ord, Generic)

instance Pretty LossBuiltinExtraFunction where
  pretty = \case
    SearchRatTensor name _direction -> "search[" <> pretty name <> "]"
    SearchRecord name _direction -> "searchRecord[" <> pretty name <> "]"

--------------------------------------------------------------------------------
-- Builtin datatype

-- | The builtin types after translation to loss functions (missing all builtins
-- that involve the Bool type).
data LossBuiltin
  = LossBuiltinFunction LossBuiltinFunction
  | LossBuiltinType LossBuiltinType
  | LossBuiltinConstructor LossBuiltinConstructor
  | LossBuiltinExtraFunction LossBuiltinExtraFunction
  deriving (Show, Eq, Ord, Generic)

instance Pretty LossBuiltin where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Accessors

zeroArityConstructorAccessor :: LossBuiltinConstructor -> Accessor LossBuiltin ()
zeroArityConstructorAccessor b =
  Access
    { getExpr = \case
        LossBuiltinConstructor b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> LossBuiltinConstructor b
    }

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

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
-- Index

instance BuiltinHasIndexType LossBuiltin where
  accessIndexTypeBuiltin = typeAccessor IndexType

instance BuiltinHasIndexLiterals LossBuiltin where
  accessIndexLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (IndexLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . IndexLiteral
      }

--------------------------------------------------------------------------------
-- Nat

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

--------------------------------------------------------------------------------
-- Rat

instance BuiltinHasRatType LossBuiltin where
  accessRatTypeBuiltin = typeAccessor RatType

instance BuiltinHasRatLiterals LossBuiltin where
  accessRatTensorLitBuiltin =
    Access
      { getExpr = \case
          LossBuiltinConstructor (RatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = LossBuiltinConstructor . RatTensorLiteral
      }

  accessNegRatTensorBuiltin = functionAccessor $ Neg NegRatTensor
  accessLogRatTensorBuiltin = functionAccessor $ Log LogRatTensor
  accessExpRatTensorBuiltin = functionAccessor $ Exp ExpRatTensor
  accessAddRatTensorBuiltin = functionAccessor $ Add AddRatTensor
  accessMulRatTensorBuiltin = functionAccessor $ Mul MulRatTensor
  accessSubRatTensorBuiltin = functionAccessor $ Sub SubRatTensor
  accessDivRatTensorBuiltin = functionAccessor $ Div DivRatTensor
  accessMinRatTensorBuiltin = functionAccessor $ Min MinRatTensor
  accessMaxRatTensorBuiltin = functionAccessor $ Max MaxRatTensor
  accessPowRatTensorBuiltin = functionAccessor $ Pow PowRatTensor
  accessReduceAddRatBuiltin = functionAccessor ReduceAddRatTensor
  accessReduceMulRatBuiltin = functionAccessor ReduceMulRatTensor
  accessReduceMinRatBuiltin = functionAccessor ReduceMinRatTensor
  accessReduceMaxRatBuiltin = functionAccessor ReduceMaxRatTensor

--------------------------------------------------------------------------------
-- List

instance BuiltinHasListType LossBuiltin where
  accessListTypeBuiltin = typeAccessor ListType

instance BuiltinHasListLiterals LossBuiltin where
  accessNilBuiltin = zeroArityConstructorAccessor Nil
  accessConsBuiltin = zeroArityConstructorAccessor Cons
  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList

--------------------------------------------------------------------------------
-- Vector

instance BuiltinHasVectorType LossBuiltin where
  accessVectorTypeBuiltin = typeAccessor VectorType

instance BuiltinHasVectors LossBuiltin where
  accessVecLitBuiltin = zeroArityConstructorAccessor VectorLiteral
  accessAtVectorBuiltin = functionAccessor AtVector

--------------------------------------------------------------------------------
-- Tensor

instance BuiltinHasTensorType LossBuiltin where
  accessTensorTypeBuiltin = typeAccessor TensorType

instance BuiltinHasTensors LossBuiltin where
  accessConstTensorBuiltin = functionAccessor ConstTensor
  accessStackTensorBuiltin = functionAccessor StackTensor
  accessAtTensorBuiltin = functionAccessor AtTensor

instance BuiltinHasForeach LossBuiltin where
  accessForeachTensorBuiltin = functionAccessor ForeachTensor
  accessForeachVectorBuiltin = functionAccessor ForeachVector

--------------------------------------------------------------------------------
-- Normalisation

instance HasTensorLiterals Value LossBuiltin where
  tensorLiterals =
    [ Wrapper accessNatTensorLiteral,
      Wrapper accessRatTensorLiteral
    ]

instance HasLiftableTensorOperations LossBuiltin where
  liftableTensorOp1s =
    [ (getExpr accessNegRatTensor, evalNegRatTensor, IRatType)
    ]

  liftableTensorOp2s =
    [ (getExpr accessAddRatTensor, evalAddRatTensor, IRatType),
      (getExpr accessMulRatTensor, evalMulRatTensor, IRatType),
      (getExpr accessSubRatTensor, evalSubRatTensor, IRatType),
      (getExpr accessDivRatTensor, evalDivRatTensor, IRatType),
      (getExpr accessMinRatTensor, evalMinRatTensor, IRatType),
      (getExpr accessMaxRatTensor, evalMaxRatTensor, IRatType)
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
      Pow PowRatTensor -> Simple evalPowRatTensor
      Log LogRatTensor -> None
      Exp ExpRatTensor -> None
      ReduceAddRatTensor -> Simple evalReduceAddRatTensor
      ReduceMulRatTensor -> Simple evalReduceMulRatTensor
      ReduceMinRatTensor -> Simple evalReduceMinRatTensor
      ReduceMaxRatTensor -> Simple evalReduceMaxRatTensor
      AtTensor -> NonSimple evalAtTensor
      StackTensor -> Simple evalStackTensor
      ConstTensor -> Simple evalConstTensor
      FoldList -> NonSimple evalFoldList
      MapList -> NonSimple evalMapList
      ForeachTensor -> NonSimple evalForeachTensor
      ForeachVector -> NonSimple evalForeachVector
      AtVector -> Simple evalAtVector
    _ -> None

  blockingStatus = developerError "Blocking arguments not yet implemented for LossBuiltin"

  isTypeClassOp _ = False

  isCast _ _ = Nothing

--------------------------------------------------------------------------------
-- Printing

instance ConvertableBuiltin LossBuiltinType Builtin where
  convertBuiltin p = convertBuiltin p . lossToStandardBuiltinType

instance ConvertableBuiltin LossBuiltinConstructor Builtin where
  convertBuiltin p = convertBuiltin p . lossToStandardBuiltinConstructor

instance ConvertableBuiltin LossBuiltinFunction Builtin where
  convertBuiltin p = convertBuiltin p . lossToStandardBuiltinFunction

instance ConvertableBuiltin LossBuiltinExtraFunction Builtin where
  convertBuiltin p b = case b of
    SearchRatTensor {} -> cheatConvertBuiltin p $ pretty b
    SearchRecord {} -> cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossBuiltin Builtin where
  convertBuiltin p b = case b of
    LossBuiltinType op -> convertBuiltin p op
    LossBuiltinConstructor op -> convertBuiltin p op
    LossBuiltinFunction op -> convertBuiltin p op
    LossBuiltinExtraFunction op -> convertBuiltin p op

instance PrintableBuiltin LossBuiltin where
  coercionArgs = const Nothing
  isDerivedBuiltin = const Nothing
