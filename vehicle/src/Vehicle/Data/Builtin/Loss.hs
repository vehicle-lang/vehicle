module Vehicle.Data.Builtin.Loss
  ( module Vehicle.Data.Builtin.Loss,
    module Vehicle.Data.Builtin.Core.BasicOperations,
  )
where

import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Prettyprinter (brackets)
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.AST.Name (HasIdentifier (..))
import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core hiding (HasQuantifier)
import Vehicle.Data.Builtin.Standard.Core qualified as S hiding (HasQuantifier)
import Vehicle.Data.Code.DSL (tRat, tTensor)
import Vehicle.Data.Code.Interface
import Vehicle.Data.DSL
import Vehicle.Prelude (Pretty (..))

--------------------------------------------------------------------------------
-- Builtins

data LossMode = Search | Train

-- | The builtin types after translation to loss functions (missing all builtins
-- that involve the Bool type).
data LossBuiltin (mode :: LossMode)
  = StandardBuiltinType S.BuiltinType
  | LossBuiltinType LossBuiltinType
  | StandardBuiltinConstructor S.BuiltinConstructor
  | LossBuiltinConstructor LossBuiltinConstructor
  | StandardBuiltinFunction S.BuiltinFunction
  | LossBuiltinFunction LossBuiltinFunction
  | StandardDerivedFunction S.DerivedFunction
  | LossBuiltinCast LossBuiltinCast
  | LossBuiltinTypeClass LossBuiltinTypeClass
  | LossBuiltinTypeClassOp LossBuiltinTypeClassOp
  deriving (Show, Eq, Ord, Generic)

instance Pretty (LossBuiltin mode) where
  pretty = pretty . show

instance Hashable (LossBuiltin mode)

--------------------------------------------------------------------------------
-- Builtin types

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data LossBuiltinType
  = GradientType
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinType where
  pretty = \case
    GradientType -> "GradientType"

instance Hashable LossBuiltinType

--------------------------------------------------------------------------------
-- Constructor datatype

-- | Constructors for types in the language. The types and type-classes
-- are viewed as constructors for `Type`.
data LossBuiltinConstructor
  = WithGradients
  | WithoutGradients
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinConstructor where
  pretty = \case
    WithGradients -> "withGradients"
    WithoutGradients -> "withoutGradients"

instance Hashable LossBuiltinConstructor

--------------------------------------------------------------------------------
-- Loss casts

-- | Note that the casts these are expressible in the existing language
--    ToBoolTensor in terms of id
--    ToRatTensor in terms of `where` `[[True]]` and `[[False]]`
-- but we include them as builtins in their own right so that we can detect
-- and eliminate the casts.
data LossBuiltinCast
  = FromBoolTensorToBoolTensor
  | FromBoolTensorToRatTensor
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinCast where
  pretty = \case
    FromBoolTensorToBoolTensor -> "fromBoolTensorToBoolTensor"
    FromBoolTensorToRatTensor -> "fromBoolTensorToRatTensor"

instance Hashable LossBuiltinCast

--------------------------------------------------------------------------------
-- Loss function

-- | Note that these additional functions are added so that they can be detected
-- and removed with a nice user error message.
data LossBuiltinFunction
  = IfRatTensorWithGradients
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinFunction where
  pretty = \case
    IfRatTensorWithGradients -> "ifWithGradients"

instance Hashable LossBuiltinFunction

--------------------------------------------------------------------------------
-- Type-classes

data LossBuiltinTypeClass
  = HasBoolLiterals
  | HasNot
  | HasAnd
  | HasOr
  | HasImplies
  | HasReduceAnd
  | HasReduceOr
  | HasIfRatTensor
  | HasRatTensorCompare ComparisonOp
  | HasExists
  | MaxGradients
  | ValidNetworkType
  | ValidNetworkIOType
  | ValidDatasetType
  | ValidParamType
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinTypeClass where
  pretty = \case
    HasBoolLiterals -> "HasBoolLiterals"
    HasAnd -> "HasAnd"
    HasOr -> "HasOr"
    HasNot -> "HasNot"
    HasImplies -> "HasImplies"
    HasReduceAnd -> "HasReduceAnd"
    HasReduceOr -> "HasReduceOr"
    HasRatTensorCompare op -> "HasRatTensorCompare" <> brackets (pretty op)
    HasIfRatTensor -> "HasIfRatTensor"
    HasExists -> "HasExists"
    MaxGradients -> "MaxGradients"
    ValidNetworkType -> "ValidNetworkType"
    ValidNetworkIOType -> "ValidNetworkIOType"
    ValidDatasetType -> "ValidDatasetType"
    ValidParamType -> "ValidParamType"

instance Hashable LossBuiltinTypeClass

--------------------------------------------------------------------------------
-- Type-classes

data LossBuiltinTypeClassOp
  = FromBoolTensorTC
  | AndTCOp
  | OrTCOp
  | NotTCOp
  | ReduceAndTCOp
  | ReduceOrTCOp
  | ImpliesTCOp
  | CompareRatTensorTCOp ComparisonOp
  | ExistsTCOp
  | IfRatTensorTCOp
  deriving (Eq, Ord, Show, Generic)

instance Pretty LossBuiltinTypeClassOp where
  pretty = \case
    FromBoolTensorTC -> "fromBoolTensorTC"
    NotTCOp -> "notTC"
    AndTCOp -> "andTC"
    OrTCOp -> "orTC"
    ImpliesTCOp -> "impliesTC"
    ReduceAndTCOp -> "reduceAndTC"
    ReduceOrTCOp -> "reduceOrTC"
    CompareRatTensorTCOp op -> comparisonOpName op <> "RatTensorTC"
    ExistsTCOp -> "existsTC"
    IfRatTensorTCOp -> "ifRatTensorTCOp"

instance Hashable LossBuiltinTypeClassOp

--------------------------------------------------------------------------------
-- Functions

-- | Is [[true]] < [[false]] in the logic, i.e. does the
-- loss value need to be minimised?
type LogicDirection = Bool

--------------------------------------------------------------------------------
-- Accessors

zeroArityConstructorAccessor :: BuiltinConstructor -> Accessor (LossBuiltin mode) ()
zeroArityConstructorAccessor b =
  Access
    { getExpr = \case
        StandardBuiltinConstructor b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> StandardBuiltinConstructor b
    }

typeAccessor :: S.BuiltinType -> Accessor (LossBuiltin mode) ()
typeAccessor b =
  Access
    { getExpr = \case
        StandardBuiltinType b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> StandardBuiltinType b
    }

functionAccessor :: BuiltinFunction -> Accessor (LossBuiltin mode) ()
functionAccessor b =
  Access
    { getExpr = \case
        StandardBuiltinFunction b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> StandardBuiltinFunction b
    }

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
-- Types

instance BuiltinHasStandardTypes (LossBuiltin mode) where
  accessBuiltinType =
    Access
      { mkExpr = StandardBuiltinType,
        getExpr = \case
          StandardBuiltinType c -> Just c
          _ -> Nothing
      }

instance BuiltinHasStandardData (LossBuiltin mode) where
  accessBuiltinFunction =
    Access
      { mkExpr = StandardBuiltinFunction,
        getExpr = \case
          StandardBuiltinFunction c -> Just c
          _ -> Nothing
      }

  accessBuiltinConstructor =
    Access
      { mkExpr = StandardBuiltinConstructor,
        getExpr = \case
          StandardBuiltinConstructor c -> Just c
          _ -> Nothing
      }

--------------------------------------------------------------------------------
-- Index

instance BuiltinHasIndexType (LossBuiltin mode) where
  accessIndexTypeBuiltin = typeAccessor IndexType

instance BuiltinHasIndexLiterals (LossBuiltin mode) where
  accessIndexLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (S.IndexLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . S.IndexLiteral
      }

--------------------------------------------------------------------------------
-- Index

instance BuiltinHasBoolType (LossBuiltin mode) where
  accessBoolTypeBuiltin = typeAccessor BoolType

instance BuiltinHasBoolLiterals (LossBuiltin mode) where
  accessBoolTensorLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (S.BoolTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . S.BoolTensorLiteral
      }

  accessNotBuiltin = functionAccessor Not
  accessAndBuiltin = functionAccessor And
  accessOrBuiltin = functionAccessor Or
  accessImpliesBuiltin = functionAccessor Implies
  accessReduceAndBuiltin = functionAccessor ReduceAndTensor
  accessReduceOrBuiltin = functionAccessor ReduceOrTensor
  accessIfBuiltin = functionAccessor If

  accessCompareIndexBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinFunction (CompareIndex op) -> Just op
          _ -> Nothing,
        mkExpr = \op -> StandardBuiltinFunction (CompareIndex op)
      }
  accessCompareNatBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinFunction (CompareNat op) -> Just op
          _ -> Nothing,
        mkExpr = \op -> StandardBuiltinFunction (CompareNat op)
      }
  accessCompareRatTensorBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinFunction (CompareRatTensor op) -> Just op
          _ -> Nothing,
        mkExpr = \op -> StandardBuiltinFunction (CompareRatTensor op)
      }

  accessQuantifyRatTensorBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinFunction (QuantifyRatTensor q) -> Just q
          _ -> Nothing,
        mkExpr = StandardBuiltinFunction . QuantifyRatTensor
      }

  accessQuantifyRecordBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinFunction (QuantifyRecord q) -> Just q
          _ -> Nothing,
        mkExpr = StandardBuiltinFunction . QuantifyRecord
      }

instance BuiltinHasIterate (LossBuiltin mode) where
  accessIterateBuiltin = functionAccessor Iterate

--------------------------------------------------------------------------------
-- Nat

instance BuiltinHasNatType (LossBuiltin mode) where
  accessNatTypeBuiltin = typeAccessor NatType

instance BuiltinHasNatLiterals (LossBuiltin mode) where
  accessNatLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (S.NatLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . S.NatLiteral
      }

  accessNatTensorLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (S.NatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . S.NatTensorLiteral
      }

  accessAddNatBuiltin = functionAccessor (Add AddNat)
  accessMulNatBuiltin = functionAccessor (Mul MulNat)

--------------------------------------------------------------------------------
-- Rat

instance BuiltinHasRatType (LossBuiltin mode) where
  accessRatTypeBuiltin = typeAccessor RatType

instance BuiltinHasRatLiterals (LossBuiltin mode) where
  accessRatTensorLitBuiltin =
    Access
      { getExpr = \case
          StandardBuiltinConstructor (S.RatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = StandardBuiltinConstructor . S.RatTensorLiteral
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

instance BuiltinHasListType (LossBuiltin mode) where
  accessListTypeBuiltin = typeAccessor ListType

instance BuiltinHasListLiterals (LossBuiltin mode) where
  accessNilBuiltin = zeroArityConstructorAccessor Nil
  accessConsBuiltin = zeroArityConstructorAccessor Cons
  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList
  accessReverseListBuiltin = functionAccessor ReverseList
  accessAppendListBuiltin = functionAccessor AppendList

--------------------------------------------------------------------------------
-- Vector

instance BuiltinHasVectorType (LossBuiltin mode) where
  accessVectorTypeBuiltin = typeAccessor VectorType

instance BuiltinHasVectors (LossBuiltin mode) where
  accessVecLitBuiltin = zeroArityConstructorAccessor VectorLiteral
  accessAtVectorBuiltin = functionAccessor AtVector

--------------------------------------------------------------------------------
-- Tensor

instance BuiltinHasTensorType (LossBuiltin mode) where
  accessTensorTypeBuiltin = typeAccessor TensorType

instance BuiltinHasTensors (LossBuiltin mode) where
  accessConstTensorBuiltin = functionAccessor ConstTensor
  accessStackTensorBuiltin = functionAccessor StackTensor
  accessAtTensorBuiltin = functionAccessor AtTensor
  accessTransposeBuiltin = functionAccessor Transpose

instance BuiltinHasForeach (LossBuiltin mode) where
  accessForeachTensorBuiltin = functionAccessor ForeachTensor
  accessForeachVectorBuiltin = functionAccessor ForeachVector

--------------------------------------------------------------------------------
-- Normalisation

instance (HasBuiltinConstructor expr thunk) => HasTensorLiterals expr (LossBuiltin mode) where
  tensorLiterals =
    [ Wrapper accessNatTensorLiteral,
      Wrapper accessRatTensorLiteral
    ]

instance
  (HasBuiltinConstructor expr thunk) =>
  HasLiftableTensorOperations expr thunk (LossBuiltin mode)
  where
  liftableTensorOp1s =
    [ (accessNegRatTensor, IRatType)
    ]

  liftableTensorOp2s =
    [ (accessAddRatTensor, IRatType),
      (accessMulRatTensor, IRatType),
      (accessSubRatTensor, IRatType),
      (accessDivRatTensor, IRatType),
      (accessMinRatTensor, IRatType),
      (accessMaxRatTensor, IRatType)
    ]

  liftableTensorComparisons = []

instance NormalisableBuiltin (LossBuiltin mode) where
  evalScheme = \case
    StandardBuiltinFunction f -> case f of
      S.Add AddNat -> Eval evalAddNat
      S.Mul MulNat -> Eval evalMulNat
      S.Neg NegRatTensor -> Eval evalNegRatTensor
      S.Add AddRatTensor -> Eval evalAddRatTensor
      S.Sub SubRatTensor -> Eval evalSubRatTensor
      S.Mul MulRatTensor -> Eval evalMulRatTensor
      S.Div DivRatTensor -> Eval evalDivRatTensor
      S.Min MinRatTensor -> Eval evalMinRatTensor
      S.Max MaxRatTensor -> Eval evalMaxRatTensor
      S.Pow PowRatTensor -> Eval evalPowRatTensor
      S.Log LogRatTensor -> None
      S.Exp ExpRatTensor -> None
      S.Not -> Eval evalNot
      S.And -> Eval evalAnd
      S.Or -> Eval evalOr
      S.Implies -> Eval evalImplies
      S.CompareIndex op -> Eval $ evalCompareIndex op
      S.CompareNat op -> Eval $ evalCompareNat op
      S.CompareRatTensor op -> Eval $ evalCompareRatTensor op
      S.If -> Eval evalIf
      S.QuantifyRatTensor {} -> None
      S.QuantifyRecord {} -> None
      S.ReduceAddRatTensor -> Eval evalReduceAddRatTensor
      S.ReduceMulRatTensor -> Eval evalReduceMulRatTensor
      S.ReduceMinRatTensor -> Eval evalReduceMinRatTensor
      S.ReduceMaxRatTensor -> Eval evalReduceMaxRatTensor
      S.ReduceAndTensor -> Eval evalReduceAndTensor
      S.ReduceOrTensor -> Eval evalReduceOrTensor
      S.AtTensor -> Eval evalAtTensor
      S.StackTensor -> Eval evalStackTensor
      S.ConstTensor -> Eval evalConstTensor
      S.Transpose -> Eval evalTransposeTensor
      S.FoldList -> Eval evalFoldList
      S.MapList -> Eval evalMapList
      S.ReverseList -> Eval evalReverseList
      S.AppendList -> Eval evalAppendList
      S.ForeachTensor -> Eval evalForeachTensor
      S.ForeachVector -> Eval evalForeachVector
      S.AtVector -> Eval evalAtVector
      S.Iterate -> Eval evalIterate
      S.WhereTensor -> None
      S.SearchRatTensor -> None
    LossBuiltinTypeClassOp {} -> TypeClassOperation
    _ -> None

  isTypeClassOp = \case
    LossBuiltinTypeClassOp {} -> True
    _ -> False

  isCast p b = case b of
    LossBuiltinCast FromBoolTensorToBoolTensor -> Just $ forceEvalSimpleBuiltin p b forcedEvalFromBoolTensorToBoolTensor
    LossBuiltinCast FromBoolTensorToRatTensor -> Just $ forceEvalSimpleBuiltin p b forcedEvalFromBoolTensorToRatTensor
    _ -> Nothing

forcedEvalFromBoolTensorToBoolTensor ::
  (MonadNormBuiltin m) =>
  EvalSimple expr thunk TensorOp1Args (LossBuiltin mode) m
forcedEvalFromBoolTensorToBoolTensor (TensorOp1Args _ value) = return $ Evaluated value

forcedEvalFromBoolTensorToRatTensor ::
  (MonadNormBuiltin m) =>
  EvalSimple Expr Expr FromBoolTensorToRatTensorArgs (LossBuiltin mode) m
forcedEvalFromBoolTensorToRatTensor (FromBoolTensorToRatTensorArgs dims tensor trueElement falseElement) = do
  let constTrueTensor = mkExpr accessConstTensor $ ConstTensorArgs IRatType trueElement dims
  let args = mkExpr accessSpine $ WhereTensorArgs dims constTrueTensor tensor falseElement
  Evaluated <$> forceEvalSimpleBuiltin mempty (StandardBuiltinFunction WhereTensor) evalWhereTensor args

--------------------------------------------------------------------------------
-- Printing

instance ConvertableBuiltin LossBuiltinType Builtin where
  convertBuiltin p b = cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossBuiltinConstructor Builtin where
  convertBuiltin p b = cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossBuiltinTypeClass Builtin where
  convertBuiltin p b = cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossBuiltinCast Builtin where
  convertBuiltin p b = cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossBuiltinFunction Builtin where
  convertBuiltin p b = cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossBuiltinTypeClassOp Builtin where
  convertBuiltin p b = cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin (LossBuiltin mode) Builtin where
  convertBuiltin p b = case b of
    StandardBuiltinType op -> Builtin p $ BuiltinType op
    LossBuiltinType op -> convertBuiltin p op
    StandardBuiltinConstructor op -> Builtin p $ BuiltinConstructor op
    LossBuiltinConstructor op -> convertBuiltin p op
    StandardBuiltinFunction op -> Builtin p $ BuiltinFunction op
    StandardDerivedFunction op -> Builtin p $ DerivedFunction op
    LossBuiltinCast op -> convertBuiltin p op
    LossBuiltinFunction op -> convertBuiltin p op
    LossBuiltinTypeClass op -> convertBuiltin p op
    LossBuiltinTypeClassOp op -> convertBuiltin p op

instance PrintableBuiltin (LossBuiltin mode) where
  coercionArgs = const Nothing
  isDerivedBuiltin = \case
    StandardDerivedFunction f -> Just $ identifierOf f
    _ -> Nothing

tGradient :: DSLExpr (LossBuiltin mode)
tGradient = builtin $ LossBuiltinType GradientType

withGradients :: DSLExpr (LossBuiltin mode)
withGradients = builtin $ LossBuiltinConstructor WithGradients

withoutGradients :: DSLExpr (LossBuiltin mode)
withoutGradients = builtin $ LossBuiltinConstructor WithoutGradients

forAllGradients :: (DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)) -> DSLExpr (LossBuiltin mode)
forAllGradients = forAllIrrelevant "g" tGradient

forAllGradientPairs :: (DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)) -> DSLExpr (LossBuiltin mode)
forAllGradientPairs fun =
  forAllIrrelevant "g1" tGradient $ \g1 ->
    forAllIrrelevant "g2" tGradient $ \g2 ->
      fun g1 g2

forAllGradientTriples :: (DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)) -> DSLExpr (LossBuiltin mode)
forAllGradientTriples fun =
  forAllIrrelevant "g1" tGradient $ \g1 ->
    forAllIrrelevant "g2" tGradient $ \g2 ->
      forAllIrrelevant "g3" tGradient $ \g3 ->
        fun g1 g2 g3

tRatWithGradients :: DSLExpr (LossBuiltin mode)
tRatWithGradients = tRat .@@ [withGradients]

tRatWithoutGradients :: DSLExpr (LossBuiltin mode)
tRatWithoutGradients = tRat .@@ [withoutGradients]

tRatTensorWithGradients :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
tRatTensorWithGradients = tTensor tRatWithGradients

tRatTensorWithoutGradients :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
tRatTensorWithoutGradients = tTensor tRatWithoutGradients

lossCast :: LossBuiltinCast -> DSLExpr (LossBuiltin mode)
lossCast = builtin . LossBuiltinCast

lossTypeClass :: LossBuiltinTypeClass -> DSLExpr (LossBuiltin mode)
lossTypeClass = builtin . LossBuiltinTypeClass

lossBuiltinFunction :: LossBuiltinFunction -> DSLExpr (LossBuiltin mode)
lossBuiltinFunction = builtin . LossBuiltinFunction

hasBoolLiterals :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasBoolLiterals t = lossTypeClass HasBoolLiterals @@ [t]

hasExists :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasExists t = lossTypeClass HasExists @@ [t]

hasIfRatTensor :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasIfRatTensor t = lossTypeClass HasIfRatTensor @@ [t]

hasNot :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasNot t = lossTypeClass HasNot @@ [t]

hasAnd :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasAnd t1 t2 t3 = lossTypeClass HasAnd @@ [t1, t2, t3]

hasOr :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasOr t1 t2 t3 = lossTypeClass HasOr @@ [t1, t2, t3]

hasImplies :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasImplies t1 t2 t3 = lossTypeClass HasImplies @@ [t1, t2, t3]

hasReduceAnd :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasReduceAnd t = lossTypeClass HasReduceAnd @@ [t]

hasReduceOr :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasReduceOr t = lossTypeClass HasReduceOr @@ [t]

hasRatTensorComparison :: ComparisonOp -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
hasRatTensorComparison op t1 t2 t3 = lossTypeClass (HasRatTensorCompare op) @@ [t1, t2, t3]

validParameterType :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
validParameterType t = lossTypeClass ValidParamType @@ [t]

validDatasetType :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
validDatasetType t = lossTypeClass ValidDatasetType @@ [t]

validNetworkType :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
validNetworkType t = lossTypeClass ValidNetworkType @@ [t]

validNetworkIOType :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
validNetworkIOType g t = lossTypeClass ValidNetworkIOType @@ [g, t]

maxGradients :: DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode) -> DSLExpr (LossBuiltin mode)
maxGradients g1 g2 g3 = lossTypeClass MaxGradients @@ [g1, g2, g3]
