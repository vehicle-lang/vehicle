module Vehicle.Compile.Print.Builtin where

import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin (..))
import Vehicle.Data.Builtin.Loss (LossTensorBuiltin (..))
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin (..))
import Vehicle.Data.Builtin.Tensor (TensorBuiltin (..))
import Vehicle.Data.Code.Expr (Expr (..), mapBuiltins, normAppList, pattern BuiltinExpr)
import Vehicle.Data.Code.Interface (tensorToExpr)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Type classes

class ConvertableBuiltin builtin1 builtin2 where
  convertBuiltin :: Provenance -> builtin1 -> Expr builtin2

instance ConvertableBuiltin builtin builtin where
  convertBuiltin = Builtin

instance ConvertableBuiltin BuiltinType Builtin where
  convertBuiltin p = Builtin p . BuiltinType

instance ConvertableBuiltin TypeClassOp Builtin where
  convertBuiltin p = Builtin p . TypeClassOp

instance ConvertableBuiltin BuiltinConstructor Builtin where
  convertBuiltin p = Builtin p . BuiltinConstructor

instance ConvertableBuiltin BuiltinFunction Builtin where
  convertBuiltin p = Builtin p . BuiltinFunction

instance ConvertableBuiltin OrderOp Builtin where
  convertBuiltin p = convertBuiltin p . OrderTC

instance ConvertableBuiltin EqualityOp Builtin where
  convertBuiltin p = convertBuiltin p . EqualsTC

instance ConvertableBuiltin DimensionTypeBuiltin Builtin where
  convertBuiltin p = \case
    TensorType -> FreeVar p (identifierOf StdTensor)
    DimensionType -> convertBuiltin p Nat
    DimensionsType -> BuiltinExpr p (BuiltinType List) [explicit (convertBuiltin p Nat)]
    DimensionIndexType -> convertBuiltin p Index

instance ConvertableBuiltin DimensionDataBuiltin Builtin where
  convertBuiltin p = \case
    Dimension n -> convertBuiltin p (LNat n)
    DimensionNil -> convertBuiltin p Nil
    DimensionCons -> convertBuiltin p Cons
    DimensionIndex n -> convertBuiltin p (LIndex n)
    DimensionIndexTensor t -> tensorToExpr (convertBuiltin p . LIndex) t
    DimensionLookup -> convertBuiltin p At
    ConstTensor -> cheatConvertBuiltin p "const"
    StackTensor {} -> cheatConvertBuiltin p "stack"

instance ConvertableBuiltin TensorBuiltin Builtin where
  convertBuiltin p b = case b of
    TensorRat op -> convertBuiltin p op
    TensorBool op -> convertBuiltin p op
    TensorDimType op -> convertBuiltin p op
    TensorDimData op -> convertBuiltin p op

instance ConvertableBuiltin RatTensorBuiltin Builtin where
  convertBuiltin p = \case
    RatTensor vs -> tensorToExpr (convertBuiltin p . LRat) vs
    RatType -> convertBuiltin p Rat
    RatLiteral r -> convertBuiltin p (LRat r)
    NegRatTensor -> convertBuiltin p NegTC
    AddRatTensor -> convertBuiltin p AddTC
    SubRatTensor -> convertBuiltin p SubTC
    MulRatTensor -> convertBuiltin p MulTC
    DivRatTensor -> convertBuiltin p DivTC
    MinRatTensor -> convertBuiltin p MinRat
    MaxRatTensor -> convertBuiltin p MaxRat
    ReduceAddRatTensor -> cheatConvertBuiltin p "reduceAdd"
    ReduceMulRatTensor -> cheatConvertBuiltin p "reduceMul"
    ReduceMinRatTensor -> cheatConvertBuiltin p "reduceMin"
    ReduceMaxRatTensor -> cheatConvertBuiltin p "reduceMax"
    SearchRatTensor {} -> cheatConvertBuiltin p "search"

instance ConvertableBuiltin BoolTensorBuiltin Builtin where
  convertBuiltin p = \case
    BoolType -> convertBuiltin p Bool
    BoolLiteral b -> convertBuiltin p (LBool b)
    BoolTensor vs -> tensorToExpr (convertBuiltin p . LBool) vs
    AndBoolTensor -> convertBuiltin p And
    OrBoolTensor -> convertBuiltin p Or
    NotBoolTensor -> convertBuiltin p Not
    QuantifyRatTensor q -> convertBuiltin p (Quantifier q)
    EqualsRatTensor op -> convertBuiltin p op
    OrderRatTensor op -> convertBuiltin p op
    ReduceAndTensor -> cheatConvertBuiltin p "ReduceAnd"
    ReduceOrTensor -> cheatConvertBuiltin p "ReduceOr"

instance ConvertableBuiltin PolarityBuiltin Builtin where
  convertBuiltin p = \case
    PolarityConstructor c -> convertBuiltin p c
    PolarityFunction f -> convertBuiltin p f
    b -> cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LinearityBuiltin Builtin where
  convertBuiltin p = \case
    LinearityConstructor c -> convertBuiltin p c
    LinearityFunction f -> convertBuiltin p f
    b -> cheatConvertBuiltin p $ pretty b

instance ConvertableBuiltin LossTensorBuiltin Builtin where
  convertBuiltin p b = case b of
    LossTensorRat op -> convertBuiltin p op
    LossTensorDimType op -> convertBuiltin p op
    LossTensorDimData op -> convertBuiltin p op

convertExprBuiltins ::
  forall builtin1 builtin2.
  (ConvertableBuiltin builtin1 builtin2) =>
  Expr builtin1 ->
  Expr builtin2
convertExprBuiltins = mapBuiltins $ \p b args ->
  normAppList (convertBuiltin p b) args

-- | Use to convert builtins for printing that have no representation in the
-- standard `Builtin` type.
cheatConvertBuiltin :: Provenance -> Doc a -> Expr builtin
cheatConvertBuiltin p b = FreeVar p $ stdlibIdentifier $ layoutAsText b

--------------------------------------------------------------------------------
-- Printable builtins

class (Show builtin, Pretty builtin, ConvertableBuiltin builtin Builtin) => PrintableBuiltin builtin where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  isCoercion :: builtin -> Bool

instance PrintableBuiltin Builtin where
  isCoercion = \case
    BuiltinFunction FromNat {} -> True
    BuiltinFunction FromRat {} -> True
    TypeClassOp FromNatTC {} -> True
    TypeClassOp FromRatTC {} -> True
    TypeClassOp FromVecTC {} -> True
    _ -> False

instance PrintableBuiltin PolarityBuiltin where
  isCoercion = const False

instance PrintableBuiltin LinearityBuiltin where
  isCoercion = const False

instance PrintableBuiltin TensorBuiltin where
  isCoercion = const False

instance PrintableBuiltin LossTensorBuiltin where
  isCoercion = const False
