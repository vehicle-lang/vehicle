module Vehicle.Backend.LossFunction.Core where

import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value (Value (..), WHNFBinder, WHNFBoundEnv, WHNFClosure (..), WHNFValue)
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))

--------------------------------------------------------------------------------
-- Boolean implementation

data BooleanDifferentiableLogicField
  = Truthity
  | Falsity
  | Conjunction
  | Disjunction
  | Negation
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq, Ord, Show, Generic)

instance Pretty BooleanDifferentiableLogicField where
  pretty = pretty . show

instance Hashable BooleanDifferentiableLogicField

--------------------------------------------------------------------------------
-- Tensor implementation

data TensorDifferentiableLogicField
  = TruthityElement
  | FalsityElement
  | PointwiseConjunction
  | PointwiseDisjunction
  | PointwiseNegation
  | PointwiseLe
  | PointwiseLt
  | PointwiseGe
  | PointwiseGt
  | PointwiseEq
  | PointwiseNe
  | ReduceConjunction
  | ReduceDisjunction
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Pretty TensorDifferentiableLogicField where
  pretty = pretty . show

type DifferentiableLogicImplementation =
  Map TensorDifferentiableLogicField (WHNFValue LossTensorBuiltin)

type CompiledDifferentiableLogic = (DifferentiableLogicID, DifferentiableLogicImplementation)

--------------------------------------------------------------------------------
-- Views on Boolean Tensors

-- | A view on all possible expressions that can have type `Tensor Bool`.
data BoolTensorView expr
  = VConstBoolTensor (GenericArg expr) expr expr
  | VBoolTensor (Tensor Bool)
  | VStackBoolTensor (GenericArg expr) (GenericArg expr) Int [expr]
  | VAndTensor (GenericArg expr) expr expr
  | VOrTensor (GenericArg expr) expr expr
  | VNotTensor (GenericArg expr) expr
  | VOrderRatTensor OrderOp (GenericArg expr) expr expr
  | VEqualsRatTensor EqualityOp (GenericArg expr) expr expr
  | VQuantifyRatTensor Quantifier (GenericArg expr) expr
  | VReduceAndTensor (GenericArg expr) expr
  | VReduceOrTensor (GenericArg expr) expr

fromBoolTensorView :: (HasBoolTensors expr, HasDimensionData expr) => BoolTensorView expr -> expr
fromBoolTensorView = \case
  VBoolTensor y -> INullaryBoolTensorOp (BoolTensor y)
  VAndTensor dims x y -> IBoolTensorOp AndBoolTensor [dims, explicit x, explicit y]
  VOrTensor dims x y -> IBoolTensorOp OrBoolTensor [dims, explicit x, explicit y]
  VNotTensor dims x -> IBoolTensorOp NotBoolTensor [dims, explicit x]
  VOrderRatTensor op dims x y -> IBoolTensorOp (OrderRatTensor op) [dims, explicit x, explicit y]
  VEqualsRatTensor op dims x y -> IBoolTensorOp (EqualsRatTensor op) [dims, explicit x, explicit y]
  VQuantifyRatTensor op dims x -> IBoolTensorOp (QuantifyRatTensor op) [dims, explicit x]
  VReduceAndTensor dims x -> IBoolTensorOp ReduceAndTensor [dims, explicit x]
  VReduceOrTensor dims x -> IBoolTensorOp ReduceOrTensor [dims, explicit x]
  VConstBoolTensor tElem x dims -> IDimensionDataOp ConstTensor [tElem, explicit x, explicit dims]
  VStackBoolTensor tElem elemDims n xs -> IDimensionDataOp (StackTensor n) (tElem : elemDims : (explicit <$> xs))

toBoolTensorView :: (HasDimensionData expr, HasBoolTensors expr) => expr -> BoolTensorView expr
toBoolTensorView expr = case getBoolTensorOp expr of
  Just (BoolTensor b, []) -> VBoolTensor b
  Just (AndBoolTensor, [dims, x, y]) -> VAndTensor dims (argExpr x) (argExpr y)
  Just (OrBoolTensor, [dims, x, y]) -> VOrTensor dims (argExpr x) (argExpr y)
  Just (NotBoolTensor, [dims, x]) -> VNotTensor dims (argExpr x)
  Just (OrderRatTensor op, [dims, x, y]) -> VOrderRatTensor op dims (argExpr x) (argExpr y)
  Just (EqualsRatTensor op, [dims, x, y]) -> VEqualsRatTensor op dims (argExpr x) (argExpr y)
  Just (QuantifyRatTensor op, [dims, x]) -> VQuantifyRatTensor op dims (argExpr x)
  Just (ReduceAndTensor, [dims, x]) -> VReduceAndTensor dims (argExpr x)
  Just (ReduceOrTensor, [dims, x]) -> VReduceOrTensor dims (argExpr x)
  Nothing -> case getDimensionDataOp expr of
    Just (ConstTensor, [tElem, x, dims]) -> VConstBoolTensor tElem (argExpr x) (argExpr dims)
    Just (StackTensor n, tElem : elemDims : args) -> VStackBoolTensor tElem elemDims n (fmap argExpr args)
    _ -> developerError "ill-typed BoolTensor expression"
  _ -> developerError "ill-typed BoolTensor expression"

--------------------------------------------------------------------------------
-- Views on Rational Tensors

-- | A view on all possible expressions that can have type `Tensor Rat`.
data RatTensorView expr
  = VConstRatTensor (GenericArg expr) expr expr
  | VRatTensor (Tensor Rational)
  | VRatTensorVar Lv
  | VStackRatTensor (GenericArg expr) (GenericArg expr) Int [expr]
  | VNegRatTensor (GenericArg expr) expr
  | VAddRatTensor (GenericArg expr) expr expr
  | VSubRatTensor (GenericArg expr) expr expr
  | VMulRatTensor (GenericArg expr) expr expr
  | VDivRatTensor (GenericArg expr) expr expr
  | VMinRatTensor (GenericArg expr) expr expr
  | VMaxRatTensor (GenericArg expr) expr expr
  | VReduceAddRatTensor (GenericArg expr) expr
  | VReduceMulRatTensor (GenericArg expr) expr
  | VReduceMinRatTensor (GenericArg expr) expr
  | VReduceMaxRatTensor (GenericArg expr) expr
  | VSearchRatTensor (GenericArg expr) expr expr expr expr

fromRatTensorView :: (BuiltinHasRatTensor builtin, BuiltinHasDimensionData builtin) => RatTensorView (WHNFValue builtin) -> WHNFValue builtin
fromRatTensorView = \case
  VRatTensor y -> INullaryRatTensorOp (RatTensor y)
  VNegRatTensor dims x -> IRatTensorOp NegRatTensor [dims, explicit x]
  VAddRatTensor dims x y -> IRatTensorOp AddRatTensor [dims, explicit x, explicit y]
  VSubRatTensor dims x y -> IRatTensorOp SubRatTensor [dims, explicit x, explicit y]
  VMulRatTensor dims x y -> IRatTensorOp MulRatTensor [dims, explicit x, explicit y]
  VDivRatTensor dims x y -> IRatTensorOp DivRatTensor [dims, explicit x, explicit y]
  VMinRatTensor dims x y -> IRatTensorOp MinRatTensor [dims, explicit x, explicit y]
  VMaxRatTensor dims x y -> IRatTensorOp MaxRatTensor [dims, explicit x, explicit y]
  VReduceAddRatTensor dims x -> IRatTensorOp ReduceAddRatTensor [dims, explicit x]
  VReduceMulRatTensor dims x -> IRatTensorOp ReduceMulRatTensor [dims, explicit x]
  VReduceMinRatTensor dims x -> IRatTensorOp ReduceMinRatTensor [dims, explicit x]
  VReduceMaxRatTensor dims x -> IRatTensorOp ReduceMaxRatTensor [dims, explicit x]
  VConstRatTensor dims x y -> IDimensionDataOp ConstTensor [dims, explicit x, explicit y]
  VStackRatTensor tElem elemDims n xs -> IDimensionDataOp (StackTensor n) (tElem : elemDims : (explicit <$> xs))
  VSearchRatTensor dims reduce lower upper fn -> IRatTensorOp SearchRatTensor (dims : (explicit <$> [reduce, lower, upper, fn]))
  VRatTensorVar v -> VBoundVar v []

toRatTensorView :: (BuiltinHasRatTensor builtin, BuiltinHasDimensionData builtin) => WHNFValue builtin -> RatTensorView (WHNFValue builtin)
toRatTensorView expr = case getRatTensorOp expr of
  Just (RatTensor b, []) -> VRatTensor b
  Just (NegRatTensor, [dims, x]) -> VNegRatTensor dims (argExpr x)
  Just (AddRatTensor, [dims, x, y]) -> VAddRatTensor dims (argExpr x) (argExpr y)
  Just (SubRatTensor, [dims, x, y]) -> VSubRatTensor dims (argExpr x) (argExpr y)
  Just (MulRatTensor, [dims, x, y]) -> VMulRatTensor dims (argExpr x) (argExpr y)
  Just (DivRatTensor, [dims, x, y]) -> VDivRatTensor dims (argExpr x) (argExpr y)
  Just (MinRatTensor, [dims, x, y]) -> VMinRatTensor dims (argExpr x) (argExpr y)
  Just (MaxRatTensor, [dims, x, y]) -> VMaxRatTensor dims (argExpr x) (argExpr y)
  Just (ReduceAddRatTensor, [dims, x]) -> VReduceAddRatTensor dims (argExpr x)
  Just (ReduceMulRatTensor, [dims, x]) -> VReduceMulRatTensor dims (argExpr x)
  Just (ReduceMinRatTensor, [dims, x]) -> VReduceMinRatTensor dims (argExpr x)
  Just (ReduceMaxRatTensor, [dims, x]) -> VReduceMaxRatTensor dims (argExpr x)
  Just (SearchRatTensor, [dims, reduce, lower, upper, fn]) -> VSearchRatTensor dims (argExpr reduce) (argExpr lower) (argExpr upper) (argExpr fn)
  Nothing -> case getDimensionDataOp expr of
    Just (ConstTensor, [tElem, x, y]) -> VConstRatTensor tElem (argExpr x) (argExpr y)
    Just (StackTensor n, tElem : dims : args) -> VStackRatTensor tElem dims n (fmap argExpr args)
    _ -> developerError "ill-typed RatTensor expression"
  _ -> developerError "ill-typed RatTensor expression"

--------------------------------------------------------------------------------
-- Other

-- | Standard library operations that we don't want to normalise
-- as we need them present to convert into tensors.
preservedStdLibOps :: Set StdLibFunction
preservedStdLibOps =
  Set.fromList
    [ StdForeachIndex
    ]

pattern VLam2 :: WHNFBinder builtin -> WHNFBoundEnv builtin -> Binder builtin -> Expr builtin -> WHNFValue builtin
pattern VLam2 binder1 env binder2 body <- VLam binder1 (WHNFClosure env (Lam _ binder2 body))
