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
import Vehicle.Data.Code.Value (BoundEnv, Closure (..), VBinder, Value (..))
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
  Map TensorDifferentiableLogicField (Value LossTensorBuiltin)

type CompiledDifferentiableLogic = (DifferentiableLogicID, DifferentiableLogicImplementation)

--------------------------------------------------------------------------------
-- Views on Boolean Tensors

-- | A view on all possible expressions that can have type `Tensor Bool`.
data BoolTensorView expr
  = VConstBoolTensor expr expr
  | VBoolTensor (Tensor Bool)
  | VStackBoolTensor Int (GenericArg expr) [expr]
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
  VConstBoolTensor x dims -> IDimensionDataOp ConstTensor [implicit IBoolElementType, explicit x, explicit dims]
  VStackBoolTensor n elemDims xs -> IDimensionDataOp (StackTensor n) (implicit IBoolElementType : elemDims : (explicit <$> xs))

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
    Just (ConstTensor, [argExpr -> IBoolElementType, x, dims]) -> VConstBoolTensor (argExpr x) (argExpr dims)
    Just (StackTensor n, (argExpr -> IBoolElementType) : elemDims : args) -> VStackBoolTensor n elemDims (fmap argExpr args)
    _ -> developerError "ill-typed BoolTensor expression"
  _ -> developerError "ill-typed BoolTensor expression"

--------------------------------------------------------------------------------
-- Views on Rational Tensors

-- | A view on all possible expressions that can have type `Tensor Rat`.
data RatTensorView expr
  = VConstRatTensor expr expr
  | VRatTensor (Tensor Rational)
  | VRatTensorVar Lv
  | VStackRatTensor Int (GenericArg expr) [expr]
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

fromRatTensorView :: (BuiltinHasRatTensor builtin, BuiltinHasDimensionData builtin) => RatTensorView (Value builtin) -> Value builtin
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
  VConstRatTensor x dims -> IDimensionDataOp ConstTensor [implicit IRatElementType, explicit x, explicit dims]
  VStackRatTensor n elemDims xs -> IDimensionDataOp (StackTensor n) (implicit IRatElementType : elemDims : (explicit <$> xs))
  VSearchRatTensor dims reduce lower upper fn -> IRatTensorOp SearchRatTensor (dims : (explicit <$> [reduce, lower, upper, fn]))
  VRatTensorVar v -> VBoundVar v []

toRatTensorView :: (BuiltinHasRatTensor builtin, BuiltinHasDimensionData builtin) => Value builtin -> RatTensorView (Value builtin)
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
    Just (ConstTensor, [argExpr -> IRatElementType, x, dims]) -> VConstRatTensor (argExpr x) (argExpr dims)
    Just (StackTensor n, (argExpr -> IRatElementType) : dims : args) -> VStackRatTensor n dims (fmap argExpr args)
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

pattern VLam2 :: VBinder builtin -> BoundEnv builtin -> Binder builtin -> Expr builtin -> Value builtin
pattern VLam2 binder1 env binder2 body <- VLam binder1 (Closure env (Lam _ binder2 body))
