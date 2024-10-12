module Vehicle.Backend.LossFunction.Core where

import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.Value (Value (..), WHNFBinder, WHNFBoundEnv, WHNFClosure (..), WHNFValue)
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
{-
-- | A view on all possible expressions that can have type `Tensor Bool`.
data BoolTensorView expr
  = VConstBoolTensor expr expr
  | VBoolTensor (Tensor Bool)
  | VStackBoolTensor Int [expr]
  | VAndTensor expr expr
  | VOrTensor expr expr
  | VNotTensor expr
  | VOrderRatTensor OrderOp expr expr
  | VEqualsRatTensor EqualityOp expr expr
  | VQuantifyRatTensor Quantifier expr
  | VReduceAndTensor expr
  | VReduceOrTensor expr

fromBoolTensorView :: (HasBoolTensors expr, HasDimensionData expr) => BoolTensorView expr -> expr
fromBoolTensorView = \case
  VBoolTensor y -> INullaryBoolTensorOp (BoolTensor y)
  VAndTensor x y -> IBoolTensorOp AndBoolTensor (explicit <$> [x, y])
  VOrTensor x y -> IBoolTensorOp OrBoolTensor (explicit <$> [x, y])
  VNotTensor x -> IBoolTensorOp NotBoolTensor (explicit <$> [x])
  VOrderRatTensor op x y -> IBoolTensorOp (OrderRatTensor op) (explicit <$> [x, y])
  VEqualsRatTensor op x y -> IBoolTensorOp (EqualsRatTensor op) (explicit <$> [x, y])
  VQuantifyRatTensor op x -> IBoolTensorOp (QuantifyRatTensor op) (explicit <$> [x])
  VReduceAndTensor x -> IBoolTensorOp ReduceAndTensor (explicit <$> [x])
  VReduceOrTensor x -> IBoolTensorOp ReduceOrTensor (explicit <$> [x])
  VConstBoolTensor x y -> IBoolConstTensor x y
  VStackBoolTensor n xs -> IDimensionDataOp (StackTensor n) (explicit <$> xs)

toBoolTensorView :: (HasDimensionData expr, HasBoolTensors expr) => expr -> BoolTensorView expr
toBoolTensorView expr = case getBoolTensorOp expr of
  Just (BoolTensor b, []) -> VBoolTensor b
  Just (AndBoolTensor, [x, y]) -> VAndTensor (argExpr x) (argExpr y)
  Just (OrBoolTensor, [x, y]) -> VOrTensor (argExpr x) (argExpr y)
  Just (NotBoolTensor, [x]) -> VNotTensor (argExpr x)
  Just (OrderRatTensor op, [x, y]) -> VOrderRatTensor op (argExpr x) (argExpr y)
  Just (EqualsRatTensor op, [x, y]) -> VEqualsRatTensor op (argExpr x) (argExpr y)
  Just (QuantifyRatTensor op, [x]) -> VQuantifyRatTensor op (argExpr x)
  Just (ReduceAndTensor, [x]) -> VReduceAndTensor (argExpr x)
  Just (ReduceOrTensor, [x]) -> VReduceOrTensor (argExpr x)
  Nothing -> case getDimensionDataOp expr of
    Just (ConstTensor, [x, y]) -> VConstBoolTensor (argExpr x) (argExpr y)
    Just (StackTensor n, args) -> VStackBoolTensor n (fmap argExpr args)
    _ -> developerError "ill-typed BoolTensor expression"
  _ -> developerError "ill-typed BoolTensor expression"

--------------------------------------------------------------------------------
-- Views on Rational Tensors

-- | A view on all possible expressions that can have type `Tensor Rat`.
data RatTensorView expr
  = VConstRatTensor expr expr
  | VRatTensor (Tensor Rational)
  | VRatTensorVar Lv
  | VStackRatTensor Int [expr]
  | VNegRatTensor expr
  | VAddRatTensor expr expr
  | VSubRatTensor expr expr
  | VMulRatTensor expr expr
  | VDivRatTensor expr expr
  | VMinRatTensor expr expr
  | VMaxRatTensor expr expr
  | VReduceAddRatTensor expr
  | VReduceMulRatTensor expr
  | VReduceMinRatTensor expr
  | VReduceMaxRatTensor expr
  | VSearchRatTensor expr expr expr expr

fromRatTensorView :: (BuiltinHasRatTensor builtin, BuiltinHasDimensionData builtin) => RatTensorView (WHNFValue builtin) -> WHNFValue builtin
fromRatTensorView = \case
  VRatTensor y -> INullaryRatTensorOp (RatTensor y)
  VNegRatTensor x -> IRatTensorOp NegRatTensor (explicit <$> [x])
  VAddRatTensor x y -> IRatTensorOp AddRatTensor (explicit <$> [x, y])
  VSubRatTensor x y -> IRatTensorOp SubRatTensor (explicit <$> [x, y])
  VMulRatTensor x y -> IRatTensorOp MulRatTensor (explicit <$> [x, y])
  VDivRatTensor x y -> IRatTensorOp DivRatTensor (explicit <$> [x, y])
  VMinRatTensor x y -> IRatTensorOp MinRatTensor (explicit <$> [x, y])
  VMaxRatTensor x y -> IRatTensorOp MaxRatTensor (explicit <$> [x, y])
  VReduceAddRatTensor x -> IRatTensorOp ReduceAddRatTensor (explicit <$> [x])
  VReduceMulRatTensor x -> IRatTensorOp ReduceMulRatTensor (explicit <$> [x])
  VReduceMinRatTensor x -> IRatTensorOp ReduceMinRatTensor (explicit <$> [x])
  VReduceMaxRatTensor x -> IRatTensorOp ReduceMaxRatTensor (explicit <$> [x])
  VConstRatTensor x y -> IRatConstTensor [x, y]
  VStackRatTensor n xs -> IDimensionDataOp (StackTensor n) (explicit <$> xs)
  VSearchRatTensor reduce lower upper fn -> IRatTensorOp SearchRatTensor (explicit <$> [reduce, lower, upper, fn])
  VRatTensorVar v -> VBoundVar v []

toRatTensorView :: (BuiltinHasRatTensor builtin, BuiltinHasDimensionData builtin) => WHNFValue builtin -> RatTensorView (WHNFValue builtin)
toRatTensorView expr = case getRatTensorOp expr of
  Just (RatTensor b, []) -> VRatTensor b
  Just (NegRatTensor, [x]) -> VNegRatTensor (argExpr x)
  Just (AddRatTensor, [x, y]) -> VAddRatTensor (argExpr x) (argExpr y)
  Just (SubRatTensor, [x, y]) -> VSubRatTensor (argExpr x) (argExpr y)
  Just (MulRatTensor, [x, y]) -> VMulRatTensor (argExpr x) (argExpr y)
  Just (DivRatTensor, [x, y]) -> VDivRatTensor (argExpr x) (argExpr y)
  Just (MinRatTensor, [x, y]) -> VMinRatTensor (argExpr x) (argExpr y)
  Just (MaxRatTensor, [x, y]) -> VMaxRatTensor (argExpr x) (argExpr y)
  Just (ReduceAddRatTensor, [x]) -> VReduceAddRatTensor (argExpr x)
  Just (ReduceMulRatTensor, [x]) -> VReduceMulRatTensor (argExpr x)
  Just (ReduceMinRatTensor, [x]) -> VReduceMinRatTensor (argExpr x)
  Just (ReduceMaxRatTensor, [x]) -> VReduceMaxRatTensor (argExpr x)
  Just (SearchRatTensor, [reduce, lower, upper, fn]) -> VSearchRatTensor (argExpr reduce) (argExpr lower) (argExpr upper) (argExpr fn)
  Nothing -> case getDimensionDataOp expr of
    Just (ConstTensor, [x, y]) -> VConstRatTensor (argExpr x) (argExpr y)
    Just (StackTensor n, args) -> VStackRatTensor n (fmap argExpr args)
    _ -> developerError "ill-typed RatTensor expression"
  _ -> developerError "ill-typed RatTensor expression"
-}
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
