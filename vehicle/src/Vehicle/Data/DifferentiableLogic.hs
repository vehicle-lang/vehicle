module Vehicle.Data.DifferentiableLogic where

import Data.Map (Map)
import GHC.Generics (Generic)
import Prettyprinter
import Vehicle.Data.AST.Expr.Scoped (Expr)
import Vehicle.Data.Builtin.Loss
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Logics
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Tensor implementation

data TensorDifferentiableLogicField
  = TruthityElement
  | FalsityElement
  | PointwiseNegation
  | PointwiseConjunction
  | PointwiseDisjunction
  | PointwiseComparison ComparisonOp
  | ReduceConjunction
  | ReduceDisjunction
  deriving (Eq, Ord, Show, Generic)

instance Pretty TensorDifferentiableLogicField where
  pretty = pretty . show

instance HasName TensorDifferentiableLogicField Name where
  nameOf = \case
    TruthityElement -> "trueElement"
    FalsityElement -> "falseElement"
    PointwiseNegation -> "pointwiseNegation"
    PointwiseConjunction -> "pointwiseConjunction"
    PointwiseDisjunction -> "pointwiseDisjunction"
    PointwiseComparison Lt -> "pointwiseLessThan"
    PointwiseComparison Le -> "pointwiseLessEqualThan"
    PointwiseComparison Gt -> "pointwiseGreaterThan"
    PointwiseComparison Ge -> "pointwiseGreaterEqualThan"
    PointwiseComparison Eq -> "pointwiseEqual"
    PointwiseComparison Ne -> "pointwiseNotEqual"
    ReduceConjunction -> "reduceConjunction"
    ReduceDisjunction -> "reduceDisjunction"

--------------------------------------------------------------------------------
-- Tensor implementation

type DifferentiableLogicImplementation mode =
  Map TensorDifferentiableLogicField (Expr (LossBuiltin mode))

elementLogicName :: Name
elementLogicName = "DifferentiableElementLogic"

tensorLogicName :: Name
tensorLogicName = "DifferentiableTensorLogic"
