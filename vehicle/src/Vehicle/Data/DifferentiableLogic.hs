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

instance HasName TensorDifferentiableLogicField Name where
  nameOf = \case
    TruthityElement -> "trueElement"
    FalsityElement -> "falseElement"
    PointwiseNegation -> "pointwiseNegation"
    PointwiseConjunction -> "pointwiseConjunction"
    PointwiseDisjunction -> "pointwiseDisjunction"
    PointwiseLe -> "pointwiseLessEqualThan"
    PointwiseLt -> "pointwiseLessThan"
    PointwiseGe -> "pointwiseGreaterEqualThan"
    PointwiseGt -> "pointwiseGreaterThan"
    PointwiseEq -> "pointwiseEqual"
    PointwiseNe -> "pointwiseNotEqual"
    ReduceConjunction -> "reduceConjunction"
    ReduceDisjunction -> "reduceDisjunction"

comparisonOpToField :: ComparisonOp -> TensorDifferentiableLogicField
comparisonOpToField = \case
  Le -> PointwiseLe
  Lt -> PointwiseLt
  Ge -> PointwiseGe
  Gt -> PointwiseGt
  Eq -> PointwiseEq
  Ne -> PointwiseNe

--------------------------------------------------------------------------------
-- Tensor implementation

type DifferentiableLogicImplementation =
  ( Map TensorDifferentiableLogicField (Expr LossBuiltin),
    LogicDirection
  )

elementLogicName :: Name
elementLogicName = "DifferentiableElementLogic"

tensorLogicName :: Name
tensorLogicName = "DifferentiableTensorLogic"
