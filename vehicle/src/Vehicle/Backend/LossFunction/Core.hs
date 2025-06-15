module Vehicle.Backend.LossFunction.Core where

import Data.Hashable (Hashable)
import Data.Map (Map)
import GHC.Generics (Generic)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Code.Value (BoundEnv, Closure (..), VBinder, Value (..))

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
  Map TensorDifferentiableLogicField (Value LossBuiltin)

type CompiledDifferentiableLogic = (DifferentiableLogicID, DifferentiableLogicImplementation)

--------------------------------------------------------------------------------
-- Other

pattern VLam2 :: VBinder builtin -> BoundEnv builtin -> Binder builtin -> Expr builtin -> Value builtin
pattern VLam2 binder1 env binder2 body <- VLam binder1 (Closure env (Lam _ binder2 body))
