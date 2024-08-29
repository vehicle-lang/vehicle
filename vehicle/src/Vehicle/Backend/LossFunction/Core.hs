module Vehicle.Backend.LossFunction.Core where

import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss.Core
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Builtin.Tensor (TensorBuiltin)
import Vehicle.Data.Builtin.Tensor qualified as T
import Vehicle.Data.Code.Value (BoundEnv, NFValue, Spine, VBinder, VDecl, Value (..), WHNFClosure (..))
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))

--------------------------------------------------------------------------------
-- Closures

data LossClosure
  = LossClosure (BoundEnv MixedClosure LossBuiltin) (Expr LossBuiltin)

-- | Okay, so closures for loss functions are complicated. How compilation
-- currently works is that we first do standard normalisation on the `Builtin`
-- type, then recurse down to the closures converting `Builtin` to `LossBuiltin`.
--
-- This means that we need the standard closures from the first half.
-- However, when we convert a standard `Builtin` to the equivalent loss
-- expression, we need we apply the translated expression to the previous arguments.
--
--                subst                           evalApp
-- e.g _<=_ a b --------> (\x y -> x - y) a b   --------->    a - b
--
-- During the evalApp phase we may need to form closures over expressions that have
-- already been translated to loss functions. This means that we need the loss
-- closure constructor as well. In theory, if all arguments to the builtin are present
-- (i.e. the boolean operator is fully-applied) then these loss closures are only
-- formed temporarily and by the time `evalApp` has finally finished executing
-- they will no longer exist.
--
-- However, during the intermediary computation of `evalApp` both closures can exist
-- within the same expression. Maybe there's a nicer way of encoding this all in the
-- types that avoids this, but I haven't found it yet.
data MixedClosure
  = StandardClos (WHNFClosure Builtin)
  | LossClos LossClosure

type MixedBoundEnv = BoundEnv MixedClosure LossBuiltin

type MixedLossValue = Value MixedClosure LossBuiltin

type MixedLossSpine = Spine MixedClosure LossBuiltin

type MixedLossBinder = VBinder MixedClosure LossBuiltin

type MixedLossDecl = VDecl MixedClosure LossBuiltin

--------------------------------------------------------------------------------
-- Implementation

data DifferentiableLogicField
  = Bool
  | Truthity
  | Falsity
  | Conjunction
  | Disjunction
  | Negation
  | Implication
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq, Ord, Show, Generic)

instance Pretty DifferentiableLogicField where
  pretty = pretty . show

instance Hashable DifferentiableLogicField

type DifferentiableLogicImplementation =
  Map DifferentiableLogicField MixedLossValue

--------------------------------------------------------------------------------
-- Other

-- | Standard library operations that we don't want to normalise
-- as we need them present to convert into tensors.
preservedStdLibOps :: Set StdLibFunction
preservedStdLibOps =
  Set.fromList
    [ StdForeachIndex
    ]

constRatTensor :: Rational -> NFValue TensorBuiltin
constRatTensor v = VBuiltin (T.ConstRatTensor $ T.convertRat v) [explicit (VBuiltin T.NilList [])]
