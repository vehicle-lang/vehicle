module Vehicle.Backend.LossFunction.Core where

import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss.Core
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Expr.Normalised (BoundEnv, Spine, VBinder, VDecl, Value, WHNFClosure (..))
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))

--------------------------------------------------------------------------------
-- Closures

data LossClosure
  = LossClosure (BoundEnv MixedClosure LossBuiltin) (Expr Ix LossBuiltin)

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
