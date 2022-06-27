module Vehicle.Language.AST.Builtin.Linearity where

import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Language.AST.Provenance

--------------------------------------------------------------------------------
-- LinearityProvenance

data LinearityProvenance
  = QuantifiedVariableProvenance Provenance
  | NetworkOutputProvenance Provenance Symbol
  deriving (Generic)

instance Show LinearityProvenance where
  show _x = ""

instance Eq LinearityProvenance where
  _x == _y = True

instance NFData LinearityProvenance where
  rnf _x = ()

instance Hashable LinearityProvenance where
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- Linearity

-- | Used to annotate numeric types, representing whether it represents a
-- constant, linear or non-linear expression.
data Linearity
  = Constant
  | Linear LinearityProvenance
  | NonLinear LinearityProvenance LinearityProvenance
  deriving (Eq, Show, Generic)

instance Ord Linearity where
  Constant    <= _           = True
  Linear{}    <= Linear{}    = True
  Linear{}    <= NonLinear{} = True
  NonLinear{} <= NonLinear{} = True
  _           <= _           = False

instance NFData   Linearity
instance Hashable Linearity

instance Pretty Linearity where
  pretty = \case
    Constant    -> "Constant"
    Linear{}    -> "Linear"
    NonLinear{} -> "Non-linear"

maxLinearity :: Linearity -> Linearity -> Linearity
maxLinearity l1 l2 = if l1 >= l2 then l1 else l2

mulLinearity :: Linearity -> Linearity -> Linearity
mulLinearity l1 l2 = case (l1, l2) of
  (Constant, _)    -> l2
  (_, Constant)    -> l1
  (NonLinear{}, _) -> l1
  (_, NonLinear{}) -> l2
  (_, _)           -> l1