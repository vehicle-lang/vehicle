{-# LANGUAGE CPP #-}

module Vehicle.Syntax.AST.Builtin.Linearity where

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..))
import Vehicle.Syntax.AST.Builtin.Core (FunctionPosition)
import Vehicle.Syntax.AST.Provenance (Provenance)

#if nothunks
import NoThunks.Class (NoThunks)
#endif

--------------------------------------------------------------------------------
-- LinearityProvenance

-- TODO
-- 1) rename LinearityProvenance to LinearityProof
-- 2) mimic AST nodes names
data LinearityProvenance
  = QuantifiedVariableProvenance !Provenance !Text
  | NetworkOutputProvenance !Provenance !Text
  | LinFunctionProvenance !Provenance !LinearityProvenance !FunctionPosition
  deriving (Generic)

#if nothunks
instance NoThunks LinearityProvenance
#endif

instance ToJSON LinearityProvenance

instance FromJSON LinearityProvenance

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
  | Linear !LinearityProvenance
  | NonLinear !Provenance !LinearityProvenance !LinearityProvenance
  deriving (Eq, Show, Generic)

instance Ord Linearity where
  Constant    <= _           = True
  Linear{}    <= Linear{}    = True
  Linear{}    <= NonLinear{} = True
  NonLinear{} <= NonLinear{} = True
  _           <= _           = False

#if nothunks
instance NoThunks Linearity
#endif

instance NFData Linearity

instance Hashable Linearity

instance ToJSON Linearity

instance FromJSON Linearity

instance Pretty Linearity where
  pretty = \case
    Constant    -> "Constant"
    Linear{}    -> "Linear"
    NonLinear{} -> "Non-linear"

mapLinearityProvenance :: (LinearityProvenance -> LinearityProvenance) -> Linearity -> Linearity
mapLinearityProvenance f = \case
  Constant           -> Constant
  Linear lp          -> Linear (f lp)
  -- At the moment we don't change non-linear provenance because we
  -- want the minimal example.
  NonLinear p lp lp' -> NonLinear p lp lp'

--------------------------------------------------------------------------------
-- Linearity constraints

data LinearityTypeClass
  = MaxLinearity
  | MulLinearity
  | FunctionLinearity !FunctionPosition
  | IfCondLinearity
  deriving (Eq, Generic, Show)

#if nothunks
instance NoThunks LinearityTypeClass
#endif

instance ToJSON LinearityTypeClass

instance FromJSON LinearityTypeClass

instance NFData LinearityTypeClass

instance Hashable LinearityTypeClass

instance Pretty LinearityTypeClass where
  pretty = \case
    MaxLinearity        -> "MaxLinearity"
    MulLinearity        -> "MulLinearity"
    IfCondLinearity     -> "IfCondLinearity"
    FunctionLinearity p -> "FunctionLinearity" <> pretty p
