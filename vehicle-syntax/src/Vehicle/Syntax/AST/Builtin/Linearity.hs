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

--------------------------------------------------------------------------------
-- LinearityProvenance

-- TODO
-- 1) rename LinearityProvenance to LinearityProof
-- 2) mimic AST nodes names
data LinearityProvenance
  = QuantifiedVariableProvenance Provenance !Text
  | NetworkOutputProvenance Provenance !Text
  | LinFunctionProvenance Provenance LinearityProvenance !FunctionPosition
  deriving (Generic)

instance ToJSON LinearityProvenance

instance FromJSON LinearityProvenance

instance Show LinearityProvenance where
  show :: LinearityProvenance -> String
  show _x = ""

instance Eq LinearityProvenance where
  (==) :: LinearityProvenance -> LinearityProvenance -> Bool
  _x == _y = True

instance NFData LinearityProvenance where
  rnf :: LinearityProvenance -> ()
  rnf _x = ()

instance Hashable LinearityProvenance where
  hashWithSalt :: Int -> LinearityProvenance -> Int
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- Linearity

-- | Used to annotate numeric types, representing whether it represents a
-- constant, linear or non-linear expression.
data Linearity
  = Constant
  | Linear LinearityProvenance
  | NonLinear Provenance LinearityProvenance LinearityProvenance
  deriving (Eq, Show, Generic)

instance Ord Linearity where
  (<=) :: Linearity -> Linearity -> Bool
  Constant <= _ = True
  Linear {} <= Linear {} = True
  Linear {} <= NonLinear {} = True
  NonLinear {} <= NonLinear {} = True
  _ <= _ = False

instance NFData Linearity

instance Hashable Linearity

instance ToJSON Linearity

instance FromJSON Linearity

instance Pretty Linearity where
  pretty :: Linearity -> Doc ann
  pretty = \case
    Constant -> "Constant"
    Linear {} -> "Linear"
    NonLinear {} -> "Non-linear"

mapLinearityProvenance :: (LinearityProvenance -> LinearityProvenance) -> Linearity -> Linearity
mapLinearityProvenance f = \case
  Constant -> Constant
  Linear lp -> Linear (f lp)
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

instance ToJSON LinearityTypeClass

instance FromJSON LinearityTypeClass

instance NFData LinearityTypeClass

instance Hashable LinearityTypeClass

instance Pretty LinearityTypeClass where
  pretty :: LinearityTypeClass -> Doc ann
  pretty = \case
    MaxLinearity -> "MaxLinearity"
    MulLinearity -> "MulLinearity"
    IfCondLinearity -> "IfCondLinearity"
    FunctionLinearity p -> "FunctionLinearity" <> pretty p
