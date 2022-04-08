
module Vehicle.Resource.NeuralNetwork where

import Data.Map (Map)

import Vehicle.Language.AST
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Neural networks

type NetworkMap = Map Symbol NetworkDetails

data NetworkDetails = NetworkDetails
  { inputTensor  :: TensorDetails
  , outputTensor :: TensorDetails
  }

instance Pretty NetworkDetails where
  pretty (NetworkDetails input output) =
    "[input =" <+> pretty input <+> "output =" <+> pretty output <> "]"

data TensorDetails = TensorDetails
  { size  :: Int
  , tElem :: Builtin
  }

instance Pretty TensorDetails where
  pretty (TensorDetails size tElem) =
    "Tensor" <+> pretty tElem <+> "[" <> pretty size <> "]"

networkSize :: NetworkDetails -> Int
networkSize network = size (inputTensor network) + size (outputTensor network)

-- | Used to anntoate
data InputOrOutput
  = Input
  | Output
  deriving (Show, Eq)

instance Pretty InputOrOutput where
  pretty = \case
    Input  -> "input"
    Output -> "output"

allowedNetworkElementTypes :: [Builtin]
allowedNetworkElementTypes =
  [ BooleanType Bool
  , NumericType Nat
  , NumericType Int
  , NumericType Rat
  , NumericType Real
  ]

type MetaNetwork = [Symbol]