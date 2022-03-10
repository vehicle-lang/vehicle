
module Vehicle.NeuralNetwork where

import Data.Map (Map)
import Data.Text(Text)
import Data.Hashable(Hashable(hash))

import Vehicle.Language.AST
import Vehicle.Prelude
import qualified Data.ByteString as ByteString

--------------------------------------------------------------------------------
-- Neural networks

data NetworkLocation = NetworkLocation
  { networkName     :: Text
  , networkLocation :: FilePath
  } deriving (Show)

type NetworkMap = Map Identifier NetworkDetails

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

hashNetwork :: FilePath -> IO Int
hashNetwork network = do
  contents <- ByteString.readFile network
  return $ hash contents


type MetaNetwork = [Identifier]