
module Vehicle.Resource.NeuralNetwork where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import Data.Text(Text)
import Data.Hashable(Hashable(hash))

import Vehicle.Language.AST
import Vehicle.Prelude
import qualified Data.ByteString as ByteString

--------------------------------------------------------------------------------
-- Neural networks

type NetworkLocations = Map Text FilePath

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

hashNetwork :: MonadIO m => FilePath -> m Int
hashNetwork network = do
  contents <- liftIO $ ByteString.readFile network
  return $ hash contents


type MetaNetwork = [Symbol]