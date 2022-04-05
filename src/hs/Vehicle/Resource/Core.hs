
module Vehicle.Resource.Core where

import Prettyprinter
import Control.DeepSeq
import GHC.Generics (Generic)

data ResourceType
  = Network
  | Dataset
  deriving (Eq, Show, Generic)

instance NFData ResourceType

instance Pretty ResourceType where
  pretty = \case
    Network  -> "network"
    Dataset  -> "dataset"