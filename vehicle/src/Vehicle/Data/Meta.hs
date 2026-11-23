module Vehicle.Data.Meta where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------
-- Meta-variables

newtype MetaID = MetaID
  { unMetaID :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable, ToJSON, Serialize)

instance Pretty MetaID where
  pretty (MetaID m) = "?" <> pretty m
