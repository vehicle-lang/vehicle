module Vehicle.Data.Meta where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------
-- Meta-variables

newtype MetaID = MetaID Int
  deriving (Eq, Ord, Show, Generic)

instance NFData MetaID

instance Hashable MetaID

instance ToJSON MetaID

instance Serialize MetaID

instance Pretty MetaID where
  pretty (MetaID m) = "?" <> pretty m
