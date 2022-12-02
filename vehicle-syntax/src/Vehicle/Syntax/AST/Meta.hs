
module Vehicle.Syntax.AST.Meta where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------
-- Meta-variables

newtype MetaID = MetaID Int
  deriving (Eq, Ord, Show, Generic, NoThunks)

instance NFData   MetaID
instance Hashable MetaID
instance ToJSON   MetaID
instance FromJSON MetaID

instance Pretty MetaID where
  pretty (MetaID m) = "?" <> pretty m
