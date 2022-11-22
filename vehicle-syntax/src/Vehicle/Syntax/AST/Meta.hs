
module Vehicle.Syntax.AST.Meta where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prettyprinter (Pretty(..))

--------------------------------------------------------------------------------
-- Meta-variables

newtype MetaID = MetaID Int
  deriving (Eq, Ord, Show, Generic)

instance NFData   MetaID
instance Hashable MetaID

instance Pretty MetaID where
  pretty (MetaID m) = "?" <> pretty m
