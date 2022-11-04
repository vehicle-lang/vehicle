
module Vehicle.Language.AST.Meta where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Meta-variables

newtype Meta = MetaVar Int
  deriving (Eq, Ord, Show, Generic)

instance NFData   Meta
instance Hashable Meta

instance Pretty Meta where
  pretty (MetaVar m) = "?" <> pretty m

--------------------------------------------------------------------------------
-- Meta-variable substitution
