{-# LANGUAGE CPP #-}

module Vehicle.Syntax.AST.Meta where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..))

#if nothunks
import NoThunks.Class (NoThunks)
#endif

--------------------------------------------------------------------------------
-- Meta-variables

newtype MetaID = MetaID Int
  deriving (Eq, Ord, Show, Generic)

#if nothunks
instance NoThunks   MetaID
#endif

instance NFData MetaID

instance Hashable MetaID

instance ToJSON MetaID

instance FromJSON MetaID

instance Pretty MetaID where
  pretty :: MetaID -> Doc ann
  pretty (MetaID m) = "?" <> pretty m
