{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Universe where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Universes

newtype UniverseLevel = UniverseLevel Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData, Hashable, Serialize)

instance Pretty UniverseLevel where
  pretty (UniverseLevel l) = "Type" <+> pretty l
