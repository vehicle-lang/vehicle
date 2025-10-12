{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Index
  ( Ix (Ix, InnerIx, unIx),
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Generics (Generic)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Indices

-- | A DeBruijn index pointing to the binder that the variable refers to,
-- counting from the variable position upwards.
newtype Ix = InnerIx
  { unIx :: Int
  }
  deriving (Eq, Ord, Num, Enum, Show, Generic)

pattern Ix :: Int -> Ix
pattern Ix i <- InnerIx i
  where
    Ix i = checkIx i

checkIx :: (HasCallStack) => Int -> Ix
checkIx i
  | i < 0 = developerError $ "malformed ix" <+> pretty i
  | otherwise = InnerIx i

instance NFData Ix

instance Hashable Ix

instance Serialize Ix

instance Pretty Ix where
  pretty i = "𝓲" <> pretty (unIx i)
