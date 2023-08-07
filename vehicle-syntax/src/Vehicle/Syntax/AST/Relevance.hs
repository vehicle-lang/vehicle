module Vehicle.Syntax.AST.Relevance where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Syntax.Builtin

--------------------------------------------------------------------------------
-- Data

data Relevance
  = Relevant
  | Irrelevant
  deriving (Eq, Ord, Show, Generic)

instance NFData Relevance

instance Hashable Relevance

instance ToJSON Relevance

instance Serialize Relevance

instance Semigroup Relevance where
  Relevant <> Relevant = Relevant
  _ <> _ = Irrelevant

instance Monoid Relevance where
  mempty = Relevant

--------------------------------------------------------------------------------
-- Type class

class HasRelevance a where
  relevanceOf :: a -> Relevance

isRelevant :: (HasRelevance a) => a -> Bool
isRelevant x = relevanceOf x == Relevant

isIrrelevant :: (HasRelevance a) => a -> Bool
isIrrelevant x = relevanceOf x == Irrelevant
