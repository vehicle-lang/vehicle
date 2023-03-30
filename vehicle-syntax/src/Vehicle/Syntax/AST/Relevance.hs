module Vehicle.Syntax.AST.Relevance where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Builtin

data Relevance
  = Relevant
  | Irrelevant
  deriving (Eq, Ord, Show, Generic)

instance NFData Relevance

instance Hashable Relevance

instance ToJSON Relevance

instance Serialize Relevance

class HasRelevance a where
  relevanceOf :: a -> Relevance

isRelevant :: (HasRelevance a) => a -> Bool
isRelevant x = relevanceOf x == Relevant

isIrrelevant :: (HasRelevance a) => a -> Bool
isIrrelevant x = relevanceOf x == Irrelevant

instance HasRelevance TypeClass where
  relevanceOf = \case
    HasEq {} -> Relevant
    HasOrd {} -> Relevant
    HasQuantifier {} -> Relevant
    HasAdd {} -> Relevant
    HasSub {} -> Relevant
    HasMul {} -> Relevant
    HasDiv {} -> Relevant
    HasNeg {} -> Relevant
    HasMap {} -> Relevant
    HasFold {} -> Relevant
    HasQuantifierIn {} -> Relevant
    HasNatLits {} -> Relevant
    HasRatLits {} -> Relevant
    HasVecLits {} -> Relevant
    NatInDomainConstraint {} -> Irrelevant
