{-# LANGUAGE CPP #-}

module Vehicle.Syntax.AST.Relevance where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Builtin.TypeClass (TypeClass (..))



data Relevance
  = Relevant
  | Irrelevant
  deriving (Eq, Ord, Show, Generic)

instance NFData Relevance
instance Hashable Relevance
instance ToJSON Relevance
instance FromJSON Relevance

class HasRelevance a where
  relevanceOf :: a -> Relevance

isRelevant :: HasRelevance a => a -> Bool
isRelevant x = relevanceOf x == Relevant

isIrrelevant :: HasRelevance a => a -> Bool
isIrrelevant x = relevanceOf x == Irrelevant

instance HasRelevance TypeClass where
  relevanceOf = \case
    HasEq{}                 -> Relevant
    HasOrd{}                -> Relevant
    HasNot{}                -> Relevant
    HasAnd{}                -> Relevant
    HasOr{}                 -> Relevant
    HasImplies{}            -> Relevant
    HasQuantifier{}         -> Relevant
    HasAdd{}                -> Relevant
    HasSub{}                -> Relevant
    HasMul{}                -> Relevant
    HasDiv{}                -> Relevant
    HasNeg{}                -> Relevant
    HasFold{}               -> Relevant
    HasQuantifierIn{}       -> Relevant
    HasNatLits{}            -> Relevant
    HasRatLits{}            -> Relevant
    HasVecLits{}            -> Relevant
    HasIf{}                 -> Irrelevant
    AlmostEqualConstraint{} -> Irrelevant
    NatInDomainConstraint{} -> Irrelevant
    LinearityTypeClass{}    -> Irrelevant
    PolarityTypeClass{}     -> Irrelevant
