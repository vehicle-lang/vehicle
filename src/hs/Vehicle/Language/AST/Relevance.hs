module Vehicle.Language.AST.Relevance where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

import Vehicle.Language.AST.Builtin

data Relevance
  = Relevant
  | Irrelevant
  deriving (Eq, Ord, Show, Generic)

instance NFData Relevance
instance Hashable Relevance

class HasRelevance a where
  relevanceOf :: a -> Relevance

isRelevant :: HasRelevance a => a -> Bool
isRelevant x = relevanceOf x == Relevant

isIrrelevant :: HasRelevance a => a -> Bool
isIrrelevant x = relevanceOf x == Irrelevant

instance HasRelevance TypeClass where
  relevanceOf = \case
    HasEq{}           -> Relevant
    HasOrd{}          -> Relevant
    HasNot{}          -> Relevant
    HasAnd{}          -> Relevant
    HasOr{}           -> Relevant
    HasImplies{}      -> Relevant
    HasQuantifier{}   -> Relevant
    HasAdd{}          -> Relevant
    HasSub{}          -> Relevant
    HasMul{}          -> Relevant
    HasDiv{}          -> Relevant
    HasNeg{}          -> Relevant
    HasFold{}         -> Relevant
    HasQuantifierIn{} -> Relevant
    HasNatLits{}      -> Relevant
    HasRatLits{}      -> Relevant
    HasVecLits{}      -> Relevant

    AlmostEqualConstraint{} -> Irrelevant
    NatInDomainConstraint{} -> Irrelevant
    MaxLinearity{}          -> Irrelevant
    MulLinearity{}          -> Irrelevant
    NegPolarity{}           -> Irrelevant
    AddPolarity{}           -> Irrelevant
    EqPolarity{}            -> Irrelevant
    ImpliesPolarity{}       -> Irrelevant
    MaxPolarity{}           -> Irrelevant
