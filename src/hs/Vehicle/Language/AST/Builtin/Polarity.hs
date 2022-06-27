module Vehicle.Language.AST.Builtin.Polarity
  ( PolarityProvenance(..)
  , Polarity(..)
  , negatePolarity
  , addPolarity
  , maxPolarity
  , eqPolarity
  , implPolarity
  ) where

import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Language.AST.Provenance
import Vehicle.Language.AST.Builtin.Core

--------------------------------------------------------------------------------
-- PolarityProvenance

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance Provenance
  | NegateProvenance     Provenance PolarityProvenance
  | LHSImpliesProvenance Provenance PolarityProvenance
  | EqProvenance         Equality Provenance PolarityProvenance
  deriving (Generic)

instance Show PolarityProvenance where
  show _x = ""

instance Eq PolarityProvenance where
  _x == _y = True

instance NFData PolarityProvenance where
  rnf _x = ()

instance Hashable PolarityProvenance where
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- Polarity

-- | Used to annotate boolean types, represents what sort of pattern of
-- quantifiers it contains.
data Polarity
  = Unquantified
  | Quantified Quantifier PolarityProvenance
  -- | Stores the provenance of the `Forall` first followed by the `Exists`.
  | MixedParallel PolarityProvenance PolarityProvenance
  -- | Stores the type and provenance of the top-most quantifier first.
  | MixedSequential Quantifier Provenance PolarityProvenance
  deriving (Eq, Generic)

instance NFData Polarity
instance Hashable Polarity

instance Pretty Polarity where
  pretty = \case
    Unquantified      -> "Unquantified"
    Quantified q _    -> "Quantified" <+> pretty q
    MixedParallel{}   -> "MixedParallel"
    MixedSequential{} -> "MixedSequential"

instance Show Polarity where
  show = layoutAsString . pretty

negPolarity :: (PolarityProvenance -> PolarityProvenance)
            -> Polarity
            -> Polarity
negPolarity modProv pol =
  case pol of
    Unquantified              -> Unquantified
    Quantified      q pp      -> Quantified (neg q) (modProv pp)
    MixedParallel     pp1 pp2 -> MixedParallel (modProv pp2) (modProv pp1)
    -- We don't negate a mixed sequential polarity as its the top of the polarity
    -- lattice and we want to give as meaningful and localised error messages
    -- as possible.
    MixedSequential{}     -> pol

negatePolarity :: Provenance
               -> Polarity
               -> Polarity
negatePolarity p = negPolarity (NegateProvenance p)

addPolarity :: Provenance -> Quantifier -> Polarity -> Polarity
addPolarity p q pol = case pol of
  Unquantified          -> Quantified q (QuantifierProvenance p)
  Quantified q' pp      -> if q == q' then pol else MixedSequential q p pp
  MixedParallel pp1 pp2 -> MixedSequential q p (if q == Forall then pp2 else pp1)
  MixedSequential{}     -> pol

maxPolarity :: Polarity -> Polarity -> Polarity
maxPolarity pol1 pol2 = case (pol1, pol2) of
  (Unquantified,      _)            -> pol2
  (_,                 Unquantified) -> pol1
  (Quantified q1 pp1, Quantified q2 pp2)
    | q1 == q2     -> pol1
    | q1 == Forall -> MixedParallel pp1 pp2
    | otherwise    -> MixedParallel pp2 pp1
  (Quantified{},      MixedParallel{})   -> pol2
  (MixedParallel{},   Quantified{})      -> pol1
  (MixedParallel{},   MixedParallel{})   -> pol1
  (MixedSequential{}, _)                 -> pol1
  (_,                 MixedSequential{}) -> pol2

eqPolarity :: Equality
           -> Provenance
           -> Polarity
           -> Polarity
           -> Polarity
eqPolarity eq p pol1 pol2 =
  let negPol = negPolarity (EqProvenance eq p) in
  -- `a == b` = (a and b) or (not a and not b)
  maxPolarity
    (maxPolarity pol1 pol2)
    (maxPolarity (negPol pol1) (negPol pol2))

implPolarity :: Provenance
             -> Polarity
             -> Polarity
             -> Polarity
implPolarity p pol1 pol2 =
  let negPol = negPolarity (LHSImpliesProvenance p) in
  -- `a => b` = not a or b
  maxPolarity (negPol pol1) pol2