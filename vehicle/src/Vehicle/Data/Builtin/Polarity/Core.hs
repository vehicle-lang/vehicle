module Vehicle.Data.Builtin.Polarity.Core where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Compile.Print (PrintableBuiltin (..))
import Vehicle.Data.Builtin.Interface (ConvertableBuiltin (..), HasStandardData (..))
import Vehicle.Data.DSL
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Prelude (layoutAsText)
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin hiding (Builtin (BuiltinConstructor, BuiltinFunction))
import Vehicle.Syntax.Builtin qualified as S

--------------------------------------------------------------------------------
-- PolarityProvenance

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance Provenance
  | NegateProvenance Provenance PolarityProvenance
  | LHSImpliesProvenance Provenance PolarityProvenance
  | EqProvenance Provenance PolarityProvenance EqualityOp
  | PolFunctionProvenance Provenance PolarityProvenance FunctionPosition
  deriving (Generic)

instance Serialize PolarityProvenance

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
  | -- | Stores the provenance of the `Forall` first followed by the `Exists`.
    MixedParallel PolarityProvenance PolarityProvenance
  | -- | Stores the type and provenance of the top-most quantifier first.
    MixedSequential Quantifier Provenance PolarityProvenance
  deriving (Eq, Generic, Show)

instance NFData Polarity

instance Hashable Polarity

instance Serialize Polarity

instance Pretty Polarity where
  pretty = \case
    Unquantified -> "Unquantified"
    Quantified q _ -> "Quantified" <+> pretty q
    MixedParallel {} -> "MixedParallel"
    MixedSequential {} -> "MixedSequential"

mapPolarityProvenance :: (PolarityProvenance -> PolarityProvenance) -> Polarity -> Polarity
mapPolarityProvenance f = \case
  Unquantified -> Unquantified
  Quantified q pp -> Quantified q (f pp)
  MixedParallel pp1 pp2 -> MixedParallel (f pp1) (f pp2)
  -- At the moment we don't change non-linear provenance because we
  -- want the minimal example.
  MixedSequential q p pp -> MixedSequential q p pp

--------------------------------------------------------------------------------
-- Polarity relationship

data PolarityRelation
  = NegPolarity
  | QuantifierPolarity Quantifier
  | AddPolarity Quantifier
  | EqPolarity EqualityOp
  | ImpliesPolarity
  | IfPolarity
  | MaxPolarity
  | FunctionPolarity FunctionPosition
  deriving (Eq, Generic, Show)

instance Serialize PolarityRelation

instance NFData PolarityRelation

instance Hashable PolarityRelation

instance Pretty PolarityRelation where
  pretty = \case
    NegPolarity -> "NegPolarity"
    AddPolarity q -> "AddPolarity" <+> pretty q
    QuantifierPolarity q -> "QuantifierPolarity" <+> pretty q
    EqPolarity eq -> "EqPolarity" <+> pretty eq
    ImpliesPolarity -> "ImpliesPolarity"
    MaxPolarity -> "MaxPolarity"
    IfPolarity -> "IfPolarity"
    FunctionPolarity p -> "FunctionPolarity" <> pretty p

--------------------------------------------------------------------------------
-- Builtin type

data PolarityBuiltin
  = BuiltinConstructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | Polarity Polarity
  | PolarityRelation PolarityRelation
  deriving (Show, Eq, Generic)

instance Hashable PolarityBuiltin

instance Pretty PolarityBuiltin where
  pretty = \case
    BuiltinConstructor c -> pretty c
    BuiltinFunction f -> pretty f
    Polarity l -> pretty l
    PolarityRelation c -> pretty c

instance HasStandardData PolarityBuiltin where
  mkBuiltinFunction = BuiltinFunction
  getBuiltinFunction = \case
    BuiltinFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = BuiltinConstructor
  getBuiltinConstructor = \case
    BuiltinConstructor c -> Just c
    _ -> Nothing

  getBuiltinTypeClassOp = const Nothing

instance HasBoolLits (Value closure PolarityBuiltin) where
  mkBoolLit _p b = VBuiltin (BuiltinConstructor (LBool b)) []
  getBoolLit = \case
    VBuiltin (BuiltinConstructor (LBool b)) [] -> Just (mempty, b)
    _ -> Nothing

instance HasIndexLits (Value closure PolarityBuiltin) where
  getIndexLit e = case e of
    VBuiltin (BuiltinConstructor (LIndex n)) [] -> Just (mempty, n)
    _ -> Nothing
  mkIndexLit _p x = VBuiltin (BuiltinConstructor (LIndex x)) mempty

instance HasNatLits (Value closure PolarityBuiltin) where
  getNatLit e = case e of
    VBuiltin (BuiltinConstructor (LNat b)) [] -> Just (mempty, b)
    _ -> Nothing
  mkNatLit _p x = VBuiltin (BuiltinConstructor (LNat x)) mempty

instance HasRatLits (Value closure PolarityBuiltin) where
  getRatLit e = case e of
    VBuiltin (BuiltinConstructor (LRat b)) [] -> Just (mempty, b)
    _ -> Nothing
  mkRatLit _p x = VBuiltin (BuiltinConstructor (LRat x)) mempty

instance ConvertableBuiltin PolarityBuiltin S.Builtin where
  convertBuiltin p = \case
    BuiltinConstructor c -> Builtin p (S.BuiltinConstructor c)
    BuiltinFunction f -> Builtin p (S.BuiltinFunction f)
    b -> FreeVar p $ stdlibIdentifier (layoutAsText $ pretty b)

instance PrintableBuiltin PolarityBuiltin where
  isCoercion = const False

-----------------------------------------------------------------------------
-- Type synonyms

pattern PolarityExpr :: Provenance -> Polarity -> Expr var PolarityBuiltin
pattern PolarityExpr p pol = Builtin p (Polarity pol)

pattern VPolarityExpr :: Polarity -> WHNFValue PolarityBuiltin
pattern VPolarityExpr l <- VBuiltin (Polarity l) []
  where
    VPolarityExpr l = VBuiltin (Polarity l) []

-----------------------------------------------------------------------------
-- DSL

type PolarityDSLExpr = DSLExpr PolarityBuiltin

forAllPolarities :: (PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarities f = forAll "p" tPol $ \p -> f p

forAllPolarityPairs :: (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarityPairs f =
  forAll "p1" tPol $ \p1 ->
    forAll "p2" tPol $ \p2 ->
      f p1 p2

forAllPolarityTriples :: (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarityTriples f =
  forAll "p1" tPol $ \p1 ->
    forAll "p2" tPol $ \p2 ->
      forAll "p3" tPol $ \p3 ->
        f p1 p2 p3

unquantified :: PolarityDSLExpr
unquantified = builtin (Polarity Unquantified)

polarityTypeClass :: PolarityRelation -> NonEmpty PolarityDSLExpr -> PolarityDSLExpr
polarityTypeClass tc args = builtin (PolarityRelation tc) @@ args

quantifierPolarity :: Quantifier -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
quantifierPolarity q l1 l2 = polarityTypeClass (QuantifierPolarity q) [l1, l2]

maxPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
maxPolarity l1 l2 l3 = polarityTypeClass MaxPolarity [l1, l2, l3]

ifPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
ifPolarity l1 l2 l3 l4 = polarityTypeClass IfPolarity [l1, l2, l3, l4]

eqPolarity :: EqualityOp -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
eqPolarity eq p1 p2 p3 = polarityTypeClass (EqPolarity eq) [p1, p2, p3]

impliesPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
impliesPolarity l1 l2 l3 = polarityTypeClass ImpliesPolarity [l1, l2, l3]

negPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
negPolarity l1 l2 = polarityTypeClass NegPolarity [l1, l2]

tPol :: PolarityDSLExpr
tPol = type0
