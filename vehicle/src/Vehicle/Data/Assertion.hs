module Vehicle.Data.Assertion where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map (mapKeys)
import GHC.Generics
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr (ConjunctAll (..), MaybeTrivial (..))
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.Tensor (HasShape, RatTensor, Tensor, at)
import Vehicle.Prelude
import Vehicle.Syntax.Tensor
  ( HasShape (..),
    Tensor (..),
    compareTensor,
  )

class IsRelation relation where
  isRelated :: relation -> Tensor Rational -> Tensor Rational -> Bool

checkTriviality :: (IsRelation relation) => relation -> RatTensor -> Bool
checkTriviality rel tensor = isRelated rel tensor (ConstantTensor (shapeOf tensor) 0)

--------------------------------------------------------------------------------
-- Relations

data Relation
  = OEq
  | OLe
  | OLt
  deriving (Eq, Ord)

relationToComparisonOp :: Relation -> ComparisonOp
relationToComparisonOp = \case
  OEq -> Eq
  OLe -> Le
  OLt -> Lt

instance Pretty Relation where
  pretty = pretty . relationToComparisonOp

instance IsRelation Relation where
  isRelated = \case
    OLe -> compareTensor (<=)
    OLt -> compareTensor (<)
    OEq -> compareTensor (==)

--------------------------------------------------------------------------------
-- Strictness

data InequalityRelation
  = Strict
  | NonStrict
  deriving (Show, Eq, Ord, Generic)

instance NFData InequalityRelation

instance ToJSON InequalityRelation

instance FromJSON InequalityRelation

inequalityToRelation :: InequalityRelation -> Relation
inequalityToRelation = \case
  Strict -> OLe
  NonStrict -> OLt

instance Pretty InequalityRelation where
  pretty = pretty . inequalityToRelation

--------------------------------------------------------------------------------
-- Equality relation

data EqualityRelation = EqualityRelation

instance IsRelation EqualityRelation where
  isRelated _ = compareTensor (==)

--------------------------------------------------------------------------------
-- Normalisation relations

-- TODO rename to `Comparison`?
data NormalisedRelation rel variable constant = NormalisedRelation
  { relation :: rel,
    linearExpr :: LinearExpr variable constant
  }
  deriving (Show, Eq, Ord, Generic)

instance
  (NFData rel, NFData variable, NFData constant) =>
  NFData (NormalisedRelation rel variable constant)

instance
  (ToJSON rel, ToJSONKey variable, ToJSON constant) =>
  ToJSON (NormalisedRelation rel variable constant)

instance
  (Ord variable, FromJSON rel, FromJSONKey variable, FromJSON constant) =>
  FromJSON (NormalisedRelation rel variable constant)

instance (Ord variable) => HasVariables (NormalisedRelation rel variable constant) variable where
  variablesOf = variablesOf . linearExpr
  containsVariable r v = linearExpr r `containsVariable` v

eliminateVarsInComparison ::
  (VariableLike variable, IsRelation relation) =>
  LinearSubstitution variable ->
  NormalisedRelation relation variable RatTensor ->
  MaybeTrivial (NormalisedRelation relation variable RatTensor)
eliminateVarsInComparison f NormalisedRelation {..} = case eliminateVars f linearExpr of
  Right newExpr -> NonTrivial $ NormalisedRelation {linearExpr = newExpr, ..}
  Left tensor -> Trivial (checkTriviality relation tensor)

reduceComparison ::
  (Ord variable) =>
  Int ->
  (variable -> [variable]) ->
  NormalisedRelation rel variable RatTensor ->
  Maybe (ConjunctAll (NormalisedRelation rel variable RatTensor))
reduceComparison lookupElementVariables dim (NormalisedRelation relation linearExpr) = do
  let rationalEqualities = reduceTensorExpr lookupElementVariables dim linearExpr
  let reducedComparison = fmap (NormalisedRelation relation) rationalEqualities
  case reducedComparison of
    [] -> Nothing
    (v : vs) -> Just $ ConjunctAll (v :| vs)

reduceTensorExpr ::
  forall variable.
  (Ord variable) =>
  Int ->
  (variable -> [variable]) ->
  LinearExpr variable RatTensor ->
  [LinearExpr variable RatTensor]
reduceTensorExpr dim lookupElementVariables expr = do
  fmap (reduceLinearExprAt lookupElementVariables expr) [0 .. dim - 1]

reduceLinearExprAt ::
  (Ord variable) =>
  (variable -> [variable]) ->
  LinearExpr variable RatTensor ->
  Int ->
  LinearExpr variable RatTensor
reduceLinearExprAt lookupElementVariables (Sparse coeff constant) i =
  Sparse
    { coefficients = Map.mapKeys (\v -> lookupElementVariables v !! i) coeff,
      constantValue = constant `at` i
    }

--------------------------------------------------------------------------------
-- Assertions

type Inequality variable constant = NormalisedRelation InequalityRelation variable constant

type Equality variable constant = NormalisedRelation () variable constant

splitRelation ::
  NormalisedRelation Relation variable constant ->
  Either (Inequality variable constant) (Equality variable constant)
splitRelation r = case relation r of
  OEq -> Right $ r {relation = ()}
  OLe -> Left $ r {relation = NonStrict}
  OLt -> Left $ r {relation = Strict}

inequalityToNormRelation :: Inequality variable constant -> NormalisedRelation Relation variable constant
inequalityToNormRelation r = case relation r of
  Strict -> r {relation = OLt}
  NonStrict -> r {relation = OLe}

type Assertion variable = NormalisedRelation Relation variable RatTensor

instance HasShape (Assertion variable) where
  shapeOf assertion = shapeOf (constantValue $ linearExpr assertion)

comparisonToAssertion ::
  (VariableLike variable) =>
  ComparisonOp ->
  LinearExpr variable RatTensor ->
  LinearExpr variable RatTensor ->
  Assertion variable
comparisonToAssertion op e1 e2 = case op of
  Ne -> developerError "Cannot convert `Ne` to assertion"
  Eq -> NormalisedRelation OEq $ addExprs 1 (-1) e1 e2
  Lt -> NormalisedRelation OLt $ addExprs 1 (-1) e1 e2
  Le -> NormalisedRelation OLe $ addExprs 1 (-1) e1 e2
  Gt -> NormalisedRelation OLt $ addExprs (-1) 1 e1 e2
  Ge -> NormalisedRelation OLe $ addExprs (-1) 1 e1 e2

type LinearSubstitution variable = Map variable (LinearExpr variable RatTensor)

equalityToAssertion :: Equality variable RatTensor -> Assertion variable
equalityToAssertion (NormalisedRelation () e) = NormalisedRelation OEq e

getEquality :: Assertion variable -> Maybe (Equality variable RatTensor)
getEquality (NormalisedRelation rel expr) = case rel of
  OEq -> Just (NormalisedRelation () expr)
  _ -> Nothing

getInequality :: Assertion variable -> Maybe (Inequality variable RatTensor)
getInequality (NormalisedRelation rel expr) = case rel of
  OLe -> Just (NormalisedRelation NonStrict expr)
  OLt -> Just (NormalisedRelation Strict expr)
  _ -> Nothing

--------------------------------------------------------------------------------
-- Bounds

type Bound variable constant = Inequality variable constant

pattern Bound :: InequalityRelation -> LinearExpr variable constant -> Bound variable constant
pattern Bound s e = NormalisedRelation s e

{-# COMPLETE Bound #-}

type LowerBound variable constant = Bound variable constant

type UpperBound variable constant = Bound variable constant

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data Bounds variable constant = Bounds
  { lowerBounds :: [LowerBound variable constant],
    upperBounds :: [UpperBound variable constant]
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData variable, NFData constant) => NFData (Bounds variable constant)

instance (ToJSONKey variable, ToJSON constant) => ToJSON (Bounds variable constant)

instance (Ord variable, FromJSONKey variable, FromJSON constant) => FromJSON (Bounds variable constant)

--------------------------------------------------------------------------------
-- Variable status

data UnderConstrainedVariableStatus
  = Unconstrained
  | BoundedAbove
  | BoundedBelow
  deriving (Show, Eq, Ord)

instance Pretty UnderConstrainedVariableStatus where
  pretty = \case
    Unconstrained -> "no lower or upper bound"
    BoundedAbove -> "no lower bound"
    BoundedBelow -> "no upper bound"

instance Semigroup UnderConstrainedVariableStatus where
  Unconstrained <> r = r
  r <> Unconstrained = r
  BoundedAbove <> r = r
  r <> BoundedAbove = r
  BoundedBelow <> BoundedBelow = BoundedBelow

prettyUnderConstrainedVariable :: (Pretty var) => (var, UnderConstrainedVariableStatus) -> Doc a
prettyUnderConstrainedVariable (var, constraint) =
  pretty var <+> "-" <+> pretty constraint

checkBoundsExist ::
  (variable, Bounds variable constant) ->
  Either (variable, UnderConstrainedVariableStatus) (NonEmpty (LowerBound variable constant), NonEmpty (UpperBound variable constant))
checkBoundsExist (var, Bounds {..}) = case (lowerBounds, upperBounds) of
  ([], []) -> Left (var, Unconstrained)
  ([], _) -> Left (var, BoundedAbove)
  (_, []) -> Left (var, BoundedBelow)
  (l : ls, u : us) -> Right (l :| ls, u :| us)
