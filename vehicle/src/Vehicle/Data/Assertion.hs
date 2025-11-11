module Vehicle.Data.Assertion where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Generics
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr (ConjunctAll (..))
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Hashing ()
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Tensor (HasShape, RatTensor, Tensor, at)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude
import Vehicle.Syntax.Tensor
  ( HasShape (..),
    Tensor (..),
    compareTensor,
  )

class IsRelation relation where
  isRelated :: relation -> Tensor Rational -> Tensor Rational -> Bool

evalTrivialRelation :: (IsRelation relation, ConstantLike constant) => relation -> constant -> Bool
evalTrivialRelation rel constant = case toRatTensor constant of
  Just tensor -> isRelated rel tensor (ConstantTensor (shapeOf tensor) 0)
  -- Unsure if this is the right thing to do. We might need to split this up by
  -- implementation of `constant`.
  Nothing -> False

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

instance Pretty InequalityRelation where
  pretty = \case
    Strict -> "<"
    NonStrict -> "<="

instance IsRelation InequalityRelation where
  isRelated = \case
    NonStrict -> compareTensor (<=)
    Strict -> compareTensor (<)

prettyFlip :: InequalityRelation -> Doc a
prettyFlip = \case
  Strict -> ">"
  NonStrict -> ">="

combineInequalityRelations :: InequalityRelation -> InequalityRelation -> InequalityRelation
combineInequalityRelations r1 r2 = case (r1, r2) of
  (Strict, _) -> Strict
  (_, Strict) -> Strict
  (NonStrict, NonStrict) -> NonStrict

inequalityRelationToOp :: (Ord a) => InequalityRelation -> (a -> a -> Bool)
inequalityRelationToOp = \case
  Strict -> (<)
  NonStrict -> (<=)

--------------------------------------------------------------------------------
-- Equality relation

data EqualityRelation = EqualityRelation

instance IsRelation EqualityRelation where
  isRelated _ = compareTensor (==)

--------------------------------------------------------------------------------
-- Normalisation relations

-- TODO rename to `Comparison`?
data NormalisedRelation rel expr = NormalisedRelation
  { relation :: rel,
    expression :: expr
  }
  deriving (Show, Eq, Ord, Generic)

instance
  (NFData rel, NFData expr) =>
  NFData (NormalisedRelation rel expr)

instance
  (ToJSON rel, ToJSON expr) =>
  ToJSON (NormalisedRelation rel expr)

instance
  (FromJSON rel, FromJSON expr) =>
  FromJSON (NormalisedRelation rel expr)

instance (HasVariables expr variable) => HasVariables (NormalisedRelation rel expr) variable where
  variablesOf = variablesOf . expression
  containsVariable r v = expression r `containsVariable` v

instance (HasShape expr) => HasShape (NormalisedRelation rel expr) where
  shapeOf assertion = shapeOf (expression assertion)

instance (Negatable rel) => Negatable (NormalisedRelation rel expr) where
  neg (NormalisedRelation rel expr) = NormalisedRelation (neg rel) expr

eliminateVarsInComparison ::
  (VariableLike variable, ConstantLike constant, IsRelation relation) =>
  Map variable (LinearExpr variable constant) ->
  NormalisedRelation relation (LinearExpr variable constant) ->
  MaybeTrivial (NormalisedRelation relation (LinearExpr variable constant))
eliminateVarsInComparison f NormalisedRelation {..} =
  case eliminateVars f expression of
    Right newExpr -> NonTrivial $ NormalisedRelation {expression = newExpr, ..}
    Left tensor -> Trivial (evalTrivialRelation relation tensor)

reduceComparison ::
  (Monad m, Ord variable) =>
  Int ->
  (variable -> m [variable]) ->
  NormalisedRelation rel (LinearExpr variable RatTensor) ->
  m (Maybe (ConjunctAll (NormalisedRelation rel (LinearExpr variable RatTensor))))
reduceComparison lookupElementVariables dim (NormalisedRelation relation linearExpr) = do
  rationalEqualities <- reduceTensorExpr lookupElementVariables dim linearExpr
  let reducedComparison = fmap (NormalisedRelation relation) rationalEqualities
  return $ case reducedComparison of
    [] -> Nothing
    (v : vs) -> Just $ ConjunctAll (v :| vs)

reduceTensorExpr ::
  (Monad m, Ord variable) =>
  Int ->
  (variable -> m [variable]) ->
  LinearExpr variable RatTensor ->
  m [LinearExpr variable RatTensor]
reduceTensorExpr dim lookupElementVariables expr = do
  traverse (reduceLinearExprAt lookupElementVariables expr) [0 .. dim - 1]

reduceLinearExprAt ::
  (HasCallStack, Ord variable, Monad m) =>
  (variable -> m [variable]) ->
  LinearExpr variable RatTensor ->
  Int ->
  m (LinearExpr variable RatTensor)
reduceLinearExprAt lookupElementVariables (Sparse coeff constant) i = do
  newCoeffs <- mapKeysM (fmap (!! i) . lookupElementVariables) coeff
  return $
    Sparse
      { coefficients = newCoeffs,
        constantValue = constant `at` i
      }

--------------------------------------------------------------------------------
-- Assertions

type Inequality expr = NormalisedRelation InequalityRelation expr

type Equality expr = NormalisedRelation () expr

splitRelation ::
  NormalisedRelation Relation expr ->
  Either (Inequality expr) (Equality expr)
splitRelation r = case relation r of
  OEq -> Right $ r {relation = ()}
  OLe -> Left $ r {relation = NonStrict}
  OLt -> Left $ r {relation = Strict}

inequalityToNormRelation :: Inequality expr -> NormalisedRelation Relation expr
inequalityToNormRelation r = case relation r of
  Strict -> r {relation = OLt}
  NonStrict -> r {relation = OLe}

type Assertion expr = NormalisedRelation Relation expr

comparisonToAssertion ::
  (Monad m, VariableLike variable, ConstantLike constant) =>
  ComparisonOp ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  m (Either Bool (Assertion (LinearExpr variable constant)))
comparisonToAssertion op e1 e2 = do
  (rel, x, y) <- case op of
    Ne -> developerError "Cannot convert `Ne` to assertion"
    Eq -> return (OEq, e1, e2)
    Lt -> return (OLt, e1, e2)
    Le -> return (OLe, e1, e2)
    Gt -> return (OLt, e2, e1)
    Ge -> return (OLe, e2, e1)

  let constantOrExpr = addExprs 1 (-1) x y
  return $ bimap (evalTrivialRelation rel) (NormalisedRelation rel) constantOrExpr

type LinearSubstitution variable = Map variable (LinearExpr variable RatTensor)

equalityToAssertion :: Equality expr -> Assertion expr
equalityToAssertion (NormalisedRelation () e) = NormalisedRelation OEq e

getEquality :: Assertion expr -> Maybe (Equality expr)
getEquality (NormalisedRelation rel expr) = case rel of
  OEq -> Just (NormalisedRelation () expr)
  _ -> Nothing

getInequality :: Assertion expr -> Maybe (Inequality expr)
getInequality (NormalisedRelation rel expr) = case rel of
  OLe -> Just (NormalisedRelation NonStrict expr)
  OLt -> Just (NormalisedRelation Strict expr)
  _ -> Nothing
