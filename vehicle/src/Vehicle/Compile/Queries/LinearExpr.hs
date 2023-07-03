module Vehicle.Compile.Queries.LinearExpr
  ( Relation (..),
    Coefficient,
    Assertion (..),
    UnreducedAssertion (..),
    VectorEquality (..),
    originalVectorEqualityExpr,
    isVectorEqualityAssertion,
    assertionToVectorEquality,
    CLSTProblem (..),
    SparseLinearExpr (..),
    lookupCoefficient,
    evaluateExpr,
    addExprs,
    scaleExpr,
    rearrangeExprToSolveFor,
    constructReducedAssertion,
    isEquality,
    prettyAssertions,
    referencesVariables,
    substitute,
    filterTrivialAssertions,
    eliminateVar,
    prettyCoefficient,
    convertToSparseFormat,
    mapAssertionVariables,
    ordToRelation,
  )
where

import Control.Monad (foldM)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import Numeric qualified
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Type.Subsystem.Standard.Core (StandardNormExpr)

--------------------------------------------------------------------------------
-- Relation

data Relation
  = Equal
  | LessThan
  | LessThanOrEqualTo
  deriving (Show, Eq, Generic)

instance Hashable Relation

instance ToJSON Relation

instance FromJSON Relation

instance Pretty Relation where
  pretty = \case
    Equal -> "=="
    LessThan -> "<"
    LessThanOrEqualTo -> "<="

relationToOp :: Relation -> Either () OrderOp
relationToOp = \case
  Equal -> Left ()
  LessThan -> Right Lt
  LessThanOrEqualTo -> Right Le

ordToRelation ::
  StandardNormExpr ->
  OrderOp ->
  StandardNormExpr ->
  (StandardNormExpr, Relation, StandardNormExpr)
ordToRelation e1 op e2 = case op of
  Lt -> (e1, LessThan, e2)
  Le -> (e1, LessThanOrEqualTo, e2)
  Gt -> (e2, LessThan, e1)
  Ge -> (e2, LessThanOrEqualTo, e1)

--------------------------------------------------------------------------------
-- Variables

type Coefficient = Double

prettyCoefficientVar ::
  (Variable variable) =>
  Bool ->
  (variable, Coefficient) ->
  Doc a
prettyCoefficientVar isFirst (variable, coefficient) = do
  let sign
        | coefficient > 0 = if isFirst then "" else "+ "
        | otherwise = if isFirst then "-" else "- "

  let value
        | coefficient == 1 = pretty variable
        | coefficient == -1 = pretty variable
        | coefficient > 0 = prettyCoefficient coefficient <> pretty variable
        | otherwise = prettyCoefficient (-coefficient) <> pretty variable

  sign <> value

prettyCoefficient :: Coefficient -> Doc a
prettyCoefficient v = pretty $ Numeric.showFFloat Nothing v ""

--------------------------------------------------------------------------------
-- Sparse representations of linear expressions

data SparseLinearExpr variable = Sparse
  { dimensions :: TensorDimensions,
    coefficients :: Map variable Coefficient,
    constantValue :: Constant
  }
  deriving (Show, Eq, Generic)

instance (Variable variable, ToJSONKey variable) => ToJSON (SparseLinearExpr variable)

instance (Variable variable, FromJSONKey variable) => FromJSON (SparseLinearExpr variable)

instance (Variable variable) => Pretty (SparseLinearExpr variable) where
  pretty (Sparse dims coefficients constant) = do
    -- Append an empty variable name for the constant at the end
    let coeffVars = Map.toList coefficients
    case coeffVars of
      [] -> prettyConstant True dims constant
      (x : xs) -> do
        let varDocs = prettyCoefficientVar True x : fmap (prettyCoefficientVar False) xs
        let constDoc = prettyConstant False dims constant
        hsep varDocs <> constDoc

addExprs ::
  (Variable variable) =>
  Coefficient ->
  SparseLinearExpr variable ->
  Coefficient ->
  SparseLinearExpr variable ->
  SparseLinearExpr variable
addExprs c1 (Sparse dims coeff1 const1) c2 (Sparse _ coeff2 const2) = do
  -- We should really be able to do this in one operation, but the API isn't flexible enough.
  let coeff1' = if c1 == 1 then coeff1 else Map.map (c1 *) coeff1
  let coeff2' = if c2 == 1 then coeff2 else Map.map (c2 *) coeff2
  let rcoeff = Map.filter (/= 0) (Map.unionWith (+) coeff1' coeff2')
  let rconst = addConstants c1 const1 c2 const2
  Sparse dims rcoeff rconst

scaleExpr :: Coefficient -> SparseLinearExpr variable -> SparseLinearExpr variable
scaleExpr c (Sparse dims coefficients constant) =
  Sparse dims (Map.map (c *) coefficients) (scaleConstant c constant)

lookupCoefficient :: (Ord variable) => SparseLinearExpr variable -> variable -> Coefficient
lookupCoefficient (Sparse _ coefficients _) v = fromMaybe 0 $ Map.lookup v coefficients

isConstant :: SparseLinearExpr variable -> Maybe Constant
isConstant (Sparse _ coeff constant)
  | Map.null coeff = Just constant
  | otherwise = Nothing

toSparse :: SparseLinearExpr variable -> SparseLinearExpr variable
toSparse = id

evaluateExpr ::
  forall variable.
  (Variable variable) =>
  SparseLinearExpr variable ->
  VariableAssignment variable ->
  Either variable Constant
evaluateExpr expr assignment = do
  let Sparse _ coefficients constant = toSparse expr
  foldM op constant (Map.toList coefficients)
  where
    op :: Constant -> (variable, Coefficient) -> Either variable Constant
    op total (var, coeff) = case Map.lookup var assignment of
      Nothing -> Left var
      Just value -> Right (addConstants 1 total coeff value)

eliminateVar ::
  (Variable variable) =>
  variable ->
  SparseLinearExpr variable ->
  SparseLinearExpr variable ->
  SparseLinearExpr variable
eliminateVar var solution row = do
  let varCoefficient = lookupCoefficient row var
  if varCoefficient == 0
    then row
    else do
      let scaleFactor = varCoefficient / lookupCoefficient solution var
      let resultExpr = addExprs 1 row (-scaleFactor) solution
      -- Needed to handle floating point errors....
      resultExpr
        { coefficients = Map.delete var $ coefficients resultExpr
        }

-- | Takes an assertion `a*x_0 + ... + b*x_i + ... c * x_n` and
-- returns the RHS of the equation: `x_i = -a/b*x_0 + ... -c/b*x_n`
rearrangeExprToSolveFor ::
  (Variable variable) =>
  variable ->
  SparseLinearExpr variable ->
  SparseLinearExpr variable
rearrangeExprToSolveFor var expr = do
  let c = lookupCoefficient expr var
  let scaledExpr = scaleExpr (-1 / c) expr
  scaledExpr
    { coefficients = Map.delete var $ coefficients scaledExpr
    }

mapExprVariables :: (Variable variable2) => (variable1 -> variable2) -> SparseLinearExpr variable1 -> SparseLinearExpr variable2
mapExprVariables f Sparse {..} =
  Sparse
    { coefficients = Map.mapKeys f coefficients,
      ..
    }

--------------------------------------------------------------------------------
-- Assertion

data Assertion variable = Assertion
  { -- | How the sum of the terms in the linear expression are related.
    assertionRel :: Relation,
    assertionExpr :: SparseLinearExpr variable
  }
  deriving (Show, Eq, Generic)

instance (Variable variable, ToJSONKey variable) => ToJSON (Assertion variable)

instance (Variable variable, FromJSONKey variable) => FromJSON (Assertion variable)

instance (Variable variable) => Pretty (Assertion variable) where
  pretty (Assertion rel linearExpr) =
    pretty linearExpr <+> pretty rel <+> "0.0"

isEquality :: Assertion variable -> Bool
isEquality a = assertionRel a == Equal

constructReducedAssertion ::
  (Variable variable) =>
  (SparseLinearExpr variable, Relation, SparseLinearExpr variable) ->
  Assertion variable
constructReducedAssertion (lhs, rel, rhs) =
  Assertion
    { assertionExpr = addExprs 1 lhs (-1) rhs,
      assertionRel = rel
    }

prettyAssertions :: (Variable variable) => [Assertion variable] -> Doc a
prettyAssertions assertions =
  vsep (fmap pretty assertions)

substitute :: (Variable variable) => Assertion variable -> variable -> SparseLinearExpr variable -> Assertion variable
substitute (Assertion r2 e2) var e1 = Assertion r2 (eliminateVar var e1 e2)

referencesVariables :: (Variable variable) => Assertion variable -> Set variable -> Bool
referencesVariables (Assertion _ e) variables = do
  let presentVariables = Set.fromList $ Map.keys $ coefficients e
  not $ Set.null $ variables `Set.intersection` presentVariables

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkTriviality :: Assertion variable -> Maybe Bool
checkTriviality (Assertion rel linexp) =
  case isConstant linexp of
    Nothing -> Nothing
    Just c -> Just $ case rel of
      Equal -> Vector.all (== 0.0) c
      LessThan -> Vector.all (< 0.0) c
      LessThanOrEqualTo -> Vector.all (<= 0.0) c

filterTrivialAssertions ::
  [Assertion variable] ->
  Maybe [Assertion variable]
filterTrivialAssertions = go
  where
    go :: [Assertion variable] -> Maybe [Assertion variable]
    go [] = Just []
    go (a : as) = case go as of
      Nothing -> Nothing
      Just as' -> case checkTriviality a of
        Nothing -> Just $ a : as'
        Just True -> Just as'
        Just False -> Nothing

-- | Converts an assertion to a sparse format, with various optimisations
-- to improve readability, including:
--   1. sorting the variables alphabetically
--   2. negating everything if all variables have negative coefficients.
--   3. moving the constant to the RHS.
convertToSparseFormat ::
  Map NetworkVariable Name ->
  Assertion NetworkVariable ->
  (NonEmpty (Coefficient, Name), Either () OrderOp, Double)
convertToSparseFormat nameMap (Assertion rel linearExpr) = do
  let Sparse _ coeffs vectConstant = toSparse linearExpr
  let varCoeff = sortVarCoeffs coeffs
  let missingErr v = developerError $ "Unknown variable" <+> pretty v <+> "when converting to sparse format"
  let coeffName = fmap (\(v, c) -> (c, Map.findWithDefault (missingErr v) v nameMap)) varCoeff
  let op = relationToOp rel
  let constant = vectConstant Vector.! 0

  -- Move constant to RHS
  let rhsConstant = -constant

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let allCoefficientsNegative = all (\(c, _) -> c < 0) coeffName
  let (almostFinalCoeff, finalOp, almostFinalConstant) =
        if not allCoefficientsNegative
          then (coeffName, op, rhsConstant)
          else do
            let negCoeffNames = fmap (\(c, v) -> (-c, v)) coeffName
            let negOp = second flipOrder op
            let negConstant = -rhsConstant
            (negCoeffNames, negOp, negConstant)

  -- Also check for and remove `-0.0`s for cleanliness.
  let finalConstant =
        if isNegativeZero almostFinalConstant
          then 0.0
          else almostFinalConstant

  let finalCoeff = case almostFinalCoeff of
        (c : cs) -> c :| cs
        [] -> developerError "Found trivial assertion"

  (finalCoeff, finalOp, finalConstant)

sortVarCoeffs :: Map NetworkVariable Coefficient -> [(NetworkVariable, Coefficient)]
sortVarCoeffs coeffs = do
  let coeffsList = Map.toList coeffs
  let getKey (var, _) = (inputOrOutput var, networkVarIndices var)
  sortOn getKey coeffsList

mapAssertionVariables :: (Variable variable2) => (variable1 -> variable2) -> Assertion variable1 -> Assertion variable2
mapAssertionVariables f Assertion {..} =
  Assertion
    { assertionExpr = mapExprVariables f assertionExpr,
      ..
    }

--------------------------------------------------------------------------------
-- Unreduced assertions

-- | A not fully reduced assertion, but none the less only represents
-- conjunctions.
data UnreducedAssertion
  = VectorEqualityAssertion VectorEquality
  | NonVectorEqualityAssertion StandardNormExpr

-- | An encoding of a vector equality.
data VectorEquality = VectorEquality
  { assertionLHS :: StandardNormExpr,
    assertionRHS :: StandardNormExpr,
    assertionDims :: TensorDimensions,
    assertionOriginalRel :: StandardNormExpr -> StandardNormExpr -> StandardNormExpr
  }

originalVectorEqualityExpr :: VectorEquality -> StandardNormExpr
originalVectorEqualityExpr VectorEquality {..} = assertionOriginalRel assertionLHS assertionRHS

assertionToVectorEquality :: UnreducedAssertion -> Maybe VectorEquality
assertionToVectorEquality = \case
  VectorEqualityAssertion eq -> Just eq
  NonVectorEqualityAssertion {} -> Nothing

isVectorEqualityAssertion :: UnreducedAssertion -> Bool
isVectorEqualityAssertion = \case
  VectorEqualityAssertion {} -> True
  NonVectorEqualityAssertion {} -> False

--------------------------------------------------------------------------------
-- Linear satisfaction problem

-- | Conjunctive linear satisfaction problem, parameterised by the type of
-- variables it is over.
data CLSTProblem = CLSTProblem (BoundCtx NetworkVariable) [Assertion NetworkVariable]

instance Pretty CLSTProblem where
  pretty (CLSTProblem _varNames assertions) = pretty assertions
