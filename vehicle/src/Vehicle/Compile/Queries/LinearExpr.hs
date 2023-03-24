module Vehicle.Compile.Queries.LinearExpr
  ( Relation (..),
    Coefficient,
    LinearVar,
    LinearExpression (..),
    Assertion (..),
    CLSTProblem (..),
    VariableAssignment,
    DenseLinearExpr (Dense),
    SparseLinearExpr (Sparse),
    SolvingLinearExpr,
    evaluateExpr,
    constructAssertion,
    splitOutConstant,
    isEquality,
    prettyLinearExpr,
    prettyAssertions,
    hasUserVariables,
    removeUserVariables,
    substitute,
    filterTrivialAssertions,
    eliminateVar,
    prettyCoefficient,
    convertToSparseFormat,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import Numeric qualified
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.Variable

--------------------------------------------------------------------------------
-- Relation

data Relation
  = Equal
  | LessThan
  | LessThanOrEqualTo
  deriving (Show, Eq, Generic)

instance ToJSON Relation

instance FromJSON Relation

instance Pretty Relation where
  pretty = \case
    Equal -> "="
    LessThan -> "<"
    LessThanOrEqualTo -> "<="

relationToOp :: Relation -> Either () OrderOp
relationToOp = \case
  Equal -> Left ()
  LessThan -> Right Lt
  LessThanOrEqualTo -> Right Le

--------------------------------------------------------------------------------
-- Variables

type Coefficient = Double

type LinearVar = Int

prettyVar :: IsVariable variable => Bool -> (Coefficient, Maybe variable) -> Doc a
prettyVar isFirst (coefficient, variable) = do
  let sign
        | coefficient > 0 = if isFirst then "" else "+ "
        | otherwise = if isFirst then "-" else "- "

  let value = case variable of
        Nothing
          | coefficient > 0 -> prettyCoefficient coefficient
          | otherwise -> prettyCoefficient (-coefficient)
        Just var
          | coefficient == 1 -> pretty var
          | coefficient == -1 -> pretty var
          | coefficient > 0 -> prettyCoefficient coefficient <> pretty var
          | otherwise -> prettyCoefficient (-coefficient) <> pretty var

  sign <> value

prettyCoefficient :: Coefficient -> Doc a
prettyCoefficient v = pretty $ Numeric.showFFloat Nothing v ""

--------------------------------------------------------------------------------
-- Linear expressions

class LinearExpression linexp where
  addExprs :: Coefficient -> linexp -> Coefficient -> linexp -> linexp
  scale :: Coefficient -> linexp -> linexp
  lookupAt :: linexp -> LinearVar -> Coefficient
  toDense :: linexp -> DenseLinearExpr
  toSparse :: linexp -> SparseLinearExpr
  fromSparse :: SparseLinearExpr -> linexp
  fromDense :: DenseLinearExpr -> linexp
  isConstant :: linexp -> Maybe Coefficient
  hasVariablesBelow :: LinearVar -> linexp -> Bool
  dropVariablesBelow :: LinearVar -> linexp -> linexp

evaluateExpr :: LinearExpression linExpr => linExpr -> Vector Double -> Double
evaluateExpr expr values = do
  let (coeff, constant) = splitOutConstant expr
  Vector.sum (Vector.zipWith (*) coeff values) + constant

splitOutConstant :: LinearExpression linExp => linExp -> (Vector Coefficient, Coefficient)
splitOutConstant expr = do
  let Dense coeff = toDense expr
  case Vector.unsnoc coeff of
    Nothing -> developerError "Invalid empty linear expression"
    Just x -> x

prettyLinearExpr :: (LinearExpression linexp, IsVariable variable) => [variable] -> linexp -> Doc a
prettyLinearExpr variables expr = do
  let Dense coefficents = toDense expr
  -- Append an empty variable name for the constant at the end
  let allNames = fmap Just variables <> [Nothing]
  let coeffVarPairs = Vector.toList coefficents
  case zip coeffVarPairs allNames of
    [] -> "0.0"
    (x : xs) -> hsep (prettyVar True x : fmap (prettyVar False) xs)

eliminateVar :: LinearExpression linexpr => LinearVar -> linexpr -> linexpr -> linexpr
eliminateVar var solution row = do
  let varCoefficient = lookupAt row var
  if varCoefficient == 0
    then row
    else do
      let scaleFactor = varCoefficient / lookupAt solution var
      addExprs 1 row (-scaleFactor) solution

--------------------------------------------------------------------------------
-- Sparse representations of linear vectors

data SparseLinearExpr = Sparse
  { numberOfVars :: Int,
    coefficients :: HashMap LinearVar Coefficient,
    constantValue :: Coefficient
  }
  deriving (Show, Generic)

instance ToJSON SparseLinearExpr

instance FromJSON SparseLinearExpr

instance LinearExpression SparseLinearExpr where
  addExprs ::
    Coefficient ->
    SparseLinearExpr ->
    Coefficient ->
    SparseLinearExpr ->
    SparseLinearExpr
  addExprs c1 (Sparse size coeff1 const1) c2 (Sparse _ coeff2 const2) = do
    -- We should really be able to do this in one operation, but the API isn't flexible enough.
    let coeff1' = if c1 == 1 then coeff1 else HashMap.map (c1 *) coeff1
    let coeff2' = if c2 == 1 then coeff2 else HashMap.map (c2 *) coeff2
    let rcoeff = HashMap.filter (/= 0) (HashMap.unionWith (+) coeff1' coeff2')
    let rconst = c1 * const1 + c2 * const2
    Sparse size rcoeff rconst

  scale :: Coefficient -> SparseLinearExpr -> SparseLinearExpr
  scale c (Sparse size coefficients constant) =
    Sparse size (HashMap.map (c *) coefficients) (c * constant)

  lookupAt :: SparseLinearExpr -> LinearVar -> Coefficient
  lookupAt (Sparse _ coefficients _) v = fromMaybe 0 $ HashMap.lookup v coefficients

  toSparse :: SparseLinearExpr -> SparseLinearExpr
  toSparse = id

  fromSparse :: SparseLinearExpr -> SparseLinearExpr
  fromSparse = id

  toDense :: SparseLinearExpr -> DenseLinearExpr
  toDense = sparseToDense

  fromDense :: DenseLinearExpr -> SparseLinearExpr
  fromDense = denseToSparse

  isConstant :: SparseLinearExpr -> Maybe Coefficient
  isConstant (Sparse _ coeff constant)
    | HashMap.null coeff = Just constant
    | otherwise = Nothing

  hasVariablesBelow :: LinearVar -> SparseLinearExpr -> Bool
  hasVariablesBelow var (Sparse _ coeff _) =
    any (< var) (HashMap.keys coeff)

  dropVariablesBelow :: LinearVar -> SparseLinearExpr -> SparseLinearExpr
  dropVariablesBelow var (Sparse size coeff constant) = do
    let coeffAbove = HashMap.filterWithKey (\k _v -> k >= var) coeff
    let newCoeff = HashMap.mapKeys (\k -> k - var) coeffAbove
    Sparse (size - var) newCoeff constant

--------------------------------------------------------------------------------
-- Dense representations of linear expressions

-- | Stores a linear expression `ax + by + ... + cz + d`
-- as <a,b,...,c,d>
newtype DenseLinearExpr = Dense
  { unDense :: Vector Coefficient
  }
  deriving (Show, Generic)

instance ToJSON DenseLinearExpr

instance FromJSON DenseLinearExpr

instance LinearExpression DenseLinearExpr where
  addExprs ::
    Coefficient ->
    DenseLinearExpr ->
    Coefficient ->
    DenseLinearExpr ->
    DenseLinearExpr
  addExprs c1 (Dense e1) c2 (Dense e2) =
    Dense $ Vector.zipWith (\a b -> c1 * a + c2 * b) e1 e2

  scale :: Coefficient -> DenseLinearExpr -> DenseLinearExpr
  scale c (Dense e) = Dense $ Vector.map (c *) e

  lookupAt :: DenseLinearExpr -> LinearVar -> Coefficient
  lookupAt (Dense e) var = e Vector.! var

  toSparse :: DenseLinearExpr -> SparseLinearExpr
  toSparse = denseToSparse

  fromSparse :: SparseLinearExpr -> DenseLinearExpr
  fromSparse = sparseToDense

  toDense :: DenseLinearExpr -> DenseLinearExpr
  toDense = id

  fromDense :: DenseLinearExpr -> DenseLinearExpr
  fromDense = id

  isConstant :: DenseLinearExpr -> Maybe Coefficient
  isConstant (Dense e) = do
    let numberOfVars = Vector.length e - 1
    if Vector.all (== 0) (Vector.take numberOfVars e)
      then Just (e Vector.! numberOfVars)
      else Nothing

  hasVariablesBelow :: LinearVar -> DenseLinearExpr -> Bool
  hasVariablesBelow v (Dense e) = Vector.any (/= 0) (Vector.take v e)

  dropVariablesBelow :: LinearVar -> DenseLinearExpr -> DenseLinearExpr
  dropVariablesBelow v (Dense e) = Dense (Vector.drop v e)

sparseToDense :: SparseLinearExpr -> DenseLinearExpr
sparseToDense (Sparse size coeffMap constant) = Dense $
  Vector.generate (size + 1) $
    \i ->
      if i == size
        then constant
        else fromMaybe 0 (HashMap.lookup i coeffMap)

denseToSparse :: DenseLinearExpr -> SparseLinearExpr
denseToSparse (Dense e) = do
  let size = Vector.length e - 1
  let coeff = zip [0 ..] (Vector.toList $ Vector.take size e)
  let nonZeroCoeff = HashMap.fromList $ filter (\(_, c) -> c /= 0) coeff
  let constant = Vector.last e
  Sparse size nonZeroCoeff constant

--------------------------------------------------------------------------------
-- Assertion

data Assertion linexp = Assertion
  { -- | How the sum of the terms in the linear expression are related.
    assertionRel :: Relation,
    assertionExpr :: linexp
  }
  deriving (Show, Functor, Generic)

instance ToJSON linexp => ToJSON (Assertion linexp)

instance FromJSON linexp => FromJSON (Assertion linexp)

isEquality :: Assertion linexp -> Bool
isEquality a = assertionRel a == Equal

constructAssertion ::
  LinearExpression linexp =>
  (linexp, Relation, linexp) ->
  Assertion linexp
constructAssertion (lhs, rel, rhs) =
  Assertion
    { assertionExpr = addExprs 1 lhs (-1) rhs,
      assertionRel = rel
    }

prettyAssertion ::
  (LinearExpression linexp, IsVariable variable) =>
  [variable] ->
  Assertion linexp ->
  Doc a
prettyAssertion varNames (Assertion rel linearExpr) =
  prettyLinearExpr varNames linearExpr <+> pretty rel <+> "0.0"

prettyAssertions ::
  (LinearExpression linexp, IsVariable variable) =>
  [variable] ->
  [Assertion linexp] ->
  Doc a
prettyAssertions varNames assertions =
  indent 2 $ vsep (fmap (prettyAssertion varNames) assertions)

substitute :: LinearExpression linexp => Assertion linexp -> LinearVar -> linexp -> Assertion linexp
substitute (Assertion r2 e2) var e1 = Assertion r2 (eliminateVar var e1 e2)

hasUserVariables :: LinearExpression linexp => Int -> Assertion linexp -> Bool
hasUserVariables numberOfUserVariables (Assertion _ e) =
  hasVariablesBelow numberOfUserVariables e

removeUserVariables :: LinearExpression linexp => Int -> Assertion linexp -> Assertion linexp
removeUserVariables numberOfUserVariables (Assertion rel e) = do
  Assertion rel (dropVariablesBelow numberOfUserVariables e)

-- Assertion rel (LinearExpr (Vector.drop numberOfUserVariables e))

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkTriviality :: LinearExpression linexp => Assertion linexp -> Maybe Bool
checkTriviality (Assertion rel linexp) =
  case isConstant linexp of
    Nothing -> Nothing
    Just c -> Just $ case rel of
      Equal -> c == 0.0
      LessThan -> c < 0.0
      LessThanOrEqualTo -> c <= 0.0

filterTrivialAssertions ::
  forall linexp.
  LinearExpression linexp =>
  [Assertion linexp] ->
  Maybe [Assertion linexp]
filterTrivialAssertions = go
  where
    go :: [Assertion linexp] -> Maybe [Assertion linexp]
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
  LinearExpression linexp =>
  Assertion linexp ->
  Seq Name ->
  (NonEmpty (Coefficient, Name), Either () OrderOp, Coefficient)
convertToSparseFormat (Assertion rel linearExpr) variableNames = do
  let Sparse _ coeffs constant = toSparse linearExpr
  let varCoeff = sortOn fst $ HashMap.toList coeffs
  let lookupName (v, c) = (c, variableNames `Seq.index` v)
  let coeffVars = lookupName <$> varCoeff
  let op = relationToOp rel

  -- Move constant to RHS
  let rhsConstant = -constant

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let allCoefficientsNegative = all (\(c, _) -> c < 0) coeffVars
  let (almostFinalCoeff, finalOp, almostFinalConstant) =
        if not allCoefficientsNegative
          then (coeffVars, op, rhsConstant)
          else do
            let negCoeffVars = fmap (\(c, v) -> (-c, v)) coeffVars
            let negOp = second flipOrder op
            let negConstant = -rhsConstant
            (negCoeffVars, negOp, negConstant)

  -- Also check for and remove `-0.0`s for cleanliness.
  let finalConstant =
        if isNegativeZero almostFinalConstant
          then 0.0
          else almostFinalConstant

  let finalCoeff = case almostFinalCoeff of
        (c : cs) -> c :| cs
        [] -> developerError "Found trivial assertion"

  (finalCoeff, finalOp, finalConstant)

--------------------------------------------------------------------------------
-- Linear satisfaction problem

-- | The representation used for solving for user variables.
type SolvingLinearExpr = SparseLinearExpr

-- | Conjunctive linear satisfaction problem, parameterised by the type of
-- variables it is over.
data CLSTProblem var = CLSTProblem [var] [Assertion SolvingLinearExpr]

instance IsVariable variable => Pretty (CLSTProblem variable) where
  pretty (CLSTProblem varNames assertions) = prettyAssertions varNames assertions

type VariableAssignment = Vector Double
