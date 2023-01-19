module Vehicle.Compile.Queries.LinearExpr
  ( Relation (..),
    Coefficient,
    LinearVar,
    LinearExpr (..),
    Assertion (..),
    CLSTProblem (..),
    VariableAssignment,
    linearExprFromMap,
    evaluateExpr,
    addLinearExprs,
    constructAssertion,
    splitOutConstant,
    mapExpression,
    isEquality,
    prettyLinearExpr,
    prettyAssertions,
    hasUserVariables,
    removeUserVariables,
    substitute,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
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

--------------------------------------------------------------------------------
-- Linear expressions

type Coefficient = Double

type LinearVar = Int

-- | Stores a linear expression `ax + by + ... + cz + d`
-- as <a,b,...,c,d>
newtype LinearExpr = LinearExpr
  { unLinearExpr :: Vector Coefficient
  }
  deriving (Show, Generic)

instance ToJSON LinearExpr

instance FromJSON LinearExpr

evaluateExpr :: LinearExpr -> Vector Double -> Double
evaluateExpr (LinearExpr e) values = Vector.sum (Vector.zipWith (*) e values)

splitOutConstant :: LinearExpr -> (Vector Coefficient, Coefficient)
splitOutConstant (LinearExpr e) = case Vector.unsnoc e of
  Nothing -> developerError "Invalid empty linear expression"
  Just x -> x

instance Negatable LinearExpr where
  neg (LinearExpr e) = LinearExpr (Vector.map ((-1) *) e)

addLinearExprs :: LinearExpr -> LinearExpr -> LinearExpr
addLinearExprs (LinearExpr e1) (LinearExpr e2) = LinearExpr $ Vector.zipWith (+) e1 e2

linearExprFromMap :: LinearVar -> Map LinearVar Coefficient -> LinearExpr
linearExprFromMap size coeffMap = LinearExpr $
  Vector.generate size $
    \i -> fromMaybe 0 (Map.lookup i coeffMap)

prettyLinearExpr :: IsVariable variable => [variable] -> LinearExpr -> Doc a
prettyLinearExpr variables (LinearExpr coeff) =
  -- Append an empty variable name for the constant at the end
  let allNames = fmap Just variables <> [Nothing]
   in let coeffVarPairs = zip (Vector.toList coeff) allNames
       in let nonZeroCoeffVarPairs = filter (\(c, _) -> c /= 0) coeffVarPairs
           in case nonZeroCoeffVarPairs of
                [] -> "0.0"
                (x : xs) -> hsep (prettyVar True x : fmap (prettyVar False) xs)

prettyVar :: IsVariable variable => Bool -> (Coefficient, Maybe variable) -> Doc a
prettyVar isFirst (coefficient, variable) = do
  let sign
        | coefficient > 0 = if isFirst then "" else "+ "
        | otherwise = if isFirst then "-" else "- "

  let value = case variable of
        Nothing
          | coefficient > 0 -> pretty coefficient
          | otherwise -> pretty (-coefficient)
        Just var
          | coefficient == 1 -> pretty var
          | coefficient == -1 -> pretty var
          | coefficient > 0 -> pretty coefficient <> pretty var
          | otherwise -> pretty (-coefficient) <> pretty var

  sign <> value

--------------------------------------------------------------------------------
-- Assertion

data Assertion = Assertion
  { -- | How the sum of the terms in the linear expression are related.
    assertionRel :: Relation,
    assertionExpr :: LinearExpr
  }
  deriving (Show, Generic)

instance ToJSON Assertion

instance FromJSON Assertion

isEquality :: Assertion -> Bool
isEquality a = assertionRel a == Equal

constructAssertion :: (LinearExpr, Relation, LinearExpr) -> Assertion
constructAssertion (lhs, rel, rhs) =
  Assertion
    { assertionExpr = addLinearExprs lhs (neg rhs),
      assertionRel = rel
    }

mapExpression :: (Vector Coefficient -> Vector Coefficient) -> Assertion -> Assertion
mapExpression f (Assertion rel (LinearExpr e)) = Assertion rel (LinearExpr $ f e)

prettyAssertion :: IsVariable variable => [variable] -> Assertion -> Doc a
prettyAssertion varNames (Assertion rel linearExpr) =
  prettyLinearExpr varNames linearExpr <+> pretty rel <+> "0.0"

prettyAssertions :: IsVariable variable => [variable] -> [Assertion] -> Doc a
prettyAssertions varNames assertions =
  indent 2 $ vsep (fmap (prettyAssertion varNames) assertions)

substitute :: Assertion -> LinearVar -> LinearExpr -> Assertion
substitute (Assertion r2 (LinearExpr e2)) var (LinearExpr e1) =
  let coeff = e2 Vector.! var
   in let e2' = Vector.zipWith (\a b -> b - coeff * a) e1 e2
       in Assertion r2 (LinearExpr e2')

hasUserVariables :: Int -> Assertion -> Bool
hasUserVariables numberOfUserVariables (Assertion _ (LinearExpr e)) =
  let userCoefficients = Vector.take numberOfUserVariables e
   in Vector.any (/= 0) userCoefficients

removeUserVariables :: Int -> Assertion -> Assertion
removeUserVariables numberOfUserVariables (Assertion rel (LinearExpr e)) =
  Assertion rel (LinearExpr (Vector.drop numberOfUserVariables e))

--------------------------------------------------------------------------------
-- Linear satisfaction problem

-- | Conjunctive linear satisfaction problem, parameterised by the type of
-- variables it is over.
data CLSTProblem variable = CLSTProblem [variable] [Assertion]

instance IsVariable variable => Pretty (CLSTProblem variable) where
  pretty (CLSTProblem varNames assertions) = prettyAssertions varNames assertions

type VariableAssignment = Vector Double
