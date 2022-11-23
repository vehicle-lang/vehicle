module Vehicle.Compile.Queries.Linearity.Core
  ( Relation(..)
  , Coefficient
  , LinearVar
  , LinearExpr(..)
  , Assertion(..)
  , CLSTProblem(..)
  , VariableNames
  , VarReconstruction(..)
  , UserVarReconstructionInfo
  , linearExprFromMap
  , evaluateExpr
  , addLinearExprs
  , constructAssertion
  , splitOutConstant
  , mapExpression
  , isEquality
  , prettyLinearExpr
  , prettyAssertions
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Relation

data Relation
  = Equal
  | LessThan
  | LessThanOrEqualTo
  deriving (Eq)

instance Pretty Relation where
  pretty = \case
    Equal             -> "="
    LessThan          -> "<"
    LessThanOrEqualTo -> "<="

--------------------------------------------------------------------------------
-- Linear expressions

type Coefficient = Double
type VariableNames = [Text]
type LinearVar = Int

-- | Stores a linear expression `ax + by + ... + cz + d`
-- as <a,b,...,c,d>
newtype LinearExpr = LinearExpr
  { unLinearExpr :: Vector Coefficient
  }

evaluateExpr :: LinearExpr -> Vector Double -> Double
evaluateExpr (LinearExpr e) values = Vector.sum (Vector.zipWith (*) e values)

splitOutConstant :: LinearExpr -> (Vector Coefficient, Coefficient)
splitOutConstant (LinearExpr e) = case Vector.unsnoc e of
  Nothing -> developerError "Invalid empty linear expression"
  Just x  -> x

instance Negatable LinearExpr where
  neg (LinearExpr e) = LinearExpr (Vector.map ((-1) *) e)

addLinearExprs :: LinearExpr -> LinearExpr -> LinearExpr
addLinearExprs (LinearExpr e1) (LinearExpr e2) = LinearExpr $ Vector.zipWith (+) e1 e2

linearExprFromMap :: LinearVar -> Map LinearVar Coefficient -> LinearExpr
linearExprFromMap size coeffMap = LinearExpr $
  Vector.generate size $ \i -> fromMaybe 0 (Map.lookup i coeffMap)

prettyLinearExpr :: LinearExpr -> VariableNames -> Doc a
prettyLinearExpr (LinearExpr coeff) names =
  -- Append an empty name for the constant at the end
  let allNames = fmap Just names <> [Nothing] in
  let coeffVarPairs = zip (Vector.toList coeff) allNames in
  let nonZeroCoeffVarPairs = filter (\(c,_) -> c /= 0) coeffVarPairs in
  case nonZeroCoeffVarPairs of
    []       -> "0.0"
    (x : xs) -> hsep (compileVar True x : fmap (compileVar False) xs)
  where
    compileVar :: Bool -> (Coefficient, Maybe Text) -> Doc a
    compileVar isFirst (coefficient, Nothing)
      | coefficient > 0 = (if isFirst then "" else "+ ")  <> pretty coefficient
      | otherwise       = (if isFirst then "-" else "- ") <> pretty (- coefficient)
    compileVar isFirst (1,  Just var) = (if isFirst then "" else "+ ")  <> pretty var
    compileVar isFirst (-1, Just var) = (if isFirst then "-" else "- ") <> pretty var
    compileVar isFirst (coefficient, Just var)
      | coefficient > 0 = (if isFirst then ""  else "+ ") <> pretty coefficient <> pretty var
      | otherwise       = (if isFirst then "-" else "- ") <> pretty (- coefficient) <> pretty var

--------------------------------------------------------------------------------
-- Assertion

data Assertion = Assertion
  { assertionRel  :: Relation
  -- ^ How the sum of the terms in the linear expression are related.
  , assertionExpr :: LinearExpr
  }

isEquality :: Assertion -> Bool
isEquality a = assertionRel a == Equal

constructAssertion :: (LinearExpr, Relation, LinearExpr) -> Assertion
constructAssertion (lhs, rel, rhs) = Assertion
  { assertionExpr = addLinearExprs lhs (neg rhs)
  , assertionRel  = rel
  }

mapExpression :: (Vector Coefficient -> Vector Coefficient) -> Assertion -> Assertion
mapExpression f (Assertion rel (LinearExpr e)) = Assertion rel (LinearExpr $ f e)

prettyAssertion :: VariableNames -> Assertion -> Doc a
prettyAssertion varNames (Assertion rel linearExpr) =
  prettyLinearExpr linearExpr varNames <+> pretty rel <+> "0.0"

prettyAssertions :: VariableNames -> [Assertion] -> Doc a
prettyAssertions varNames assertions =
  indent 2 $ vsep (fmap (prettyAssertion varNames) assertions)

--------------------------------------------------------------------------------
-- Linear satisfaction problem

-- | Conjunctive linear satisfaction problem
data CLSTProblem = CLSTProblem VariableNames [Assertion]

instance Pretty CLSTProblem where
  pretty (CLSTProblem varNames assertions) = prettyAssertions varNames assertions

--------------------------------------------------------------------------------
-- Variable reconstruction

-- | Information neccesary to reconstruct the user variables from the magic
-- input/output variables.
data VarReconstruction
  = RecEquality LinearExpr
  | RecInequalities [Assertion] [Assertion]

type UserVarReconstructionInfo = [(LinearVar, VarReconstruction)]
