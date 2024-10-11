module Vehicle.Data.Code.LinearExpr where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- Sparse representations of linear expressions

data LinearExpr variable constant = Sparse
  { coefficients :: Map variable Coefficient,
    constantValue :: constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData variable, NFData constant) => NFData (LinearExpr variable constant)

instance (ToJSONKey variable, ToJSON constant) => ToJSON (LinearExpr variable constant)

instance (Ord variable, FromJSONKey variable, FromJSON constant) => FromJSON (LinearExpr variable constant)

constantExpr :: (Ord variable) => constant -> LinearExpr variable constant
constantExpr = Sparse mempty

-- This is a bit annoying as we can't reconstruct `zero` purely from the type alone,
-- see comment on `IsConstant` type-class so we have to pass it explicitly.
singletonVarExpr :: constant -> variable -> LinearExpr variable constant
singletonVarExpr zero var = Sparse (Map.singleton var 1) zero

linearExprToExpr ::
  (Bool -> constant -> expr) ->
  (Bool -> (variable, Coefficient) -> expr) ->
  (expr -> expr -> expr) ->
  LinearExpr variable constant ->
  expr
linearExprToExpr constantToExpr variableToExpr combineExprs (Sparse coefficients constant) = do
  let coeffVars = Map.toList coefficients
  case coeffVars of
    [] -> constantToExpr True constant
    (x : xs) -> do
      let varDocs = variableToExpr True x : fmap (variableToExpr False) xs
      let constDoc = constantToExpr False constant
      foldr1 combineExprs (varDocs <> [constDoc])

prettyVariable ::
  (Pretty variable) =>
  Bool ->
  (variable, Coefficient) ->
  Doc a
prettyVariable isFirst (variable, coefficient) = do
  let sign
        | coefficient > 0 = if isFirst then "" else "+ "
        | otherwise = if isFirst then "-" else "- "

  let value
        | coefficient == 1 = pretty variable
        | coefficient == -1 = pretty variable
        | coefficient > 0 = pretty coefficient <> pretty variable
        | otherwise = pretty (-coefficient) <> pretty variable

  sign <> value

-- | Pretty prints a constant value given a set of dimensions.
-- Note, an alternative would be to go via the Vehicle AST and pretty print
-- that, but we run into dependency cycle issues.
prettyConstant :: (IsConstant constant, Pretty constant) => Bool -> constant -> Doc a
prettyConstant isFirst value
  | not isFirst && isZero value = ""
  | not isFirst = " + " <> pretty value
  | otherwise = pretty value

instance (Pretty variable, Pretty constant, IsConstant constant) => Pretty (LinearExpr variable constant) where
  pretty = linearExprToExpr prettyConstant prettyVariable (<>)

addExprs ::
  (Ord variable, IsConstant constant) =>
  Coefficient ->
  Coefficient ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  LinearExpr variable constant
addExprs c1 c2 (Sparse coeff1 const1) (Sparse coeff2 const2) = do
  -- We should really be able to do this in one operation, but the API isn't flexible enough.
  let coeff1' = if c1 == 1 then coeff1 else Map.map (c1 *) coeff1
  let coeff2' = if c2 == 1 then coeff2 else Map.map (c2 *) coeff2
  let rcoeff = Map.filter (/= 0) (Map.unionWith (+) coeff1' coeff2')
  let rconst = addConstants c1 c2 const1 const2
  Sparse rcoeff rconst

scaleExpr :: (IsConstant constant) => Coefficient -> LinearExpr variable constant -> LinearExpr variable constant
scaleExpr c (Sparse coefficients constant) =
  Sparse (Map.map (c *) coefficients) (scaleConstant c constant)

lookupCoefficient :: (Ord variable) => LinearExpr variable constant -> variable -> Coefficient
lookupCoefficient (Sparse coefficients _) v = fromMaybe 0 $ Map.lookup v coefficients

referencesVariable :: (Ord variable) => LinearExpr variable constant -> variable -> Bool
referencesVariable (Sparse coefficients _) v = v `Map.member` coefficients

isConstant :: LinearExpr variable constant -> Maybe constant
isConstant (Sparse coeff constant)
  | Map.null coeff = Just constant
  | otherwise = Nothing

evaluateExpr ::
  forall variable constant.
  (Ord variable, IsConstant constant) =>
  LinearExpr variable constant ->
  Map variable constant ->
  Either variable constant
evaluateExpr expr assignment = do
  let Sparse coefficients constant = expr
  foldM op constant (Map.toList coefficients)
  where
    op :: constant -> (variable, Coefficient) -> Either variable constant
    op total (var, coeff) = case Map.lookup var assignment of
      Nothing -> Left var
      Just value -> Right (addConstants 1 coeff total value)

eliminateVar ::
  (Ord variable, IsConstant constant) =>
  variable ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  LinearExpr variable constant
eliminateVar var solution row = do
  let varCoefficient = lookupCoefficient row var
  if varCoefficient == 0
    then row
    else do
      let resultExpr = addExprs 1 varCoefficient row solution
      resultExpr
        { coefficients = Map.delete var $ coefficients resultExpr
        }

-- | Takes an assertion `c_0*x_0 + ... + c_i*x_i + ... c_n * x_n` and
-- returns (c_i, -(c_0/c_i)*x_0 ... - (c_n/c_i) * x_n), i.e.
-- the expression is the expression equal to `x_i`.
rearrangeExprToSolveFor ::
  (Ord variable, IsConstant constant) =>
  variable ->
  LinearExpr variable constant ->
  (Coefficient, LinearExpr variable constant)
rearrangeExprToSolveFor var expr = do
  let c = lookupCoefficient expr var
  if c == 0
    then (0, expr)
    else do
      let scaledExpr = scaleExpr (-(1 / c)) expr
      ( c,
        scaledExpr
          { coefficients = Map.delete var $ coefficients scaledExpr
          }
        )

mapExprVariables ::
  (Ord variable1, Ord variable2) =>
  (variable1 -> variable2) ->
  LinearExpr variable1 constant ->
  LinearExpr variable2 constant
mapExprVariables f Sparse {..} =
  Sparse
    { coefficients = Map.mapKeys f coefficients,
      ..
    }