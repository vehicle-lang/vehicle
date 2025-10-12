module Vehicle.Data.Code.LinearExpr where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import Vehicle.Data.Tensor (HasShape, RatTensor, allTensor, mapTensor, zipWithTensor, pattern ZeroDimTensor)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude
import Vehicle.Syntax.Tensor (HasShape (..))

-------------------------------------------------------------------------------
-- Constants

type ScaleConstant constant = Coefficient -> constant -> constant

type AddConstants constant = Coefficient -> Coefficient -> constant -> constant -> constant

class ConstantLike constant where
  addConstants :: AddConstants constant
  scaleConstant :: ScaleConstant constant

  -- The zero value must be an annihilator for scaling by a coefficient, and the
  -- identity when added.
  isZero :: constant -> Bool

instance ConstantLike RatTensor where
  addConstants :: Coefficient -> Coefficient -> RatTensor -> RatTensor -> RatTensor
  addConstants a b = zipWithTensor (\x y -> a * x + b * y)

  scaleConstant :: Coefficient -> RatTensor -> RatTensor
  scaleConstant a = mapTensor (\x -> a * x)

  isZero :: RatTensor -> Bool
  isZero = allTensor (== 0)

extractRationalConstant :: RatTensor -> Rational
extractRationalConstant = \case
  ZeroDimTensor v -> v
  t -> developerError $ "Cannot extract constant from multi-dim tensor" <+> pretty t

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

instance (Hashable variable, Hashable constant) => Hashable (LinearExpr variable constant)

instance (HasShape constant) => HasShape (LinearExpr variable constant) where
  shapeOf = shapeOf . constantValue

mapVariables ::
  (Ord variable2) =>
  (variable1 -> variable2) ->
  LinearExpr variable1 constant ->
  LinearExpr variable2 constant
mapVariables f Sparse {..} =
  Sparse
    { coefficients = Map.mapKeys f coefficients,
      ..
    }

constantExpr :: (Ord variable) => constant -> LinearExpr variable constant
constantExpr = Sparse mempty

-- This is a bit annoying as we can't reconstruct `zero` purely from the type alone,
-- see comment on `IsConstant` type-class so we have to pass it explicitly.
singletonVarExpr :: constant -> variable -> LinearExpr variable constant
singletonVarExpr zero var = Sparse (Map.singleton var 1) zero

linearExprLikeToExpr ::
  (Bool -> constant -> expr) ->
  (Bool -> (variable, Coefficient) -> expr) ->
  (expr -> expr -> expr) ->
  [(variable, Coefficient)] ->
  constant ->
  expr
linearExprLikeToExpr constantToExpr variableToExpr combineExprs coefficients constant = do
  case coefficients of
    [] -> constantToExpr True constant
    (x : xs) -> do
      let varDocs = variableToExpr True x : fmap (variableToExpr False) xs
      let constDoc = constantToExpr False constant
      foldr1 combineExprs (varDocs <> [constDoc])

addExprsBase ::
  (VariableLike variable) =>
  AddConstants constant ->
  Coefficient ->
  Coefficient ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  LinearExpr variable constant
addExprsBase add c1 c2 (Sparse coeff1 const1) (Sparse coeff2 const2) = do
  -- We should really be able to do this in one operation, but the API isn't flexible enough.
  let coeff1' = if c1 == 1 then coeff1 else Map.map (c1 *) coeff1
  let coeff2' = if c2 == 1 then coeff2 else Map.map (c2 *) coeff2
  let rcoeff = Map.filter (/= 0) (Map.unionWith (+) coeff1' coeff2')
  let rconst = add c1 c2 const1 const2
  Sparse rcoeff rconst

addExprs ::
  (VariableLike variable, ConstantLike constant) =>
  Coefficient ->
  Coefficient ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  LinearExpr variable constant
addExprs = addExprsBase addConstants

scaleExprBase ::
  ScaleConstant constant ->
  Coefficient ->
  LinearExpr variable constant ->
  LinearExpr variable constant
scaleExprBase scale c (Sparse coefficients constant) =
  Sparse (Map.map (c *) coefficients) (scale c constant)

scaleExpr :: (ConstantLike constant) => Coefficient -> LinearExpr variable constant -> LinearExpr variable constant
scaleExpr = scaleExprBase scaleConstant

lookupCoefficient :: (VariableLike variable) => LinearExpr variable constant -> variable -> Coefficient
lookupCoefficient (Sparse coefficients _) v = fromMaybe 0 $ Map.lookup v coefficients

isConstant :: LinearExpr variable constant -> Maybe constant
isConstant (Sparse coeff constant)
  | Map.null coeff = Just constant
  | otherwise = Nothing

evaluateExpr :: forall constant variable. (VariableLike variable, ConstantLike constant) => LinearExpr variable constant -> Map variable constant -> Either variable constant
evaluateExpr expr assignment = do
  let Sparse coefficients constant = expr
  foldM op constant (Map.toList coefficients)
  where
    op :: constant -> (variable, Coefficient) -> Either variable constant
    op total (var, coeff) = case Map.lookup var assignment of
      Nothing -> Left var
      Just value -> Right $ addConstants 1 coeff total value

-- | Takes an assertion `c_0*x_0 + ... + c_i*x_i + ... c_n * x_n` and
-- returns (c_i, -(c_0/c_i)*x_0 ... - (c_n/c_i) * x_n), i.e.
-- the expression is the expression equal to `x_i`.
rearrangeExprToSolveForBase ::
  (VariableLike variable) =>
  ScaleConstant constant ->
  variable ->
  LinearExpr variable constant ->
  (Coefficient, LinearExpr variable constant)
rearrangeExprToSolveForBase scale var expr = do
  let c = lookupCoefficient expr var
  if c == 0
    then (0, expr)
    else do
      let scaledExpr = scaleExprBase scale (-(1 / c)) expr
      ( c,
        scaledExpr
          { coefficients = Map.delete var $ coefficients scaledExpr
          }
        )

rearrangeExprToSolveFor ::
  (VariableLike variable, ConstantLike constant) =>
  variable ->
  LinearExpr variable constant ->
  (Coefficient, LinearExpr variable constant)
rearrangeExprToSolveFor = rearrangeExprToSolveForBase scaleConstant

eliminateVars ::
  forall variable constant.
  (VariableLike variable, ConstantLike constant) =>
  Map variable (LinearExpr variable constant) ->
  LinearExpr variable constant ->
  Either constant (LinearExpr variable constant)
eliminateVars solutions expr@(Sparse coeffs _) = do
  let relevantVars = Map.intersectionWith (,) solutions coeffs
  let newExpr = foldr elim expr (Map.toList relevantVars)
  case isConstant newExpr of
    Just c -> Left c
    Nothing -> Right newExpr
  where
    elim :: (variable, (LinearExpr variable constant, Coefficient)) -> LinearExpr variable constant -> LinearExpr variable constant
    elim (var, (sol, coef)) row
      | coef == 0 = row
      | otherwise = do
          let resultExpr = addExprs 1 coef row sol
          resultExpr
            { coefficients = Map.delete var $ coefficients resultExpr
            }

linearExprVariables :: (VariableLike variable) => LinearExpr variable constant -> Set variable
linearExprVariables linearExpr = Map.keysSet $ coefficients linearExpr

prettyLinearExpr ::
  forall variable constant a.
  (variable -> Doc a) ->
  (constant -> Doc a) ->
  LinearExpr variable constant ->
  Doc a
prettyLinearExpr prettyVar prettyConst (Sparse coefficients constant) =
  prettyLinearExprLike prettyVar prettyConst (Map.toList coefficients) constant

prettyLinearExprLike ::
  forall variable constant a.
  (variable -> Doc a) ->
  (constant -> Doc a) ->
  [(variable, Coefficient)] ->
  constant ->
  Doc a
prettyLinearExprLike prettyVar prettyConst =
  linearExprLikeToExpr prettyConstant prettyVarCoeff (<>)
  where
    prettyConstant :: Bool -> constant -> Doc a
    prettyConstant isFirst value
      | isFirst = prettyConst value
      | otherwise = " + " <> prettyConst value

    prettyVarCoeff :: Bool -> (variable, Coefficient) -> Doc a
    prettyVarCoeff isFirst (variable, coefficient) = do
      let sign
            | coefficient > 0 = if isFirst then "" else " + "
            | otherwise = if isFirst then "-" else " - "

      let value
            | coefficient == 1 = prettyVar variable
            | coefficient == -1 = prettyVar variable
            | coefficient > 0 = pretty coefficient <> prettyVar variable
            | otherwise = pretty (-coefficient) <> prettyVar variable

      sign <> value

-------------------------------------------------------------------------------
-- Has variables

class HasVariables expr var | expr -> var where
  variablesOf :: expr -> Set var
  containsVariable :: expr -> var -> Bool

instance (Ord var) => HasVariables (LinearExpr var constant) var where
  variablesOf (Sparse coefficients _) = Map.keysSet coefficients
  containsVariable (Sparse coefficients _) v = v `Map.member` coefficients

-------------------------------------------------------------------------------
-- Specialisations

type LinearExpression = LinearExpr SliceVariable RatTensor
