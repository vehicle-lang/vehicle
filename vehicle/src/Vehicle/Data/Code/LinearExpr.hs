module Vehicle.Data.Code.LinearExpr where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Foldable (foldrM)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import Vehicle.Data.Tensor (HasShape (..), RatTensor, allTensor)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- Constants

-- At the moment we only support rational coefficients.
type Coefficient = Rational

type ScaleConstant constant m = Coefficient -> constant -> m constant

type AddConstants constant m = Coefficient -> Coefficient -> constant -> constant -> m constant

class (Monad m) => ConstantLike constant m where
  addConstants :: AddConstants constant m
  scaleConstant :: ScaleConstant constant m
  minConstants :: constant -> constant -> m constant
  maxConstants :: constant -> constant -> m constant
  stackConstants :: [constant] -> m constant
  unstackConstants :: constant -> m [constant]

  toRatTensor :: constant -> m (Maybe RatTensor)

-- The zero value must be an annihilator for scaling by a coefficient,
-- and the identity when added.
isZero :: (ConstantLike constant m) => constant -> m Bool
isZero constant = maybe False (allTensor (== 0)) <$> toRatTensor constant

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

mapExpr ::
  (Ord variable2) =>
  (variable1 -> variable2) ->
  (constant1 -> constant2) ->
  LinearExpr variable1 constant1 ->
  LinearExpr variable2 constant2
mapExpr f g Sparse {..} =
  Sparse
    { coefficients = Map.mapKeys f coefficients,
      constantValue = g constantValue
    }

constantExpr :: (Ord variable) => constant -> LinearExpr variable constant
constantExpr = Sparse mempty

checkExprTriviality :: LinearExpr variable constant -> Either constant (LinearExpr variable constant)
checkExprTriviality expr = case isConstant expr of
  Just c -> Left c
  Nothing -> Right expr

-- This is a bit annoying as we can't reconstruct `zero` purely from the type alone,
-- see comment on `IsConstant` type-class so we have to pass it explicitly.
singletonVarExpr :: constant -> variable -> LinearExpr variable constant
singletonVarExpr zero var = Sparse (Map.singleton var 1) zero

linearExprToExpr ::
  (Monad m) =>
  (constant -> expr) ->
  ((variable, Coefficient) -> m expr) ->
  (expr -> expr -> m expr) ->
  LinearExpr variable constant ->
  m expr
linearExprToExpr mkConst mkTerm add (Sparse coeff constant) =
  linearExprLikeToExpr (const mkConst) (const mkTerm) add (Map.toList coeff) constant

linearExprLikeToExpr ::
  (Monad m) =>
  (Bool -> constant -> expr) ->
  (Bool -> (variable, Coefficient) -> m expr) ->
  (expr -> expr -> m expr) ->
  [(variable, Coefficient)] ->
  constant ->
  m expr
linearExprLikeToExpr constantToExpr variableToExpr combineExprs coefficients constant = do
  case coefficients of
    [] -> return $ constantToExpr True constant
    (x : xs) -> do
      xDoc <- variableToExpr True x
      xsDocs <- traverse (variableToExpr False) xs
      let constDoc = constantToExpr False constant
      foldrM1 combineExprs ((xDoc :| xsDocs) <> [constDoc])

-- | This function does not check that the returned linear expression
-- is a constant. This is often problematic, and unless you are sure you
-- don't need to check for this case, it is recommended you use `addExprs`.
addExprsUnsafe ::
  (VariableLike variable, ConstantLike constant m) =>
  Coefficient ->
  Coefficient ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  m (LinearExpr variable constant)
addExprsUnsafe c1 c2 (Sparse coeff1 const1) (Sparse coeff2 const2) = do
  -- We should really be able to do this in one operation, but the API isn't flexible enough.
  let coeff1' = if c1 == 1 then coeff1 else Map.map (c1 *) coeff1
  let coeff2' = if c2 == 1 then coeff2 else Map.map (c2 *) coeff2
  let rcoeff = Map.filter (/= 0) (Map.unionWith (+) coeff1' coeff2')
  rconst <- addConstants c1 c2 const1 const2
  return $ Sparse rcoeff rconst

addExprs ::
  (VariableLike variable, ConstantLike constant m) =>
  Coefficient ->
  Coefficient ->
  LinearExpr variable constant ->
  LinearExpr variable constant ->
  m (Either constant (LinearExpr variable constant))
addExprs c1 e1 c2 e2 = checkExprTriviality <$> addExprsUnsafe c1 e1 c2 e2

scaleExpr ::
  (ConstantLike constant m) =>
  Coefficient ->
  LinearExpr variable constant ->
  m (LinearExpr variable constant)
scaleExpr c (Sparse coefficients constant) =
  Sparse (Map.map (c *) coefficients) <$> scaleConstant c constant

lookupCoefficient :: (VariableLike variable) => LinearExpr variable constant -> variable -> Coefficient
lookupCoefficient (Sparse coefficients _) v = fromMaybe 0 $ Map.lookup v coefficients

isConstant :: LinearExpr variable constant -> Maybe constant
isConstant (Sparse coeff constant)
  | Map.null coeff = Just constant
  | otherwise = Nothing

evaluateExpr ::
  forall constant m variable.
  (VariableLike variable, ConstantLike constant m) =>
  Map variable constant ->
  LinearExpr variable constant ->
  m (Either variable constant)
evaluateExpr assignment (Sparse coefficients constant) = do
  eval (Map.toList coefficients)
  where
    eval :: [(variable, Coefficient)] -> m (Either variable constant)
    eval = \case
      [] -> return $ Right constant
      (var, coeff) : cs -> do
        errorOrRecResult <- eval cs
        case errorOrRecResult of
          Left err -> return $ Left err
          Right recResult -> case Map.lookup var assignment of
            Nothing -> return $ Left var
            Just value -> Right <$> addConstants 1 coeff recResult value

-- | Takes an assertion `c_0*x_0 + ... + c_i*x_i + ... c_n * x_n` and
-- returns (c_i, -(c_0/c_i)*x_0 ... - (c_n/c_i) * x_n), i.e.
-- the expression is the expression equal to `x_i`.
rearrangeExprToSolveFor ::
  (VariableLike variable, ConstantLike constant m) =>
  variable ->
  LinearExpr variable constant ->
  m (Coefficient, LinearExpr variable constant)
rearrangeExprToSolveFor var expr = do
  let c = lookupCoefficient expr var
  if c == 0
    then return (0, expr)
    else do
      scaledExpr <- scaleExpr (-(1 / c)) expr
      let prunedExpr =
            scaledExpr
              { coefficients = Map.delete var $ coefficients scaledExpr
              }
      return (c, prunedExpr)

eliminateVars ::
  forall variable m constant.
  (VariableLike variable, ConstantLike constant m) =>
  Map variable (LinearExpr variable constant) ->
  LinearExpr variable constant ->
  m (Either constant (LinearExpr variable constant))
eliminateVars solutions expr@(Sparse coeffs _) = do
  let relevantVars = Map.intersectionWith (,) solutions coeffs
  newExpr <- foldrM elim expr (Map.toList relevantVars)
  return $ checkExprTriviality newExpr
  where
    elim ::
      (variable, (LinearExpr variable constant, Coefficient)) ->
      LinearExpr variable constant ->
      m (LinearExpr variable constant)
    elim (var, (sol, coef)) row
      | coef == 0 = return row
      | otherwise = do
          resultExpr <- addExprsUnsafe 1 coef row sol
          return $
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
prettyLinearExprLike prettyVar prettyConst vars cons =
  runIdentity $ linearExprLikeToExpr prettyConstant prettyVarCoeff (\x y -> return $ x <> y) vars cons
  where
    prettyConstant :: Bool -> constant -> Doc a
    prettyConstant isFirst value
      | isFirst = prettyConst value
      | otherwise = " + " <> prettyConst value

    prettyVarCoeff :: (Monad m) => Bool -> (variable, Coefficient) -> m (Doc a)
    prettyVarCoeff isFirst (variable, coefficient) = do
      let sign
            | coefficient > 0 = if isFirst then "" else " + "
            | otherwise = if isFirst then "-" else " - "

      let value
            | coefficient == 1 = prettyVar variable
            | coefficient == -1 = prettyVar variable
            | coefficient > 0 = pretty coefficient <> prettyVar variable
            | otherwise = pretty (-coefficient) <> prettyVar variable

      return $ sign <> value

-------------------------------------------------------------------------------
-- Has variables

class HasVariables expr var | expr -> var where
  variablesOf :: expr -> Set var
  containsVariable :: expr -> var -> Bool

instance (Ord var) => HasVariables (LinearExpr var constant) var where
  variablesOf (Sparse coefficients _) = Map.keysSet coefficients
  containsVariable (Sparse coefficients _) v = v `Map.member` coefficients
