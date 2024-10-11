module Vehicle.Data.Code.LinearExpr where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Vehicle.Data.DeBruijn (Lv)
import Vehicle.Data.QuantifiedVariable (Variable)
import Vehicle.Data.Tensor (RationalTensor, Tensor (..), allTensor, zipWithTensor)
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- Constants

class Constant constant where
  addConstants :: Coefficient -> Coefficient -> constant -> constant -> constant
  scaleConstant :: Coefficient -> constant -> constant

  -- The zero value must be an annihilator for scaling by a coefficient, and the
  -- identity when added.
  isZero :: constant -> Bool

instance Constant RationalTensor where
  addConstants :: Coefficient -> Coefficient -> RationalTensor -> RationalTensor -> RationalTensor
  addConstants a b = zipWithTensor (\x y -> a * x + b * y)

  scaleConstant :: Coefficient -> RationalTensor -> RationalTensor
  scaleConstant a = fmap (\x -> a * x)

  isZero :: RationalTensor -> Bool
  isZero = allTensor (== 0)

{-
extractTensorConstant :: Maybe RationalTensor -> RationalTensor
extractTensorConstant Nothing = 0
extractTensorConstant (Just v) = v
-}
extractRationalConstant :: RationalTensor -> Rational
extractRationalConstant (Tensor [] [v]) = v
extractRationalConstant _ = developerError "FM-elimination doesn't yet work over tensors"

-------------------------------------------------------------------------------
-- Sparse representations of linear expressions

data LinearExpr constant = Sparse
  { coefficients :: Map Variable Coefficient,
    constantValue :: constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData constant) => NFData (LinearExpr constant)

instance (ToJSON constant) => ToJSON (LinearExpr constant)

instance (FromJSON constant) => FromJSON (LinearExpr constant)

constantExpr :: constant -> LinearExpr constant
constantExpr = Sparse mempty

-- This is a bit annoying as we can't reconstruct `zero` purely from the type alone,
-- see comment on `IsConstant` type-class so we have to pass it explicitly.
singletonVarExpr :: constant -> Variable -> LinearExpr constant
singletonVarExpr zero var = Sparse (Map.singleton var 1) zero

linearExprToExpr ::
  (Bool -> constant -> expr) ->
  (Bool -> (Variable, Coefficient) -> expr) ->
  (expr -> expr -> expr) ->
  LinearExpr constant ->
  expr
linearExprToExpr constantToExpr variableToExpr combineExprs (Sparse coefficients constant) = do
  let coeffVars = Map.toList coefficients
  case coeffVars of
    [] -> constantToExpr True constant
    (x : xs) -> do
      let varDocs = variableToExpr True x : fmap (variableToExpr False) xs
      let constDoc = constantToExpr False constant
      foldr1 combineExprs (varDocs <> [constDoc])

addExprs ::
  (Constant constant) =>
  Coefficient ->
  Coefficient ->
  LinearExpr constant ->
  LinearExpr constant ->
  LinearExpr constant
addExprs c1 c2 (Sparse coeff1 const1) (Sparse coeff2 const2) = do
  -- We should really be able to do this in one operation, but the API isn't flexible enough.
  let coeff1' = if c1 == 1 then coeff1 else Map.map (c1 *) coeff1
  let coeff2' = if c2 == 1 then coeff2 else Map.map (c2 *) coeff2
  let rcoeff = Map.filter (/= 0) (Map.unionWith (+) coeff1' coeff2')
  let rconst = addConstants c1 c2 const1 const2
  Sparse rcoeff rconst

scaleExpr :: (Constant constant) => Coefficient -> LinearExpr constant -> LinearExpr constant
scaleExpr c (Sparse coefficients constant) =
  Sparse (Map.map (c *) coefficients) (scaleConstant c constant)

lookupCoefficient :: LinearExpr constant -> Variable -> Coefficient
lookupCoefficient (Sparse coefficients _) v = fromMaybe 0 $ Map.lookup v coefficients

referencesVariable :: LinearExpr constant -> Variable -> Bool
referencesVariable (Sparse coefficients _) v = v `Map.member` coefficients

isConstant :: LinearExpr constant -> Maybe constant
isConstant (Sparse coeff constant)
  | Map.null coeff = Just constant
  | otherwise = Nothing

evaluateExpr :: forall constant. (Constant constant) => LinearExpr constant -> Map Variable constant -> Either Variable constant
evaluateExpr expr assignment = do
  let Sparse coefficients constant = expr
  foldM op constant (Map.toList coefficients)
  where
    op :: constant -> (Variable, Coefficient) -> Either Variable constant
    op total (var, coeff) = case Map.lookup var assignment of
      Nothing -> Left var
      Just value -> Right $ addConstants 1 coeff total value

eliminateVar :: (Constant constant) => Variable -> LinearExpr constant -> LinearExpr constant -> LinearExpr constant
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
rearrangeExprToSolveFor :: (Constant constant) => Variable -> LinearExpr constant -> (Coefficient, LinearExpr constant)
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

prettyLinearExpr ::
  forall constant a.
  (Constant constant) =>
  (Lv -> Doc a) ->
  (constant -> Doc a) ->
  LinearExpr constant ->
  Doc a
prettyLinearExpr prettyVar prettyConst = linearExprToExpr prettyConstant prettyVarCoeff (<>)
  where
    prettyConstant :: Bool -> constant -> Doc a
    prettyConstant isFirst value
      | isZero value && not isFirst = ""
      | isZero value = prettyConst value
      | otherwise = " + " <> prettyConst value

    prettyVarCoeff :: Bool -> (Lv, Coefficient) -> Doc a
    prettyVarCoeff isFirst (variable, coefficient) = do
      let sign
            | coefficient > 0 = if isFirst then "" else "+ "
            | otherwise = if isFirst then "-" else "- "

      let value
            | coefficient == 1 = prettyVar variable
            | coefficient == -1 = prettyVar variable
            | coefficient > 0 = pretty coefficient <> prettyVar variable
            | otherwise = pretty (-coefficient) <> prettyVar variable

      sign <> value
