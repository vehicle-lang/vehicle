module Vehicle.Data.LinearExpr where

import Control.Monad (foldM)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Coefficients

-- At the moment we only support rational coefficients.
type Coefficient = Rational

--------------------------------------------------------------------------------
-- Constant interface

-- If we're ever to support matrix multiplication we'll need to make this a full
-- field structure.
--
-- However, we run into issues that we can't define a `zero` element as we don't
-- don't have access to the dimensions of tensors at the type level due to the
-- lack of dependent types.
class (Pretty constant) => IsConstant constant where
  isZero :: constant -> Bool
  scaleConstant :: Coefficient -> constant -> constant
  addConstants :: Coefficient -> Coefficient -> constant -> constant -> constant

--------------------------------------------------------------------------------
-- Rational constants

instance IsConstant Rational where
  isZero = (== 0.0)
  scaleConstant = (*)
  addConstants a b x y = a * x + b * y

--------------------------------------------------------------------------------
-- Tensor constants

data RationalTensor = RationalTensor
  { tensorDims :: TensorDimensions,
    tensorValues :: Vector Rational
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RationalTensor

instance FromJSON RationalTensor

zeroTensor :: TensorDimensions -> RationalTensor
zeroTensor dims = RationalTensor dims (Vector.replicate (product dims) 0)

mapTensor ::
  (Rational -> Rational) ->
  RationalTensor ->
  RationalTensor
mapTensor f RationalTensor {..} =
  RationalTensor
    { tensorValues = Vector.map f tensorValues,
      ..
    }

zipWithTensor ::
  (Rational -> Rational -> Rational) ->
  RationalTensor ->
  RationalTensor ->
  RationalTensor
zipWithTensor f t1 t2 =
  RationalTensor
    { tensorDims = tensorDims t1,
      tensorValues = Vector.zipWith f (tensorValues t1) (tensorValues t2)
    }

instance Pretty RationalTensor where
  pretty = foldTensor pretty prettyFlatList

foldTensor :: forall a. (Rational -> a) -> ([a] -> a) -> RationalTensor -> a
foldTensor mkValue mkVec (RationalTensor dims value) = go dims (Vector.toList value)
  where
    go :: TensorDimensions -> [Rational] -> a
    go [] xs = mkValue (head xs)
    go (_d : ds) xs = do
      let inputVarIndicesChunks = chunksOf (product ds) xs
      let elems = fmap (go ds) inputVarIndicesChunks
      mkVec elems

instance IsConstant RationalTensor where
  isZero = Vector.all (== 0) . tensorValues
  scaleConstant v = mapTensor (scaleConstant v)
  addConstants a b = zipWithTensor (addConstants a b)

--------------------------------------------------------------------------------
-- Sparse representations of linear expressions

data LinearExpr variable constant = Sparse
  { coefficients :: Map variable Coefficient,
    constantValue :: constant
  }
  deriving (Show, Eq, Ord, Generic)

instance (ToJSONKey variable, ToJSON constant) => ToJSON (LinearExpr variable constant)

instance (Ord variable, FromJSONKey variable, FromJSON constant) => FromJSON (LinearExpr variable constant)

constantExpr :: (Ord variable) => constant -> LinearExpr variable constant
constantExpr = Sparse mempty

-- This is a bit annoying as we can't reconstruct `zero` purely from the type alone,
-- see comment on `IsConstant` type-class so we have to pass it explicitly.
singletonVarExpr :: constant -> variable -> LinearExpr variable constant
singletonVarExpr zero var = Sparse (Map.singleton var 1) zero

prettyCoefficientVar ::
  (Pretty variable) =>
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
        | coefficient > 0 = pretty coefficient <> pretty variable
        | otherwise = pretty (-coefficient) <> pretty variable

  sign <> value

-- | Pretty prints a constant value given a set of dimensions.
-- Note, an alternative would be to go via the Vehicle AST and pretty print
-- that, but we run into dependency cycle issues.
prettyConstant :: (IsConstant constant) => Bool -> constant -> Doc a
prettyConstant isFirst value
  | not isFirst && isZero value = ""
  | not isFirst = " + " <> pretty value
  | otherwise = pretty value

instance (Pretty variable, IsConstant constant) => Pretty (LinearExpr variable constant) where
  pretty (Sparse coefficients constant) = do
    -- Append an empty variable name for the constant at the end
    let coeffVars = Map.toList coefficients
    case coeffVars of
      [] -> prettyConstant True constant
      (x : xs) -> do
        let varDocs = prettyCoefficientVar True x : fmap (prettyCoefficientVar False) xs
        let constDoc = prettyConstant False constant
        hsep varDocs <> constDoc

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
      let scaleFactor = varCoefficient / lookupCoefficient solution var
      let resultExpr = addExprs 1 (-scaleFactor) row solution
      let newCoefficients = Map.delete var $ coefficients resultExpr
      -- Need to handle floating point errors....
      resultExpr
        { coefficients = newCoefficients
        }

-- | Takes an assertion `a*x_0 + ... + b*x_i + ... c * x_n` and
-- returns the RHS of the equation: `x_i = -a/b*x_0 + ... -c/b*x_n`
rearrangeExprToSolveFor ::
  (Ord variable, IsConstant constant) =>
  variable ->
  LinearExpr variable constant ->
  LinearExpr variable constant
rearrangeExprToSolveFor var expr = do
  let c = lookupCoefficient expr var
  let scaledExpr = scaleExpr (-1 / c) expr
  scaledExpr
    { coefficients = Map.delete var $ coefficients scaledExpr
    }

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
