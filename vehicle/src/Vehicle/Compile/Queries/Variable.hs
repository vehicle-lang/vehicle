{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}
module Vehicle.Compile.Queries.Variable
  ( Variable (..),
    UserVariable (..),
    NetworkVariable (..),
    sequentialNetworkVariableNaming,
    MixedVariable (..),
    isRationalVariable,
    isUserVariable,
    getUserVariable,
    getNetworkVariable,
    MixedVariables (..),
    mixedVariableCtx,
    mixedVariableDBCtx,
    Constant,
    scaleConstant,
    prettyConstant,
    addConstants,
    constantExpr,
    VariableValue,
    VariableAssignment,
    lookupAndRemoveAll,
    pattern VInfiniteQuantifier,
    pattern VFiniteQuantifier,
    pattern VFiniteQuantifierSpine,
  )
where

import Control.Monad (foldM)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor (Bifunctor (..))
import Data.Char.SScript
import Data.Hashable (Hashable)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Text qualified as Text (pack)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import Prettyprinter (brackets, list)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DeBruijn (Lv (..))
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (fromFiniteQuantifier, toFiniteQuantifier)

--------------------------------------------------------------------------------
-- Variable class

class
  (Pretty variable, Eq variable, Ord variable, Hashable variable) =>
  Variable variable
  where
  variableDimensions :: variable -> TensorDimensions

  reduceVariable :: Lv -> variable -> ([variable], StandardNormExpr)

  toMixedVariable :: variable -> MixedVariable

isRationalVariable :: (Variable variable) => variable -> Bool
isRationalVariable v = null (variableDimensions v)

--------------------------------------------------------------------------------
-- User variables

-- | Variables entered by the user
data UserVariable = UserVariable
  { userVarName :: Name,
    userVarDimensions :: TensorDimensions,
    unreducedUserVarName :: Name
  }
  deriving (Show, Eq, Ord, Generic)

instance Pretty UserVariable where
  pretty (UserVariable name _ _) = pretty name

instance FromJSON UserVariable

instance FromJSONKey UserVariable

instance ToJSON UserVariable

instance ToJSONKey UserVariable

instance Hashable UserVariable

instance Variable UserVariable where
  variableDimensions = userVarDimensions
  reduceVariable = reduceUserVariable
  toMixedVariable = UserVar

reduceUserVariable :: Lv -> UserVariable -> (BoundCtx UserVariable, StandardNormExpr)
reduceUserVariable dbLevel UserVariable {..} = do
  let (vars, expr) = runSupply (go userVarName userVarDimensions) [dbLevel ..]
  (reverse vars, expr)
  where
    go ::
      Name ->
      TensorDimensions ->
      Supply Lv ([UserVariable], StandardNormExpr)
    go name = \case
      [] -> do
        lv <- demand
        return ([UserVariable name [] userVarName], VBoundVar lv [])
      dim : dims -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. dim - 1]

        -- Generate the corresponding names from the indices
        let mkName i = name <> "_" <> pack (show i)
        (elementUserVars, subexprs) <- unzip <$> traverse (\i -> go (mkName i) dims) allIndices

        let userVars = concat elementUserVars
        return (userVars, mkVLVec subexprs)

--------------------------------------------------------------------------------
-- Network variables

-- | Network input and output variables
data NetworkVariable = NetworkVariable
  { -- | The name of the network this variable belongs to.
    networkName :: Name,
    -- | Records which number application of the network it is.
    application :: Int,
    -- | The dimensions of the variable
    networkVarDimensions :: TensorDimensions,
    -- | Whether its an input or an output variable
    inputOrOutput :: InputOrOutput,
    -- | The position of the input or output variable in the network application.
    networkVarIndices :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON NetworkVariable

instance FromJSON NetworkVariable

instance Hashable NetworkVariable

instance Pretty NetworkVariable where
  pretty NetworkVariable {..} =
    pretty networkName
      <> pretty (fmap subscript (show application))
      <> brackets (pretty inputOrOutput)
      <> maybe "" (\i -> "!" <> pretty i) networkVarIndices

instance Variable NetworkVariable where
  variableDimensions = networkVarDimensions

  reduceVariable = reduceNetworkVariable

  toMixedVariable = NetworkVar

reduceNetworkVariable :: Lv -> NetworkVariable -> (BoundCtx NetworkVariable, StandardNormExpr)
reduceNetworkVariable dbLevel NetworkVariable {..} = do
  let (vars, expr) = runSupply (go networkVarDimensions) [0 :: Int ..]
  (reverse vars, expr)
  where
    go ::
      (MonadSupply Int m) =>
      TensorDimensions ->
      m ([NetworkVariable], StandardNormExpr)
    go = \case
      [] -> do
        index <- demand
        let var = NetworkVariable networkName application [] inputOrOutput (Just index)
        let expr = VBoundVar (dbLevel + Lv index) []
        return ([var], expr)
      dim : dims -> do
        -- Generate the corresponding names from the indices
        (elementUserVars, subexprs) <- unzip <$> traverse (const $ go dims) [0 .. dim - 1]

        let userVars = concat elementUserVars
        return (userVars, mkVLVec subexprs)

sequentialNetworkVariableNaming :: Text -> Text -> BoundCtx NetworkVariable -> [Name]
sequentialNetworkVariableNaming inputPrefix outputPrefix variables = do
  let (_, _, result) = foldl forNetwork (0, 0, []) (reverse variables)
  result
  where
    forNetwork :: (Int, Int, [Name]) -> NetworkVariable -> (Int, Int, [Name])
    forNetwork (inputIndex, outputIndex, result) NetworkVariable {..} =
      case inputOrOutput of
        Input -> do
          let name = inputPrefix <> Text.pack (show inputIndex)
          (inputIndex + 1, outputIndex, name : result)
        Output -> do
          let name = outputPrefix <> Text.pack (show outputIndex)
          (inputIndex, outputIndex + 1, name : result)

--------------------------------------------------------------------------------
-- All variables

-- | Both user and network variables
data MixedVariable
  = UserVar UserVariable
  | NetworkVar NetworkVariable
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MixedVariable

instance FromJSON MixedVariable

instance ToJSONKey MixedVariable

instance FromJSONKey MixedVariable

instance Hashable MixedVariable

instance Pretty MixedVariable where
  pretty = \case
    UserVar v -> pretty v
    NetworkVar v -> pretty v

instance Variable MixedVariable where
  variableDimensions = \case
    UserVar v -> variableDimensions v
    NetworkVar v -> variableDimensions v

  reduceVariable dbLevel = \case
    UserVar v -> first (fmap UserVar) $ reduceUserVariable dbLevel v
    NetworkVar v -> first (fmap NetworkVar) $ reduceNetworkVariable dbLevel v

  toMixedVariable = id

isUserVariable :: MixedVariable -> Bool
isUserVariable = \case
  UserVar {} -> True
  NetworkVar {} -> False

getNetworkVariable :: MixedVariable -> Maybe NetworkVariable
getNetworkVariable = \case
  UserVar {} -> Nothing
  NetworkVar v -> Just v

getUserVariable :: MixedVariable -> Maybe UserVariable
getUserVariable = \case
  UserVar v -> Just v
  NetworkVar {} -> Nothing

data MixedVariables = MixedVariables
  { userVariableCtx :: BoundCtx UserVariable,
    networkVariableCtx :: BoundCtx NetworkVariable
  }

mixedVariableCtx :: MixedVariables -> BoundCtx MixedVariable
mixedVariableCtx MixedVariables {..} =
  fmap NetworkVar networkVariableCtx <> fmap UserVar userVariableCtx

mixedVariableDBCtx :: MixedVariables -> BoundDBCtx
mixedVariableDBCtx ctx = fmap (Just . layoutAsText . pretty) (mixedVariableCtx ctx)

--------------------------------------------------------------------------------
-- Constants

type Constant = Vector Double

scaleConstant :: Double -> Constant -> Constant
scaleConstant v = Vector.map (v *)

addConstants :: Double -> Constant -> Double -> Constant -> Constant
addConstants a xs b ys = Vector.zipWith (\x y -> a * x + b * y) xs ys

foldConstant :: forall a. (Double -> a) -> ([a] -> a) -> TensorDimensions -> Constant -> a
foldConstant mkValue mkVec dims value = go dims (Vector.toList value)
  where
    go :: TensorDimensions -> [Double] -> a
    go [] xs = mkValue (head xs)
    go (_d : ds) xs = do
      let inputVarIndicesChunks = chunksOf (product ds) xs
      let elems = fmap (go ds) inputVarIndicesChunks
      mkVec elems

constantExpr :: TensorDimensions -> Constant -> StandardNormExpr
constantExpr = foldConstant (VRatLiteral . toRational) VVecLiteral

-- | Pretty prints a constant value given a set of dimensions.
-- Note, an alternative would be to go via the Vehicle AST and pretty print
-- that, but we run into dependency cycle issues.
prettyConstant :: Bool -> TensorDimensions -> Constant -> Doc a
prettyConstant isFirst dims value
  | not isFirst && Vector.all (== 0.0) value = ""
  | not isFirst = " + " <> foldConstant pretty list dims value
  | otherwise = foldConstant pretty list dims value

--------------------------------------------------------------------------------
-- Variable assignments

-- | A single value assigned to a variable. The dimensions of the value are
-- stored by the accompanying `Variable`.
type VariableValue = Constant

type VariableAssignment variable = Map variable VariableValue

-- | Lookup the value of the variable in an assignment and remove it from the
-- assignment.
lookupAndRemove ::
  (Variable variable) =>
  VariableAssignment variable ->
  variable ->
  Maybe (VariableValue, VariableAssignment variable)
lookupAndRemove assignment var = do
  let (maybeValue, newAssignment) = Map.updateLookupWithKey (\_ _ -> Nothing) var assignment
  case maybeValue of
    Nothing -> Nothing
    Just value -> Just (value, newAssignment)

-- | Lookups the values in the variable assignment and removes them from the
-- assignment. Returns either the first missing variable or the list of values
-- and the resulting assignment.
lookupAndRemoveAll ::
  (Variable variable) =>
  VariableAssignment variable ->
  [variable] ->
  Either variable ([VariableValue], VariableAssignment variable)
lookupAndRemoveAll assignment = foldM op ([], assignment)
  where
    op (values, ass) var = case lookupAndRemove ass var of
      Nothing -> Left var
      Just (value, ass') -> Right (value : values, ass')

--------------------------------------------------------------------------------
-- Other

pattern VInfiniteQuantifier :: Quantifier -> QuantifierDomain -> [StandardNormExpr] -> StandardNormBinder -> StandardEnv -> TypeCheckedExpr -> StandardNormExpr
pattern VInfiniteQuantifier q dom args binder env body <-
  VBuiltinFunction (Quantifier q dom) (reverse -> VLam binder env body : args)
  where
    VInfiniteQuantifier q dom args binder env body =
      VBuiltinFunction (Quantifier q dom) (reverse (VLam binder env body : args))

pattern VFiniteQuantifier :: Quantifier -> StandardSpine -> StandardNormBinder -> StandardEnv -> TypeCheckedExpr -> StandardNormExpr
pattern VFiniteQuantifier q spine binder env body <-
  VFreeVar (toFiniteQuantifier -> Just q) (VFiniteQuantifierSpine spine binder env body)
  where
    VFiniteQuantifier q spine binder env body =
      VFreeVar (fromFiniteQuantifier q) (VFiniteQuantifierSpine spine binder env body)

pattern VFiniteQuantifierSpine :: StandardSpine -> StandardNormBinder -> StandardEnv -> TypeCheckedExpr -> StandardSpine
pattern VFiniteQuantifierSpine spine binder env body <- (reverse -> ExplicitArg _ (VLam binder env body) : spine)
  where
    VFiniteQuantifierSpine spine binder env body =
      reverse (ExplicitArg mempty (VLam binder env body) : spine)
