{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Queries.UserVariableElimination.Core where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), StateT, gets)
import Data.Bifunctor (Bifunctor (..))
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Compile.Context.Bound.Class (MonadBoundContext (..))
import Vehicle.Compile.Context.Free.Class (MonadFreeContext)
import Vehicle.Compile.Context.Name (MonadNameContext, getNameContext)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Resource (NetworkType (..), dimensions)
import Vehicle.Data.Assertion
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Interface (NetworkAppArgs)
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.Value
import Vehicle.Data.Hashing ()
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Specification

--------------------------------------------------------------------------------
-- Network applications

-- | A single application of a neural network to a set of arguments.
type NetworkApplication = (Name, NetworkAppArgs (Value Builtin))

-- | Bookkeeping information associated with an application that describes
-- the variables and corresponding expressions that replace a given
-- NetworkApplication.
data NetworkApplicationReplacement = NetworkApplicationReplacement
  { networkApp :: NetworkApplication,
    networkInfo :: NetworkContextInfo,
    inputVariable :: NetworkTensorVariable,
    outputVarExpr :: Value Builtin,
    outputVariable :: NetworkTensorVariable
  }

--------------------------------------------------------------------------------
-- Reader state

data PropertyMetaData = PropertyMetaData
  { queryFormat :: QueryFormat,
    networkCtx :: NetworkContext,
    propertyProvenance :: DeclProvenance,
    propertyAddress :: PropertyAddress,
    outputLocation :: Maybe FilePath
  }

--------------------------------------------------------------------------------
-- Global state

data GlobalCtx = GlobalCtx
  { globalBoundVarCtx :: !(GenericBoundCtx Name),
    userTensorVariableInfo :: !(Map UserTensorVariable TensorVariableInfo),
    networkTensorVariableInfo :: !(Map NetworkTensorVariable TensorVariableInfo),
    networkApplications :: !(LinkedHashMap NetworkApplication NetworkApplicationReplacement)
  }

emptyGlobalCtx :: GlobalCtx
emptyGlobalCtx =
  GlobalCtx
    { globalBoundVarCtx = mempty,
      networkTensorVariableInfo = mempty,
      userTensorVariableInfo = mempty,
      networkApplications = LinkedHashMap.empty
    }

addVectorVarToBoundVarCtx :: Name -> [Name] -> GenericBoundCtx Name -> GenericBoundCtx Name
addVectorVarToBoundVarCtx tensorVar elementVars ctx = reverse elementVars <> [tensorVar] <> ctx

addUserVarToGlobalContext ::
  (MonadLogger m) =>
  Name ->
  TensorShape ->
  GlobalCtx ->
  m (UserTensorVariable, GlobalCtx)
addUserVarToGlobalContext userVarName shape GlobalCtx {..} = do
  -- Create the unreduced and reduced versions of the user variables.
  let currentLevel = Lv $ length globalBoundVarCtx
  let (reducedVariableNames, reducedVariables, reducedVariablesExpr) = reduceTensorVariable currentLevel userVarName shape
  let userVar = mkUserTensorVariable currentLevel
  let variableInfo =
        TensorVariableInfo
          { elementVariables = reducedVariables,
            reducedVarExpr = reducedVariablesExpr,
            tensorVariableType = Nothing
          }
  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = addVectorVarToBoundVarCtx userVarName reducedVariableNames globalBoundVarCtx,
            userTensorVariableInfo = Map.insert userVar variableInfo userTensorVariableInfo,
            ..
          }
  return (userVar, newGlobalCtx)

addNetworkApplicationToGlobalCtx ::
  (MonadLogger m) =>
  NetworkApplication ->
  NetworkContextInfo ->
  GlobalCtx ->
  m (Value Builtin, Value Builtin, GlobalCtx)
addNetworkApplicationToGlobalCtx app@(networkName, _) networkInfo GlobalCtx {..} = do
  let metaNetworkSoFar = LinkedHashMap.toList networkApplications
  let applicationNumber = length $ filter (\((name, _), _) -> name == networkName) metaNetworkSoFar

  -- Create a single variable for the input of the network to
  -- (avoiding prematurely normalising so that we can potentially solve
  -- user tensor variables in terms of it).
  let inputLv = Lv $ length globalBoundVarCtx
  let inputShape = dimensions (inputTensor (networkType networkInfo))
  let inputVarName = layoutAsText $ createNetworkVarName networkName applicationNumber Input
  let inputVar = mkNetworkTensorVariable inputLv
  let (reducedInputVarNames, reducedInputVars, reducedInputVarsExpr) = reduceTensorVariable inputLv inputVarName inputShape
  let inputVarExpr = VBoundVar inputLv []

  let inputVarInfo =
        TensorVariableInfo
          { elementVariables = reducedInputVars,
            reducedVarExpr = reducedInputVarsExpr,
            tensorVariableType = Just Input
          }

  -- Create a tensor of variables for the output of the network.
  let outputLv = inputLv + 1 + Lv (length reducedInputVarNames)
  let outputShape = dimensions (outputTensor (networkType networkInfo))
  let outputVarName = layoutAsText $ createNetworkVarName networkName applicationNumber Output
  let outputVar = mkNetworkTensorVariable outputLv
  let (reducedOutputVarNames, reducedOutputVars, reducedOutputVarsExpr) = reduceTensorVariable outputLv outputVarName outputShape
  let outputVarExpr = VBoundVar outputLv []
  let outputVarInfo =
        TensorVariableInfo
          { elementVariables = reducedOutputVars,
            reducedVarExpr = reducedOutputVarsExpr,
            tensorVariableType = Just Output
          }

  -- Create the context extension of the bound context.
  let newGlobalBoundVarCtx =
        addVectorVarToBoundVarCtx outputVarName reducedOutputVarNames $
          addVectorVarToBoundVarCtx inputVarName reducedInputVarNames globalBoundVarCtx

  -- Create the object to store information about the application
  let appInfo =
        NetworkApplicationReplacement
          { networkApp = app,
            networkInfo = networkInfo,
            inputVariable = inputVar,
            outputVarExpr = outputVarExpr,
            outputVariable = outputVar
          }

  let newTensorVariableInfo =
        Map.insert inputVar inputVarInfo $
          Map.insert outputVar outputVarInfo networkTensorVariableInfo

  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = newGlobalBoundVarCtx,
            networkTensorVariableInfo = newTensorVariableInfo,
            networkApplications = LinkedHashMap.insert app appInfo networkApplications,
            ..
          }

  return (inputVarExpr, outputVarExpr, newGlobalCtx)

instance (Monad m) => MonadBoundContext () (StateT GlobalCtx m) where
  addBinderToContext = developerError "Cannot add binder to context in GlobalCtx"
  getBoundCtx _p = do
    nameCtx <- gets globalBoundVarCtx
    return $ map (mkExplicitBinder () . Just) nameCtx

--------------------------------------------------------------------------------
-- Partitions

-- | An `AssertionTree` represents a boolean expression with assertions at
-- each terminal leaf.
type AssertionTree variable = BooleanExpr (Assertion variable)

-- | A partition is an `AssertionTree` in which all variables belong to a
-- consistent mapping of user variables to tensor variables.
type Partition variable = ([UserVariableCompilationStep], AssertionTree variable)

-- | A `Partitions` object represents the intermediate state of query compilation.
-- It is implicitly a disjunction of set of query trees, each of which has a
-- unique solution for the user variables in terms of network variables. e.g.
--
--   Vehicle expression:
--     exists u. f [u] >= 2 or f[u+2] >= 2
--
--   maps to
--
--   Partitions:
--     1. x0 = u     && y0 >= 2
--    OR
--     2. x0 = u + 2 && y0 >= 2
newtype Partitions variable
  = Partitions (Map [UserVariableCompilationStep] (AssertionTree variable))

partitionsToDisjuncts :: Partitions variable -> DisjunctAll (Partition variable)
partitionsToDisjuncts (Partitions ps) = DisjunctAll $ NonEmpty.fromList $ Map.toList ps

andPartitions :: Partitions variable -> Partitions variable -> Partitions variable
andPartitions (Partitions xs) (Partitions ys) = do
  let xs' = Map.toList xs
  let ys' = Map.toList ys
  let combine (s1, t1) (s2, t2) = (s1 <> s2, andBoolExpr t1 t2)
  Partitions $ Map.fromList $ cartesianProduct combine xs' ys'

orPartitions :: Partitions variable -> Partitions variable -> Partitions variable
orPartitions (Partitions p1) (Partitions p2) =
  Partitions $ Map.unionWith orBoolExpr p1 p2

mkSingletonPartitions ::
  ([UserVariableCompilationStep], MaybeTrivial (AssertionTree variable)) ->
  MaybeTrivial (Partitions variable)
mkSingletonPartitions (solutions, maybeAssertion) =
  fmap (Partitions . Map.singleton solutions) maybeAssertion

mkTrivialPartition :: Assertion variable -> MaybeTrivial (Partitions variable)
mkTrivialPartition assertion =
  mkSingletonPartitions (mempty, NonTrivial $ Query assertion)

--------------------------------------------------------------------------------
-- Monads

type MonadPropertyStructure m =
  ( MonadFreeContext Builtin m,
    MonadReader PropertyMetaData m,
    MonadCompile m
  )

type MonadQueryStructure m =
  ( MonadPropertyStructure m,
    MonadState GlobalCtx m,
    MonadNameContext m
  )

prettyFriendlyInCtx :: (MonadNameContext m, PrettyFriendly (Contextualised a NamedBoundCtx)) => a -> m (Doc b)
prettyFriendlyInCtx e = prettyFriendly . WithContext e <$> getNameContext

prettyExternalInCtx :: (MonadNameContext m, PrettyExternal (Contextualised a NamedBoundCtx)) => a -> m (Doc b)
prettyExternalInCtx e = prettyExternal . WithContext e <$> getNameContext

getNetworkElementVariables :: GlobalCtx -> NetworkTensorVariable -> Tensor NetworkElementVariable
getNetworkElementVariables GlobalCtx {..} var = do
  case Map.lookup var networkTensorVariableInfo of
    Just info -> elementVariables info
    Nothing ->
      developerError $
        "Variable"
          <+> quotePretty (lookupLvInBoundCtx (toLv var) globalBoundVarCtx)
          <+> "has no associated meta-information"

getTensorVariableInfo ::
  (MonadState GlobalCtx m, MonadLogger m) =>
  Lv ->
  m (Maybe TensorVariableInfo)
getTensorVariableInfo var = do
  GlobalCtx {..} <- get
  let userInfo = Map.lookup (mkUserTensorVariable var) userTensorVariableInfo
  let networkInfo = Map.lookup (mkNetworkTensorVariable var) networkTensorVariableInfo
  return (userInfo <|> networkInfo)

reduceTensorExpr ::
  GlobalCtx ->
  LinearExpr NetworkTensorVariable RatTensor ->
  [LinearExpr NetworkTensorVariable RatTensor]
reduceTensorExpr globalCtx (Sparse coeff constant) = do
  let constValues = tensorToList constant
  let numRatEqs = product (tensorShape constant)
  let coeffList = fmap (first (tensorToList . getNetworkElementVariables globalCtx)) (Map.toList coeff)
  let asserts = fmap (mkRatEquality coeffList constValues) [0 .. numRatEqs - 1]
  asserts
  where
    mkRatEquality ::
      [([NetworkTensorVariable], Coefficient)] ->
      [Rational] ->
      Int ->
      LinearExpr NetworkTensorVariable RatTensor
    mkRatEquality coeffs consts i =
      Sparse (Map.fromList (fmap (first (!! i)) coeffs)) (ZeroDimTensor (consts !! i))

--------------------------------------------------------------------------------
-- Context operations

variableCtxToBoundCtx :: (Pretty variable) => [variable] -> BoundCtx (Type builtin)
variableCtxToBoundCtx ctx = zipWith variableCtxToBoundCtxEntry [0 .. Ix (length ctx - 1)] ctx
  where
    variableCtxToBoundCtxEntry ix var = mkExplicitBinder (BoundVar mempty ix) (Just (layoutAsText $ pretty var))
