{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Queries.UserVariableElimination.Core where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), StateT, gets)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
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
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView (RatTensorValue (VRatStackTensor), TypeValue (..), fromRatTensorValue, fromTypeValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.Hashing ()
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor as Tensor
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Interface (QueryFormat)
import Vehicle.Verify.Specification

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

-- | A single application of a neural network to a set of arguments.
type NetworkApplication = (Name, NetworkAppArgs (LinearExpr TensorVariable RatTensor))

-- | Bookkeeping information associated with an application that describes
-- the variables and corresponding expressions that replace a given
-- NetworkApplication.
data NetworkApplicationInfo = NetworkApplicationInfo
  { inputVariable :: NetworkIOVariable,
    outputVariable :: NetworkIOVariable,
    inputValue :: Value Builtin
  }

type NetworkApplicationReplacements = Map Name (NonEmpty NetworkApplicationInfo)

toListOfApplications :: NetworkApplicationReplacements -> [(Name, NetworkApplicationInfo)]
toListOfApplications metaNetworkApps = do
  let flattenNetworkApps (name, apps) = fmap (name,) (NonEmpty.toList apps)
  concatMap flattenNetworkApps $ Map.toList metaNetworkApps

data GlobalCtx = GlobalCtx
  { globalBoundVarCtx :: !(GenericBoundCtx TensorVariableInfo),
    userTensorVariables :: !(Set UserVariable),
    networkTensorVariables :: !(Map NetworkIOVariable NetworkIOVariable),
    networkApplications :: !NetworkApplicationReplacements
  }

emptyGlobalCtx :: GlobalCtx
emptyGlobalCtx =
  GlobalCtx
    { globalBoundVarCtx = mempty,
      networkTensorVariables = mempty,
      userTensorVariables = mempty,
      networkApplications = mempty
    }

completeNamedCtx :: GlobalCtx -> CompleteNamedBoundCtx
completeNamedCtx GlobalCtx {..} = fmap variableName globalBoundVarCtx

lookupTensorVariableInfo ::
  (TensorVariableLike variable) =>
  GlobalCtx ->
  variable ->
  TensorVariableInfo
lookupTensorVariableInfo GlobalCtx {..} var =
  lookupLvInBoundCtx (toLv var) globalBoundVarCtx

lookupTensorVariableShape ::
  (TensorVariableLike variable) =>
  GlobalCtx ->
  variable ->
  TensorShape
lookupTensorVariableShape ctx var = do
  let info = lookupTensorVariableInfo ctx var
  case childrenVariables info of
    Nothing -> mempty
    Just (children, _) -> shapeOf children

lookupChildVariablesExpr ::
  (TensorVariableLike variable) =>
  GlobalCtx ->
  variable ->
  Maybe (Value Builtin)
lookupChildVariablesExpr ctx var = do
  let userInfo = lookupTensorVariableInfo ctx var
  snd <$> childrenVariables userInfo

lookupChildVariables ::
  (TensorVariableLike variable) =>
  GlobalCtx ->
  variable ->
  Maybe (Tensor TensorVariable)
lookupChildVariables ctx var = do
  let userInfo = lookupTensorVariableInfo ctx var
  fst <$> childrenVariables userInfo

lookupZeroDimVariables ::
  (TensorVariableLike variable) =>
  GlobalCtx ->
  variable ->
  Tensor TensorVariable
lookupZeroDimVariables ctx var =
  fromMaybe (ZeroDimTensor (toTensorVar var)) (lookupChildVariables ctx var)

lookupCorrespondingOutputVar :: GlobalCtx -> NetworkIOVariable -> NetworkIOVariable
lookupCorrespondingOutputVar ctx inputVar =
  case Map.lookup inputVar (networkTensorVariables ctx) of
    Just outputVar -> outputVar
    Nothing -> do
      let varName = lookupLvInBoundCtx (toLv inputVar) (completeNamedCtx ctx)
      developerError ("Network input var" <+> quotePretty varName <+> "has no corresponding output variable")

addVectorVarToBoundVarCtx :: [TensorVariableInfo] -> GenericBoundCtx TensorVariableInfo -> GenericBoundCtx TensorVariableInfo
addVectorVarToBoundVarCtx newVars ctx = reverse newVars <> ctx

reduceTensorVariable ::
  forall variable.
  (TensorVariableLike variable) =>
  variable ->
  Name ->
  TensorShape ->
  [TensorVariableInfo]
reduceTensorVariable var varName shape = do
  let (reducedVariablesInfo, reducedVariables) = case shape of
        [] -> (mempty, Nothing)
        _ -> do
          let (reducedVarsInfo, tensors) = runSupply [toLv var + 1 ..] $ go shape []

          (reducedVarsInfo, Just (tensors, tensorVariablesToExpr tensors))
  let variableInfo =
        TensorVariableInfo
          { variableName = varName,
            parentVariable = Nothing,
            childrenVariables = reducedVariables
          }
  variableInfo : reducedVariablesInfo
  where
    elementVariable ::
      TensorIndices ->
      Lv ->
      ([TensorVariableInfo], Tensor TensorVariable)
    elementVariable indices currentLv = do
      let name = varName <> Text.pack (showTensorIndices indices)
      let tensorVariableInfo = TensorVariableInfo name (Just (toTensorVar var, indices)) Nothing
      ([tensorVariableInfo], ZeroDimTensor $ fromLv currentLv)

    go ::
      TensorShape ->
      TensorIndices ->
      Supply Lv ([TensorVariableInfo], Tensor TensorVariable)
    go dims indices = case dims of
      [] -> elementVariable (reverse indices) <$> demand
      d : ds -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. d - 1]

        -- Generate the corresponding names from the indices
        (elementVarNames, elementVars) <- unzip <$> traverse (\i -> go ds (i : indices)) allIndices
        let varsNames = concat elementVarNames
        let vars = stack ds elementVars
        return (varsNames, vars)

tensorVariablesToExpr :: Tensor TensorVariable -> Value Builtin
tensorVariablesToExpr = foldMapTensor mkElem mkRow
  where
    mkElem :: TensorVariable -> Value Builtin
    mkElem v = VBoundVar (toLv v) mempty

    mkRow :: TensorShape -> [Value Builtin] -> Value Builtin
    mkRow ds xs = do
      let dim = INatLiteral (length xs)
      let dims = mkDims ds
      let typ = fromTypeValue $ VRatTensorType dims
      let args = StackTensorArgs (implicit typ) dim (implicit dims) xs
      fromRatTensorValue $ VRatStackTensor args

addUserVarToGlobalContext ::
  (MonadLogger m) =>
  Name ->
  TensorShape ->
  GlobalCtx ->
  m (UserVariable, GlobalCtx)
addUserVarToGlobalContext userVarName shape GlobalCtx {..} = do
  -- Create the unreduced and reduced versions of the user variables.
  let userVar = UserVariable $ Lv $ length globalBoundVarCtx
  let newVarsTelescope = reduceTensorVariable userVar userVarName shape
  let newCtx = addVectorVarToBoundVarCtx newVarsTelescope globalBoundVarCtx
  let newUserVars = Set.insert userVar userTensorVariables
  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = newCtx,
            userTensorVariables = newUserVars,
            ..
          }
  return (userVar, newGlobalCtx)

addNetworkApplicationToGlobalCtx ::
  (MonadLogger m) =>
  Name ->
  NetworkContextInfo ->
  GlobalCtx ->
  Value Builtin ->
  m (Value Builtin, Value Builtin, GlobalCtx)
addNetworkApplicationToGlobalCtx name networkInfo GlobalCtx {..} arg = do
  let applicationNumber = maybe 0 length $ Map.lookup name networkApplications
  let ctxSize = length globalBoundVarCtx

  -- Create a single variable for the input of the network to
  -- (avoiding prematurely normalising so that we can potentially solve
  -- user tensor variables in terms of it).
  let inputVar = NetworkIOVariable $ Lv ctxSize
  let inputShape = dimensions (inputTensor (networkType networkInfo))
  let inputVarName = layoutAsText $ createNetworkVarName name applicationNumber Input
  let inputVarsTelescope = reduceTensorVariable inputVar inputVarName inputShape
  let inputVarExpr = VBoundVar (toLv inputVar) []

  -- Create a tensor of variables for the output of the network.
  let outputVar = NetworkIOVariable $ Lv (ctxSize + length inputVarsTelescope)
  let outputShape = dimensions (outputTensor (networkType networkInfo))
  let outputVarName = layoutAsText $ createNetworkVarName name applicationNumber Output
  let outputVarsTelescope = reduceTensorVariable outputVar outputVarName outputShape
  let outputVarExpr = VBoundVar (toLv outputVar) []

  -- Create the context extension of the bound context.
  let newGlobalBoundVarCtx =
        addVectorVarToBoundVarCtx outputVarsTelescope $
          addVectorVarToBoundVarCtx inputVarsTelescope globalBoundVarCtx

  -- Create the object to store information about the application
  let appInfo =
        NetworkApplicationInfo
          { inputVariable = inputVar,
            outputVariable = outputVar,
            inputValue = arg
          }

  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = newGlobalBoundVarCtx,
            networkTensorVariables = Map.insert inputVar outputVar networkTensorVariables,
            networkApplications = Map.insertWith (<>) name [appInfo] networkApplications,
            ..
          }

  return (inputVarExpr, outputVarExpr, newGlobalCtx)

instance (Monad m) => MonadBoundContext () (StateT GlobalCtx m) where
  addBinderToContext = developerError "Cannot add binder to context in GlobalCtx"
  getBoundCtx _p = do
    nameCtx <- gets globalBoundVarCtx
    return $ map (mkExplicitBinder () . Just . variableName) nameCtx

--------------------------------------------------------------------------------
-- Partitions

-- | An `AssertionTree` represents a boolean expression with assertions at
-- each terminal leaf.
type AssertionTree variable = BooleanExpr (Assertion variable)

-- | A partition is an `AssertionTree` in which all variables belong to a
-- consistent mapping of user variables to tensor variables.
type Partition variable = ([CompilationStep], AssertionTree variable)

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
  = Partitions (Map [CompilationStep] (AssertionTree variable))

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
  ([CompilationStep], MaybeTrivial (AssertionTree variable)) ->
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

lookupNetworkElementVariables ::
  GlobalCtx ->
  NetworkIOVariable ->
  Tensor TensorVariable
lookupNetworkElementVariables globalCtx var =
  case lookupChildVariables globalCtx var of
    Just childVariables -> coerce childVariables
    Nothing -> coerce $ ZeroDimTensor var

reduceTensorExpr ::
  GlobalCtx ->
  LinearExpr TensorVariable RatTensor ->
  [LinearExpr TensorVariable RatTensor]
reduceTensorExpr globalCtx (Sparse coeff constant) = do
  let equationIDs = [0 .. product (shapeOf constant) - 1]
  let constValues = Tensor.toList constant
  let malformedVariableError = developerError "Expecting a non-zero tensor variable"
  let findChildVariables var = fromMaybe malformedVariableError $ lookupChildVariables globalCtx var
  let findChildVariablesAndCoefficient = first (Tensor.toList . findChildVariables)
  let coeffList = fmap findChildVariablesAndCoefficient (Map.toList coeff)
  fmap (mkZeroDimEquality coeffList constValues) equationIDs
  where
    mkZeroDimEquality ::
      [([TensorVariable], Coefficient)] ->
      [Rational] ->
      Int ->
      LinearExpr TensorVariable RatTensor
    mkZeroDimEquality coeffs consts i =
      Sparse (Map.fromList (fmap (first (!! i)) coeffs)) (ZeroDimTensor (consts !! i))

createSubstitutionForVariable ::
  (MonadCompile m) =>
  GlobalCtx ->
  TensorVariable ->
  Equality TensorVariable RatTensor ->
  m (LinearSubstitution TensorVariable, CompilationStep)
createSubstitutionForVariable ctx var (NormalisedRelation () linearExpr) = do
  let (_, rearrangedExpr) = rearrangeExprToSolveFor var linearExpr
  let varEquality = (var, rearrangedExpr)
  let childVars = maybe [] Tensor.toList (lookupChildVariables ctx var)
  let childEqualities = zip childVars $ reduceTensorExpr ctx rearrangedExpr
  let substitution = Map.fromList $ varEquality : childEqualities
  return (substitution, SolveEquality var childVars rearrangedExpr)

--------------------------------------------------------------------------------
-- Context operations

variableCtxToBoundCtx :: (Pretty variable) => [variable] -> BoundCtx (Type builtin)
variableCtxToBoundCtx ctx = zipWith variableCtxToBoundCtxEntry [0 .. Ix (length ctx - 1)] ctx
  where
    variableCtxToBoundCtxEntry ix var = mkExplicitBinder (BoundVar mempty ix) (Just (layoutAsText $ pretty var))
