module Vehicle.Backend.Queries.UserVariableElimination.Core where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), StateT, gets)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Internal.Check (HasCallStack)
import Vehicle.Compile.Context.Bound (MonadBoundContext)
import Vehicle.Compile.Context.Free.Class (MonadFreeContext)
import Vehicle.Compile.Context.Name (MonadNameContext)
import Vehicle.Compile.Context.Var (MonadBoundContext (..))
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
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

class (MonadNameContext m) => MonadTensorVariableContext m where
  lookupChildVariables :: SliceVariable -> m (Maybe [SliceVariable])

--------------------------------------------------------------------------------
-- Global state

-- | A single application of a neural network to a set of arguments.
type NetworkApplication = (Name, NetworkAppArgs (LinearExpr SliceVariable RatTensor))

-- | Bookkeeping information associated with an application that describes
-- the variables and corresponding expressions that replace a given
-- NetworkApplication.
data NetworkApplicationInfo = NetworkApplicationInfo
  { inputVariable :: NetworkInputTensorVariable,
    outputVariable :: NetworkOutputTensorVariable,
    inputValue :: Value Builtin
  }

type NetworkApplications = Map Name (NonEmpty NetworkApplicationInfo)

toListOfApplications :: NetworkApplications -> [(Name, NetworkApplicationInfo)]
toListOfApplications metaNetworkApps = do
  let flattenNetworkApps (name, apps) = fmap (name,) (NonEmpty.toList apps)
  concatMap flattenNetworkApps $ Map.toList metaNetworkApps

data GlobalCtx = GlobalCtx
  { globalBoundVarCtx :: !NestedTensorVariableCtx,
    userTensorVariables :: !(Set UserVariable),
    networkTensorVariables :: !(Map NetworkInputTensorVariable NetworkOutputTensorVariable),
    networkApplications :: !NetworkApplications
  }

emptyGlobalCtx :: GlobalCtx
emptyGlobalCtx =
  GlobalCtx
    { globalBoundVarCtx = emptyNestedCtx,
      networkTensorVariables = mempty,
      userTensorVariables = mempty,
      networkApplications = mempty
    }

completeNamedCtx :: GlobalCtx -> CompleteNamedBoundCtx
completeNamedCtx GlobalCtx {..} = nestedCtxToNameCtx globalBoundVarCtx

lookupChildVariablesExpr ::
  (SliceVariableLike variable) =>
  GlobalCtx ->
  variable ->
  Maybe (Value Builtin)
lookupChildVariablesExpr ctx var = do
  let nestedVar = findCorrespondingSliceVariable (globalBoundVarCtx ctx) var
  case (childVariablesOf nestedVar, shapeOf nestedVar) of
    (Nothing, []) -> Nothing
    (Just childVars, d : ds) -> Just $ do
      let dim = INatLiteral d
      let dims = implicit $ mkDims ds
      let varExprs = flip map childVars $ \v -> VBoundVar (toLv v) []
      let args = StackTensorArgs (implicit IRatType) dim dims varExprs
      fromRatTensorValue $ VRatStackTensor args
    _ -> developerError "mismatched children and shape"

lookupChildVariablesCertain ::
  (SliceVariableLike variable, HasCallStack) =>
  GlobalCtx ->
  variable ->
  [SliceVariable]
lookupChildVariablesCertain ctx var = do
  let maybeChildVariables = childVariablesOf $ findCorrespondingSliceVariable (globalBoundVarCtx ctx) var
  case maybeChildVariables of
    Nothing -> developerError "Expecting a non-zero tensor variable"
    Just childVars -> fmap toSliceVar childVars

lookupCorrespondingOutputVar ::
  GlobalCtx ->
  NetworkInputTensorVariable ->
  NetworkOutputTensorVariable
lookupCorrespondingOutputVar ctx inputVar =
  case Map.lookup inputVar (networkTensorVariables ctx) of
    Just outputVar -> outputVar
    Nothing -> do
      let varName = snd $ lookupTensorVariableAndName (globalBoundVarCtx ctx) (coerce inputVar)
      developerError ("Network input var" <+> quotePretty varName <+> "has no corresponding output variable")

tensorVariablesToExpr :: Tensor SliceVariable -> Value Builtin
tensorVariablesToExpr = foldMapTensor mkElem mkRow
  where
    mkElem :: SliceVariable -> Value Builtin
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
  m (UserTensorVariable, GlobalCtx)
addUserVarToGlobalContext userVarName shape GlobalCtx {..} = do
  -- Create the unreduced and reduced versions of the user variables.
  let (tensorVar, newCtx) = appendTensorVariableToNestedCtx globalBoundVarCtx (userVarName, shape)
  let userVar = coerce tensorVar
  let newUserVars = Set.insert userVar userTensorVariables
  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = newCtx,
            userTensorVariables = newUserVars,
            ..
          }
  return (coerce userVar, newGlobalCtx)

addNetworkApplicationToGlobalCtx ::
  (MonadLogger m) =>
  Name ->
  NetworkContextInfo ->
  GlobalCtx ->
  Value Builtin ->
  m (Value Builtin, Value Builtin, GlobalCtx)
addNetworkApplicationToGlobalCtx name networkInfo GlobalCtx {..} arg = do
  let applicationNumber = maybe 0 length $ Map.lookup name networkApplications

  -- Create variables representing the input of the network.
  let inputShape = dimensions (inputTensor (networkType networkInfo))
  let inputVarName = layoutAsText $ createNetworkVarName name applicationNumber Input
  let (inputVar, ctxWithInput) = appendTensorVariableToNestedCtx globalBoundVarCtx (inputVarName, inputShape)
  let inputVarExpr = VBoundVar (toLv inputVar) []

  -- Create variables representing the output of the network.
  let outputShape = dimensions (outputTensor (networkType networkInfo))
  let outputVarName = layoutAsText $ createNetworkVarName name applicationNumber Output
  let (outputVar, finalCtx) = appendTensorVariableToNestedCtx ctxWithInput (outputVarName, outputShape)
  let outputVarExpr = VBoundVar (toLv outputVar) []

  -- Create the object to store information about the application
  let appInfo =
        NetworkApplicationInfo
          { inputVariable = coerce inputVar,
            outputVariable = coerce outputVar,
            inputValue = arg
          }

  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = finalCtx,
            networkTensorVariables = Map.insert (coerce inputVar) (coerce outputVar) networkTensorVariables,
            networkApplications = Map.insertWith (<>) name [appInfo] networkApplications,
            ..
          }

  return (inputVarExpr, outputVarExpr, newGlobalCtx)

--------------------------------------------------------------------------------
-- Partitions

type LinearAssertion = Assertion SliceVariable

-- | An `AssertionTree` represents a boolean expression with assertions at
-- each terminal leaf.
type LinearAssertionTree = BooleanExpr LinearAssertion

-- | A partition is an `AssertionTree` in which all variables belong to a
-- consistent mapping of user variables to tensor variables.
type Partition = ([CompilationStep], LinearAssertionTree)

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
newtype Partitions = Partitions (Map [CompilationStep] LinearAssertionTree)

partitionsSize :: MaybeTrivial Partitions -> Int
partitionsSize = trivial (const 0) (length . partitionsToDisjuncts)

singletonPartition :: Partition -> Partitions
singletonPartition (steps, tree) = Partitions $ Map.singleton steps tree

partitionsToDisjuncts :: Partitions -> DisjunctAll Partition
partitionsToDisjuncts (Partitions ps) = case Map.toList ps of
  [] -> developerError "Empty partition"
  x : xs -> DisjunctAll $ x :| xs

andPartitions :: Partitions -> Partitions -> Partitions
andPartitions xs ys = do
  let combine (s1, t1) (s2, t2) = (s1 <> s2, andBoolExpr t1 t2)
  let disjuncts = conjunctDisjuncts combine (partitionsToDisjuncts xs) (partitionsToDisjuncts ys)
  Partitions $ Map.fromList $ disjunctsToList disjuncts

orPartitions :: Partitions -> Partitions -> Partitions
orPartitions (Partitions p1) (Partitions p2) = do
  Partitions $ Map.unionWith orBoolExpr p1 p2

disjunctPartitions :: DisjunctAll Partitions -> Partitions
disjunctPartitions (DisjunctAll ps) = foldr1 orPartitions ps

disjunctMaybeTrivialPartitions :: DisjunctAll (MaybeTrivial Partitions) -> MaybeTrivial Partitions
disjunctMaybeTrivialPartitions = fmap disjunctPartitions . eliminateTrivialDisjunctions

mkSingletonPartitions ::
  ([CompilationStep], MaybeTrivial LinearAssertionTree) ->
  MaybeTrivial Partitions
mkSingletonPartitions (steps, maybeAssertion) =
  fmap (\x -> singletonPartition (steps, x)) maybeAssertion

mkTrivialPartition :: LinearAssertion -> MaybeTrivial Partitions
mkTrivialPartition assertion =
  mkSingletonPartitions (mempty, NonTrivial $ Query assertion)

instance (Monad m) => MonadBoundContext () (StateT GlobalCtx m) where
  addBinderToContext = developerError "Cannot add binder to context in GlobalCtx"
  getBoundCtx _p = do
    nameCtx <- gets (nestedCtxToNameCtx . globalBoundVarCtx)
    return $ map (mkExplicitBinder () . Just) nameCtx

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

getNestedTensorVariable ::
  (MonadQueryStructure m, TensorVariableLike variable) =>
  variable ->
  m NestedSliceVariable
getNestedTensorVariable var = gets (flip lookupTensorVariable var . globalBoundVarCtx)

createSubstitutionForVariable ::
  forall m variable.
  (MonadCompile m, SliceVariableLike variable) =>
  GlobalCtx ->
  variable ->
  Equality SliceVariable RatTensor ->
  m (LinearSubstitution SliceVariable, CompilationStep)
createSubstitutionForVariable ctx varToSolveFor (NormalisedRelation () linearExpr) = do
  let nestedVar = findCorrespondingSliceVariable (globalBoundVarCtx ctx) varToSolveFor
  let (_, rearrangedExpr) = rearrangeExprToSolveFor (toSliceVar varToSolveFor) linearExpr
  varSubsts <- go nestedVar rearrangedExpr
  let step = SolveEquality nestedVar rearrangedExpr
  return (Map.fromList varSubsts, step)
  where
    go ::
      NestedSliceVariable ->
      LinearExpr SliceVariable RatTensor ->
      m [(SliceVariable, LinearExpr SliceVariable RatTensor)]
    go var rearrangedExpr = do
      childSubsts <- case childVariablesOf var of
        Nothing -> return mempty
        Just childVars -> do
          xs <- forM (zip childVars [0 ..]) $ \(childVar, index) -> do
            let childExpr = reduceLinearExprAt (lookupChildVariablesCertain ctx) rearrangedExpr index
            go childVar childExpr
          let childSolutions = xs
          return (concat childSolutions)

      return $ (toSliceVar var, rearrangedExpr) : childSubsts

--------------------------------------------------------------------------------
-- Context operations

variableCtxToBoundCtx :: (Pretty variable) => [variable] -> BoundCtx (Type builtin)
variableCtxToBoundCtx ctx = zipWith variableCtxToBoundCtxEntry [0 .. Ix (length ctx - 1)] ctx
  where
    variableCtxToBoundCtxEntry ix var = mkExplicitBinder (BoundVar mempty ix) (Just (layoutAsText $ pretty var))
