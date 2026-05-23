module Vehicle.Backend.Solver.UserVariableElimination.Core where

import Control.Monad (forM)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Data.Char.SScript (subscript)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter (brackets)
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkName)
import Vehicle.Data.Assertion
import Vehicle.Data.Bound (BoundedValue, Domain)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.Value
import Vehicle.Data.Hashing ()
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor as Tensor
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context.Class (MonadFreeContext)
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

-- | Bookkeeping information associated with an application that describes
-- the variables and corresponding expressions that replace a given
-- NetworkApplication.
data NetworkApplicationInfo = NetworkApplicationInfo
  { inputVariable :: NetworkInputTensorVariable,
    outputVariable :: NetworkOutputTensorVariable,
    inputValue :: Value Builtin
  }

type NetworkApplications = Map NetworkName (NonEmpty NetworkApplicationInfo)

toListOfApplications :: NetworkApplications -> [(NetworkName, NetworkApplicationInfo)]
toListOfApplications metaNetworkApps = do
  let flattenNetworkApps (name, apps) = fmap (name,) (NonEmpty.toList apps)
  concatMap flattenNetworkApps $ Map.toList metaNetworkApps

data GlobalCtx = GlobalCtx
  { userTensorVariables :: !(Set UserSliceVariable),
    networkTensorVariables :: !(Map NetworkInputTensorVariable NetworkOutputTensorVariable),
    networkApplications :: !NetworkApplications
  }

emptyGlobalCtx :: GlobalCtx
emptyGlobalCtx =
  GlobalCtx
    { networkTensorVariables = mempty,
      userTensorVariables = mempty,
      networkApplications = mempty
    }

addUserVarToGlobalContext ::
  (MonadLogger m, MonadTensorBoundContext m) =>
  VBinder Builtin ->
  TensorShape ->
  GlobalCtx ->
  m (UserTensorVariable, GlobalCtx)
addUserVarToGlobalContext binder shape GlobalCtx {..} = do
  let (name, p) = getNamedBinderInfo binder
  -- Create the unreduced and reduced versions of the user variables.
  tensorVar <- toSliceVar <$> addTensorBinderToContextPermenantly p name shape
  let userVar = coerce tensorVar
  let newUserVars = Set.insert userVar userTensorVariables
  let newGlobalCtx =
        GlobalCtx
          { userTensorVariables = newUserVars,
            ..
          }
  return (coerce userVar, newGlobalCtx)

-- | Creates the internal name for a network variable. Not used in the final
-- queries!
createNetworkVarName :: Name -> Int -> InputOrOutput -> Name
createNetworkVarName networkName application inputOrOutput =
  layoutAsText $
    pretty networkName
      <> pretty (fmap subscript (show application))
      <> brackets (pretty inputOrOutput)

--------------------------------------------------------------------------------
-- Monads

type MonadPropertyStructure m =
  ( MonadLogger m,
    MonadFreeContext Builtin m,
    MonadReader PropertyMetaData m,
    MonadTensorBoundContext m,
    MonadReadableNameContext m
  )

type MonadQueryStructure m =
  ( MonadPropertyStructure m,
    MonadState GlobalCtx m,
    MonadError CompileError m
  )

addNetworkApplicationToGlobalCtx ::
  (MonadPropertyStructure m, MonadState GlobalCtx m) =>
  Name ->
  NetworkContextInfo ->
  Value Builtin ->
  m (Value Builtin, Value Builtin)
addNetworkApplicationToGlobalCtx name networkInfo arg = do
  -- Can't current track network application provenance
  let p = mempty

  GlobalCtx {..} <- get
  let applicationNumber = maybe 0 length $ Map.lookup name networkApplications

  -- Create variables representing the input of the network.
  let inputVarName = createNetworkVarName name applicationNumber Input
  inputVar <- toSliceVar <$> addTensorBinderToContextPermenantly p inputVarName (inputShape networkInfo)
  let inputVarExpr = VBoundVar (toLv inputVar) []

  -- Create variables representing the output of the network.
  let outputVarName = createNetworkVarName name applicationNumber Output
  outputVar <- toSliceVar <$> addTensorBinderToContextPermenantly p outputVarName (outputShape networkInfo)
  let outputVarExpr = VBoundVar (toLv outputVar) []

  -- Create the object to store information about the application
  let appInfo =
        NetworkApplicationInfo
          { inputVariable = coerce inputVar,
            outputVariable = coerce outputVar,
            inputValue = arg
          }

  -- Update the global context
  put $
    GlobalCtx
      { networkTensorVariables = Map.insert (coerce inputVar) (coerce outputVar) networkTensorVariables,
        networkApplications = Map.insertWith (<>) name [appInfo] networkApplications,
        ..
      }

  return (inputVarExpr, outputVarExpr)

createSubstitutionForVariable ::
  forall m variable.
  (MonadReadableTensorBoundContext m, MonadLogger m, SliceVariableLike variable) =>
  variable ->
  LinearEquality ->
  m (LinearSubstitution SliceVariable, CompilationStep)
createSubstitutionForVariable varToSolveFor (NormalisedRelation () linearExpr) = do
  nestedVar <- lookupNestedSliceVariable varToSolveFor
  let (_, rearrangedExpr) = rearrangeExprToSolveFor (toSliceVar varToSolveFor) linearExpr
  varSubsts <- go nestedVar rearrangedExpr
  let step = SolveEquality nestedVar rearrangedExpr
  return (Map.fromList varSubsts, step)
  where
    go ::
      NestedSliceVariable ->
      LinearExpression ->
      m [(SliceVariable, LinearExpression)]
    go var rearrangedExpr = do
      childSubsts <- case childVariablesOf var of
        Nothing -> return mempty
        Just childVars -> do
          xs <- forM (zip childVars [0 ..]) $ \(childVar, index) -> do
            childExpr <- reduceLinearExprAt lookupChildVariablesCertain rearrangedExpr index
            go childVar childExpr
          let childSolutions = xs
          return (concat childSolutions)

      return $ (toSliceVar var, rearrangedExpr) : childSubsts

data BoundedAssertions inputVariable variable constant = BoundedAssertions
  { variableBounds :: [BoundedValue inputVariable (Domain constant)],
    assertions :: ConjunctAll (Assertion (LinearExpr variable constant))
  }

--------------------------------------------------------------------------------
-- Partitions

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
partitionsSize = trivialElim (const 0) (length . partitionsToDisjuncts)

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
disjunctPartitions = foldr1 orPartitions

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
