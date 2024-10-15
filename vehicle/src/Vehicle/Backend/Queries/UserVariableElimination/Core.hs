module Vehicle.Backend.Queries.UserVariableElimination.Core where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap (insert, lookup)
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import GHC.Generics
import Vehicle.Compile.Context.Free
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
import Vehicle.Data.Code.Value
import Vehicle.Data.Hashing ()
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Interface

--------------------------------------------------------------------------------
-- Builtins

type QueryBuiltin = Builtin

--------------------------------------------------------------------------------
-- Network applications

-- | A single application of a neural network to a set of arguments.
type NetworkApplication = (Name, WHNFSpine QueryBuiltin)

-- | Bookkeeping information associated with an application that describes
-- the variables and corresponding expressions that replace a given
-- NetworkApplication.
data NetworkApplicationReplacement = NetworkApplicationReplacement
  { networkApp :: NetworkApplication,
    networkInfo :: NetworkContextInfo,
    inputVariable :: TensorVariable,
    outputVarExpr :: WHNFValue QueryBuiltin,
    outputVariable :: TensorVariable
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
  { globalBoundVarCtx :: !(LinkedHashMap Lv Variable),
    userVariableReductions :: !(HashMap TensorVariable ([UserElementVariable], WHNFValue QueryBuiltin)),
    networkVariableReductions :: !(HashMap TensorVariable NetworkVariableInfo),
    networkApplications :: !(LinkedHashMap NetworkApplication NetworkApplicationReplacement)
  }

emptyGlobalCtx :: GlobalCtx
emptyGlobalCtx =
  GlobalCtx
    { globalBoundVarCtx = LinkedHashMap.empty,
      userVariableReductions = mempty,
      networkVariableReductions = mempty,
      networkApplications = LinkedHashMap.empty
    }

addVectorVarToBoundVarCtx ::
  (Lv, TensorVariable) ->
  [(Lv, ElementVariable)] ->
  LinkedHashMap Lv Variable
addVectorVarToBoundVarCtx tensorVar elemVars = do
  let tensorVar' = second TensorVar tensorVar
  let elemVars' = fmap (second RationalVar) elemVars
  LinkedHashMap.union (LinkedHashMap.fromList elemVars') (LinkedHashMap.fromList [tensorVar'])

addUserVarToGlobalContext ::
  TensorVariable ->
  GlobalCtx ->
  (WHNFValue QueryBuiltin, GlobalCtx)
addUserVarToGlobalContext userVar GlobalCtx {..} = do
  -- Create the unreduced and reduced versions of the user variables.
  let currentLevel = Lv $ length globalBoundVarCtx
  let envEntry@(reducedUseVars, _) = reduceVariable (currentLevel + 1) userVar
  let userVarExpr = VBoundVar currentLevel []
  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = LinkedHashMap.union (addVectorVarToBoundVarCtx (currentLevel, userVar) reducedUseVars) globalBoundVarCtx,
            userVariableReductions = HashMap.insert userVar (first (fmap snd) envEntry) userVariableReductions,
            ..
          }
  (userVarExpr, newGlobalCtx)

addNetworkApplicationToGlobalCtx ::
  NetworkApplication ->
  NetworkContextInfo ->
  GlobalCtx ->
  (WHNFValue QueryBuiltin, WHNFValue QueryBuiltin, GlobalCtx)
addNetworkApplicationToGlobalCtx app@(networkName, _) networkInfo GlobalCtx {..} = do
  let metaNetworkSoFar = LinkedHashMap.toList networkApplications
  let applicationNumber = length $ filter (\((name, _), _) -> name == networkName) metaNetworkSoFar

  -- Create a single variable for the input of the network to
  -- (avoiding prematurely normalising so that we can potentially solve
  -- user tensor variables in terms of it).
  let inputLv = Lv $ length globalBoundVarCtx
  let inputVar =
        TensorVariable
          { tensorVarName = layoutAsText $ createNetworkVarName networkName applicationNumber Input,
            tensorVarDimensions = dimensions (inputTensor (networkType networkInfo))
          }
  let (reducedInputVars, reducedInputVarsExpr) = reduceVariable (inputLv + 1) inputVar
  let inputVarExpr = VBoundVar inputLv []
  let inputVarInfo =
        NetworkVariableInfo
          { elementVariables = fmap snd reducedInputVars,
            reducedNetworkVarExpr = reducedInputVarsExpr
          }

  -- Create a tensor of variables for the output of the network.
  let outputLv = inputLv + 1 + Lv (length reducedInputVars)
  let outputVar =
        TensorVariable
          { tensorVarName = layoutAsText $ createNetworkVarName networkName applicationNumber Output,
            tensorVarDimensions = dimensions (outputTensor (networkType networkInfo))
          }
  let (reducedOutputVars, reducedOutputVarsExpr) = reduceVariable (outputLv + 1) outputVar
  let outputVarExpr = VBoundVar outputLv []
  let outputVarInfo =
        NetworkVariableInfo
          { elementVariables = fmap snd reducedOutputVars,
            reducedNetworkVarExpr = reducedOutputVarsExpr
          }

  -- Create the context extension of the bound context.
  let newGlobalBoundVarCtx =
        LinkedHashMap.unions
          [ addVectorVarToBoundVarCtx (outputLv, outputVar) reducedOutputVars,
            addVectorVarToBoundVarCtx (inputLv, inputVar) reducedInputVars,
            globalBoundVarCtx
          ]

  -- Create the object to store information about the application
  let appInfo =
        NetworkApplicationReplacement
          { networkApp = app,
            networkInfo = networkInfo,
            inputVariable = inputVar,
            outputVarExpr = outputVarExpr,
            outputVariable = outputVar
          }

  let newNetworkVariableReductions =
        HashMap.insert inputVar inputVarInfo $
          HashMap.insert
            outputVar
            outputVarInfo
            networkVariableReductions

  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = newGlobalBoundVarCtx,
            networkVariableReductions = newNetworkVariableReductions,
            networkApplications = LinkedHashMap.insert app appInfo networkApplications,
            ..
          }

  (inputVarExpr, outputVarExpr, newGlobalCtx)

--------------------------------------------------------------------------------
-- Partitions

type AssertionTree = BooleanExpr Assertion

-- | One step in the process for transforming unreduced user variables into
-- reduced network input and output variables.
data UserVariableReconstructionStep
  = SolveTensorEquality TensorVariable (LinearExpr TensorVariable RationalTensor)
  | SolveRationalEquality UserElementVariable (LinearExpr ElementVariable Rational)
  | SolveRationalInequalities UserElementVariable (Bounds ElementVariable Rational)
  | ReconstructTensor TensorVariable [ElementVariable]
  deriving (Eq, Ord, Show, Generic)

instance NFData UserVariableReconstructionStep

instance ToJSON UserVariableReconstructionStep

instance FromJSON UserVariableReconstructionStep

instance Pretty UserVariableReconstructionStep where
  pretty = \case
    SolveTensorEquality v s -> "Equation:" <+> pretty v <+> "=" <+> pretty s
    SolveRationalEquality v s -> "Equation:" <+> pretty v <+> "=" <+> pretty s
    SolveRationalInequalities v s -> "Inequalities:" <+> pretty v <+> "bounded" <+> pretty s
    ReconstructTensor v vs -> "Reconstruct:" <+> pretty v <+> "from" <+> prettyList vs

-- | The steps for transforming unreduced user variables into reduced network
-- input and output varibles.
-- These are used to recreate a satisfying assignment for the user variables
-- from the satisfying assignment for the network variables spat out by the
-- verifier.
--
-- The steps are stored in the same order they occured during compilation.
type UserVariableReconstruction = [UserVariableReconstructionStep]

type Partition = (UserVariableReconstruction, AssertionTree)

newtype Partitions = Partitions (Map UserVariableReconstruction AssertionTree)

partitionsToDisjuncts :: Partitions -> DisjunctAll Partition
partitionsToDisjuncts (Partitions ps) = DisjunctAll $ NonEmpty.fromList $ Map.toList ps

instance Pretty Partitions where
  pretty = pretty . partitionsToDisjuncts

andPartitions :: Partitions -> Partitions -> Partitions
andPartitions (Partitions xs) (Partitions ys) = do
  let xs' = Map.toList xs
  let ys' = Map.toList ys
  let combine (s1, t1) (s2, t2) = (s1 <> s2, andBoolExpr t1 t2)
  Partitions $ Map.fromList $ cartesianProduct combine xs' ys'

orPartitions :: Partitions -> Partitions -> Partitions
orPartitions (Partitions p1) (Partitions p2) =
  Partitions $ Map.unionWith orBoolExpr p1 p2

mkSinglePartition :: (UserVariableReconstruction, MaybeTrivial AssertionTree) -> MaybeTrivial Partitions
mkSinglePartition (solutions, maybeAssertion) =
  fmap (Partitions . Map.singleton solutions) maybeAssertion

mkTrivialPartition :: Assertion -> MaybeTrivial Partitions
mkTrivialPartition assertion = mkSinglePartition (mempty, NonTrivial $ Query assertion)

--------------------------------------------------------------------------------
-- Monads

type MonadPropertyStructure m =
  ( MonadFreeContext QueryBuiltin m,
    MonadReader PropertyMetaData m,
    MonadCompile m
  )

type MonadQueryStructure m =
  ( MonadPropertyStructure m,
    MonadState GlobalCtx m
  )

getGlobalBoundCtx :: (MonadQueryStructure m) => m (BoundCtx (Type QueryBuiltin))
getGlobalBoundCtx = gets (variableCtxToBoundCtx . (fmap snd . LinkedHashMap.toList . globalBoundVarCtx))

getGlobalNamedBoundCtx :: (MonadQueryStructure m) => m NamedBoundCtx
getGlobalNamedBoundCtx = toNamedBoundCtx <$> getGlobalBoundCtx

prettyFriendlyInCtx :: (MonadQueryStructure m) => WHNFValue QueryBuiltin -> m (Doc a)
prettyFriendlyInCtx e = do
  ctx <- toNamedBoundCtx <$> getGlobalBoundCtx
  return $ prettyFriendly (WithContext e ctx)

lookupVarByLevel :: (MonadState GlobalCtx m) => Lv -> m Variable
lookupVarByLevel lv = do
  GlobalCtx {..} <- get
  case LinkedHashMap.lookup lv globalBoundVarCtx of
    Nothing -> developerError "Cannot find variable var"
    Just v -> return v

getReducedVariableExprFor :: (MonadState GlobalCtx m) => Lv -> m (Maybe (WHNFValue QueryBuiltin))
getReducedVariableExprFor lv = do
  GlobalCtx {..} <- get
  var <- lookupVarByLevel lv
  case var of
    RationalVar {} -> return Nothing
    TensorVar v -> case HashMap.lookup v userVariableReductions of
      Just (_, expr) -> return $ Just expr
      Nothing -> case HashMap.lookup v networkVariableReductions of
        Just info -> return $ Just $ reducedNetworkVarExpr info
        Nothing -> developerError "Missing reductions for tensor variable"

getTensorVariable :: (MonadState GlobalCtx m) => Lv -> m TensorVariable
getTensorVariable lv = do
  var <- lookupVarByLevel lv
  case var of
    TensorVar v -> return v
    _ -> developerError "Expected tensor variable but found rational variable"

getRationalVariable :: (MonadState GlobalCtx m) => Lv -> m ElementVariable
getRationalVariable lv = do
  var <- lookupVarByLevel lv
  case var of
    RationalVar rv -> return rv
    TensorVar tv
      | not (null (tensorVarDimensions tv)) -> developerError $ "Expected rational variable but found tensor variable" <+> pretty var
      | otherwise -> do
          rvs <- getReducedVariablesFor tv
          case rvs of
            [rv] -> return rv
            _ -> developerError "Mismatched tensor dimensions!"

getReducedUserVariablesFor :: (MonadQueryStructure m) => TensorVariable -> m [UserElementVariable]
getReducedUserVariablesFor var = do
  GlobalCtx {..} <- get
  case HashMap.lookup var userVariableReductions of
    Just (vars, _) -> return vars
    Nothing ->
      compilerDeveloperError $
        "User variable" <+> pretty var <+> "has no reductions"

getReducedNetworkVariableInfo :: GlobalCtx -> TensorVariable -> NetworkVariableInfo
getReducedNetworkVariableInfo GlobalCtx {..} var = do
  case HashMap.lookup var networkVariableReductions of
    Just info -> info
    Nothing ->
      developerError $
        "Network variable" <+> pretty var <+> "has no reductions"

getReducedNetworkVariablesFor :: GlobalCtx -> TensorVariable -> [NetworkElementVariable]
getReducedNetworkVariablesFor globalCtx var =
  elementVariables (getReducedNetworkVariableInfo globalCtx var)

getReducedVariablesFor :: (MonadState GlobalCtx m) => TensorVariable -> m [ElementVariable]
getReducedVariablesFor var = do
  GlobalCtx {..} <- get
  case HashMap.lookup var userVariableReductions of
    Just (vars, _) -> return vars
    Nothing -> case HashMap.lookup var networkVariableReductions of
      Just r -> return (elementVariables r)
      Nothing -> developerError $ "Variable" <+> pretty var <+> "has no reductions"

reduceTensorExpr ::
  (MonadState GlobalCtx m) =>
  LinearExpr TensorVariable RationalTensor ->
  m [LinearExpr ElementVariable Rational]
reduceTensorExpr (Sparse coeff constant) = do
  let constValues = Vector.toList $ tensorValue constant
  let numRatEqs = product (tensorShape constant)
  coeffList <- traverse (\(v, c) -> (,c) <$> getReducedVariablesFor v) (Map.toList coeff)
  let asserts = fmap (mkRatEquality coeffList constValues) [0 .. numRatEqs - 1]
  return asserts
  where
    mkRatEquality ::
      [([ElementVariable], Coefficient)] ->
      [Rational] ->
      Int ->
      LinearExpr ElementVariable Rational
    mkRatEquality coeffs consts i = Sparse (Map.fromList (fmap (first (!! i)) coeffs)) (consts !! i)

--------------------------------------------------------------------------------
-- Context operations

variableCtxToBoundCtx :: (Pretty variable) => [variable] -> BoundCtx (Type builtin)
variableCtxToBoundCtx ctx = zipWith variableCtxToBoundCtxEntry [0 .. Ix (length ctx - 1)] ctx
  where
    variableCtxToBoundCtxEntry ix var = mkExplicitBinder (BoundVar mempty ix) (Just (layoutAsText $ pretty var))

--------------------------------------------------------------------------------
-- Vector operation patterns

mkVVectorEquality ::
  TensorShape ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin
mkVVectorEquality dimensions e1 e2 = do
  mkVectorEquality (fmap (INatLiteral mempty) dimensions) (Arg mempty Explicit Relevant <$> [e1, e2])
  where
    -- Would definitely be nicer to somehow reuse the type-class resolution machinery here,
    -- but it seems incredibly complicated to setup...
    mkVectorEquality :: [WHNFValue QueryBuiltin] -> WHNFSpine QueryBuiltin -> WHNFValue QueryBuiltin
    mkVectorEquality dims spine =
      let p = mempty
       in case dims of
            [] -> VBuiltinFunction (Equals EqRat Eq) spine
            d : ds -> VFreeVar (identifierOf StdEqualsVector) (nonExplicitArgs <> spine)
              where
                tensorType = foldr (\dim t -> IVectorType mempty t dim) (IRatType mempty) ds
                nonExplicitArgs =
                  [ Arg p (Implicit True) Relevant tensorType,
                    Arg p (Implicit True) Relevant tensorType,
                    Arg p (Implicit True) Irrelevant d,
                    Arg p (Instance True) Relevant (mkVectorEquality ds [])
                  ]

-- | The set of vector operations that we sometimes want to avoid normalising
-- out in the property for efficiency reasons.
vectorOperations :: Set StdLibFunction
vectorOperations =
  Set.fromList
    [ StdAddVector,
      StdSubVector,
      StdEqualsVector,
      StdNotEqualsVector
    ]
