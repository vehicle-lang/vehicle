module Vehicle.Backend.Queries.UserVariableElimination.Core where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Char.SScript (subscript)
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
import Prettyprinter (brackets)
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
    inputVarExpr :: WHNFValue QueryBuiltin,
    inputRationalVars :: [NetworkRationalVariable],
    outputVarExpr :: WHNFValue QueryBuiltin,
    outputRationalVars :: [NetworkRationalVariable]
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
    userVariableReductions :: !(HashMap OriginalUserVariable ([UserRationalVariable], WHNFValue QueryBuiltin)),
    networkVariableReductions :: !(HashMap OriginalNetworkVariable ([NetworkRationalVariable], WHNFValue QueryBuiltin)),
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
  (variable -> TensorVariable) ->
  (ReducedVariable variable -> RationalVariable) ->
  (Lv, variable) ->
  [(Lv, ReducedVariable variable)] ->
  LinkedHashMap Lv Variable
addVectorVarToBoundVarCtx fromVar1 fromVar2 tensorVar elemVars = do
  let tensorVar' = second (TensorVar . fromVar1) tensorVar
  let elemVars' = fmap (second (RationalVar . fromVar2)) elemVars
  LinkedHashMap.union (LinkedHashMap.fromList elemVars') (LinkedHashMap.fromList [tensorVar'])

addUserVarToGlobalContext ::
  OriginalUserVariable ->
  GlobalCtx ->
  (WHNFValue QueryBuiltin, GlobalCtx)
addUserVarToGlobalContext userVar GlobalCtx {..} = do
  -- Create the unreduced and reduced versions of the user variables.
  let currentLevel = Lv $ length globalBoundVarCtx
  let envEntry@(reducedUseVars, _) = reduceVariable userTensorVarDimensions (currentLevel + 1) userVar
  let userVarExpr = VBoundVar currentLevel []
  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = LinkedHashMap.union (addVectorVarToBoundVarCtx UserTensorVar UserRationalVar (currentLevel, userVar) reducedUseVars) globalBoundVarCtx,
            userVariableReductions = HashMap.insert userVar (first (fmap snd) envEntry) userVariableReductions,
            ..
          }
  (userVarExpr, newGlobalCtx)

addNetworkApplicationToGlobalCtx ::
  NetworkApplication ->
  NetworkContextInfo ->
  GlobalCtx ->
  (NetworkApplicationReplacement, GlobalCtx)
addNetworkApplicationToGlobalCtx app@(networkName, _) networkInfo GlobalCtx {..} = do
  let metaNetworkSoFar = LinkedHashMap.toList networkApplications
  let applicationNumber = length $ filter (\((name, _), _) -> name == networkName) metaNetworkSoFar

  -- Create a single variable for the input of the network to
  -- (avoiding prematurely normalising so that we can potentially solve
  -- user tensor variables in terms of it).
  let inputLv = Lv $ length globalBoundVarCtx
  let inputVar =
        OriginalNetworkVariable
          { networkVarName = createNetworkVarName networkName applicationNumber Input,
            application = applicationNumber,
            networkTensorVarDimensions = dimensions (inputTensor (networkType networkInfo)),
            inputOrOutput = Input,
            startingIndex = sum (fmap (length . inputRationalVars . snd) metaNetworkSoFar)
          }
  let (reducedInputVars, reducedInputVarsExpr) = reduceVariable networkTensorVarDimensions (inputLv + 1) inputVar
  let inputVarExpr = VBoundVar inputLv []

  -- Create a tensor of variables for the output of the network.
  let outputLv = inputLv + 1 + Lv (length reducedInputVars)
  let outputVar =
        OriginalNetworkVariable
          { networkVarName = createNetworkVarName networkName applicationNumber Output,
            application = applicationNumber,
            networkTensorVarDimensions = dimensions (outputTensor (networkType networkInfo)),
            inputOrOutput = Output,
            startingIndex = sum (fmap (length . outputRationalVars . snd) metaNetworkSoFar)
          }
  let (reducedOutputVars, reducedOutputVarsExpr) = reduceVariable networkTensorVarDimensions (outputLv + 1) outputVar
  let outputVarExpr = VBoundVar outputLv []

  -- Create the context extension of the bound context.
  let newGlobalBoundVarCtx =
        LinkedHashMap.unions
          [ addVectorVarToBoundVarCtx NetworkTensorVar NetworkRationalVar (outputLv, outputVar) reducedOutputVars,
            addVectorVarToBoundVarCtx NetworkTensorVar NetworkRationalVar (inputLv, inputVar) reducedInputVars,
            globalBoundVarCtx
          ]

  -- Create the object to store information about the application
  let appInfo =
        NetworkApplicationReplacement
          { networkApp = app,
            networkInfo = networkInfo,
            inputVarExpr = inputVarExpr,
            inputRationalVars = fmap snd reducedInputVars,
            outputVarExpr = outputVarExpr,
            outputRationalVars = fmap snd reducedOutputVars
          }

  let newNetworkVariableReductions =
        HashMap.insert inputVar (fmap snd reducedInputVars, reducedInputVarsExpr) $
          HashMap.insert
            outputVar
            (fmap snd reducedOutputVars, reducedOutputVarsExpr)
            networkVariableReductions

  let newGlobalCtx =
        GlobalCtx
          { globalBoundVarCtx = newGlobalBoundVarCtx,
            networkVariableReductions = newNetworkVariableReductions,
            networkApplications = LinkedHashMap.insert app appInfo networkApplications,
            ..
          }

  (appInfo, newGlobalCtx)

createNetworkVarName :: Name -> Int -> InputOrOutput -> Name
createNetworkVarName networkName application inputOrOutput =
  layoutAsText $
    pretty networkName
      <> pretty (fmap subscript (show application))
      <> brackets (pretty inputOrOutput)

--------------------------------------------------------------------------------
-- Partitions

type AssertionTree = BooleanExpr Assertion

-- | One step in the process for transforming unreduced user variables into
-- reduced network input and output variables.
data UserVariableReconstructionStep
  = SolveTensorEquality OriginalUserVariable (LinearExpr TensorVariable RationalTensor)
  | SolveRationalEquality UserRationalVariable (LinearExpr RationalVariable Rational)
  | SolveRationalInequalities UserRationalVariable (Bounds RationalVariable Rational)
  | ReconstructTensor TensorVariable [RationalVariable]
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
    TensorVar (UserTensorVar v) -> return (snd <$> HashMap.lookup v userVariableReductions)
    TensorVar (NetworkTensorVar v) -> return (snd <$> HashMap.lookup v networkVariableReductions)
    _ -> return Nothing

getTensorVariable :: (MonadState GlobalCtx m) => Lv -> m TensorVariable
getTensorVariable lv = do
  var <- lookupVarByLevel lv
  case var of
    TensorVar v -> return v
    _ -> developerError "Expected tensor variable but found rational variable"

getRationalVariable :: (MonadState GlobalCtx m) => Lv -> m RationalVariable
getRationalVariable lv = do
  var <- lookupVarByLevel lv
  case var of
    RationalVar rv -> return rv
    TensorVar tv
      | not (null (tensorVariableDims tv)) -> developerError $ "Expected rational variable but found tensor variable" <+> pretty var
      | otherwise -> do
          rvs <- getReducedVariablesFor tv
          case rvs of
            [rv] -> return rv
            _ -> developerError "Mismatched tensor dimensions!"

getReducedUserVariablesFor :: (MonadQueryStructure m) => OriginalUserVariable -> m [UserRationalVariable]
getReducedUserVariablesFor var = do
  GlobalCtx {..} <- get
  case HashMap.lookup var userVariableReductions of
    Just (vars, _) -> return vars
    Nothing ->
      compilerDeveloperError $
        "User variable" <+> pretty var <+> "has no reductions"

getReducedNetworkVariablesFor :: (MonadCompile m) => GlobalCtx -> OriginalNetworkVariable -> m [NetworkRationalVariable]
getReducedNetworkVariablesFor GlobalCtx {..} var = do
  case HashMap.lookup var networkVariableReductions of
    Just (vars, _) -> return vars
    Nothing ->
      compilerDeveloperError $
        "Network variable" <+> pretty var <+> "has no reductions"

getReducedVariablesFor :: (MonadState GlobalCtx m) => TensorVariable -> m [RationalVariable]
getReducedVariablesFor var = do
  GlobalCtx {..} <- get
  case var of
    NetworkTensorVar v -> case HashMap.lookup v networkVariableReductions of
      Just (vars, _) -> return $ fmap NetworkRationalVar vars
      Nothing -> developerError $ "Network variable" <+> pretty var <+> "has no reductions"
    UserTensorVar v -> case HashMap.lookup v userVariableReductions of
      Just (vars, _) -> return $ fmap UserRationalVar vars
      Nothing -> developerError $ "User variable" <+> pretty var <+> "has no reductions"

reduceTensorExpr ::
  (MonadQueryStructure m) =>
  LinearExpr TensorVariable RationalTensor ->
  m [LinearExpr RationalVariable Rational]
reduceTensorExpr (Sparse coeff constant) = do
  let constValues = Vector.toList $ tensorValue constant
  let numRatEqs = product (tensorShape constant)
  coeffList <- traverse (\(v, c) -> (,c) <$> getReducedVariablesFor v) (Map.toList coeff)
  let asserts = fmap (mkRatEquality coeffList constValues) [0 .. numRatEqs - 1]
  return asserts
  where
    mkRatEquality ::
      [([RationalVariable], Coefficient)] ->
      [Rational] ->
      Int ->
      LinearExpr RationalVariable Rational
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
