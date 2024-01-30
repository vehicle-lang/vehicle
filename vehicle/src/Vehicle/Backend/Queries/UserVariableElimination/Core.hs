module Vehicle.Backend.Queries.UserVariableElimination.Core where

import Control.Monad.Reader (MonadReader (..), asks)
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
import Vehicle.Compile.Normalise.NBE (normaliseApp)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkType (..), dimensions)
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface.Expr
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.Hashing ()
import Vehicle.Data.LinearExpr
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Syntax.Builtin
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Variable

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
    inputVar :: OriginalNetworkVariable,
    inputVarExpr :: WHNFValue QueryBuiltin,
    inputRationalVars :: [NetworkRationalVariable],
    outputVar :: OriginalNetworkVariable,
    outputVarExpr :: WHNFValue QueryBuiltin,
    outputRationalVars :: [NetworkRationalVariable]
  }

--------------------------------------------------------------------------------
-- Reader state

data PropertyMetaData = PropertyMetaData
  { queryFormat :: QueryFormat,
    networkCtx :: NetworkContext,
    unalteredFreeContext :: FreeCtx QueryBuiltin,
    propertyProvenance :: DeclProvenance,
    propertyAddress :: PropertyAddress
  }

--------------------------------------------------------------------------------
-- Global state

data GlobalCtx = GlobalCtx
  { globalBoundVarCtx :: LinkedHashMap Lv Variable,
    userVariableReductions :: HashMap OriginalUserVariable ([UserRationalVariable], WHNFValue QueryBuiltin),
    networkVariableReductions :: HashMap OriginalNetworkVariable ([NetworkRationalVariable], WHNFValue QueryBuiltin),
    networkApplications :: LinkedHashMap NetworkApplication NetworkApplicationReplacement
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
          { networkName = networkName,
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
          { networkName = networkName,
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
            inputVar = inputVar,
            inputVarExpr = inputVarExpr,
            inputRationalVars = fmap snd reducedInputVars,
            outputVar = outputVar,
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

--------------------------------------------------------------------------------
-- Rational equalities

newtype RationalEquality = RationalEquality
  { rationalEqExpr :: LinearExpr RationalVariable Rational
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RationalEquality

instance FromJSON RationalEquality

instance Pretty RationalEquality where
  pretty e = pretty (rationalEqExpr e) <+> "== 0"

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkRationalEqualityTriviality :: RationalEquality -> Maybe Bool
checkRationalEqualityTriviality (RationalEquality e) = case isConstant e of
  Nothing -> Nothing
  Just c -> Just $ c == 0.0

--------------------------------------------------------------------------------
-- Rational inequalities

data RationalInequality = RationalInequality
  { strictness :: Strictness,
    rationalIneqExpr :: LinearExpr RationalVariable Rational
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RationalInequality

instance FromJSON RationalInequality

instance Pretty RationalInequality where
  pretty ineq =
    pretty (rationalIneqExpr ineq)
      <+> (if strictness ineq == Strict then "<" else "<=")
      <+> "0.0"

-- | Checks whether an assertion is trivial or not. Returns `Nothing` if
-- non-trivial, and otherwise `Just b` where `b` is the value of the assertion
-- if it is trivial.
checkRationalInequalityTriviality :: RationalInequality -> Maybe Bool
checkRationalInequalityTriviality (RationalInequality s e) = case isConstant e of
  Nothing -> Nothing
  Just c -> Just $ case s of
    Strict -> c < 0.0
    NonStrict -> c <= 0.0

--------------------------------------------------------------------------------
-- Tensor equalities

newtype TensorEquality = TensorEquality
  { tensorEqExpr :: LinearExpr TensorVariable RationalTensor
  }
  deriving (Show, Eq, Ord, Generic)

instance Pretty TensorEquality where
  pretty TensorEquality {..} = pretty tensorEqExpr <+> "== 0"

instance ToJSON TensorEquality

instance FromJSON TensorEquality

--------------------------------------------------------------------------------
-- Assertions

data Assertion
  = RationalEq RationalEquality
  | RationalIneq RationalInequality
  | TensorEq TensorEquality
  deriving (Show, Eq, Generic)

instance ToJSON Assertion

instance FromJSON Assertion

instance Pretty Assertion where
  pretty = \case
    RationalEq eq -> pretty eq
    RationalIneq ineq -> pretty ineq
    TensorEq eq -> pretty eq

checkTriviality :: Assertion -> MaybeTrivial Assertion
checkTriviality ass = case ass of
  RationalEq RationalEquality {..} -> case isConstant rationalEqExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial (d == 0)
  RationalIneq RationalInequality {..} -> case isConstant rationalIneqExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial ((if strictness == Strict then (<) else (<=)) d 0)
  TensorEq TensorEquality {..} -> case isConstant tensorEqExpr of
    Nothing -> NonTrivial ass
    Just d -> Trivial (isZero d)

prettyAssertions :: [Assertion] -> Doc a
prettyAssertions assertions =
  vsep (fmap pretty assertions)

prettyInequalities :: [RationalInequality] -> Doc a
prettyInequalities assertions =
  vsep (fmap pretty assertions)

data Relation
  = Equal
  | LessThan
  | LessThanOrEqual
  deriving (Eq, Ord)

assertionRel :: Assertion -> Relation
assertionRel = \case
  RationalEq {} -> Equal
  TensorEq {} -> Equal
  RationalIneq ineq
    | strictness ineq == Strict -> LessThan
    | otherwise -> LessThanOrEqual

eqToAssertion ::
  LinearExpr RationalVariable Rational ->
  LinearExpr RationalVariable Rational ->
  Assertion
eqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  RationalEq $ RationalEquality e

ordToAssertion ::
  OrderOp ->
  LinearExpr RationalVariable Rational ->
  LinearExpr RationalVariable Rational ->
  Assertion
ordToAssertion op e1 e2 =
  RationalIneq $ case op of
    Lt -> RationalInequality Strict (addExprs 1 (-1) e1 e2)
    Le -> RationalInequality NonStrict (addExprs 1 (-1) e1 e2)
    Gt -> RationalInequality Strict (addExprs (-1) 1 e1 e2)
    Ge -> RationalInequality NonStrict (addExprs (-1) 1 e1 e2)

tensorEqToAssertion ::
  LinearExpr TensorVariable RationalTensor ->
  LinearExpr TensorVariable RationalTensor ->
  Assertion
tensorEqToAssertion e1 e2 = do
  let e = addExprs 1 (-1) e1 e2
  TensorEq $ TensorEquality e

mapAssertionExprs ::
  (LinearExpr TensorVariable RationalTensor -> LinearExpr TensorVariable RationalTensor) ->
  (LinearExpr RationalVariable Rational -> LinearExpr RationalVariable Rational) ->
  Assertion ->
  MaybeTrivial Assertion
mapAssertionExprs ft fr ass = checkTriviality $ case ass of
  TensorEq TensorEquality {..} -> TensorEq $ TensorEquality $ ft tensorEqExpr
  RationalEq RationalEquality {..} -> RationalEq $ RationalEquality $ fr rationalEqExpr
  RationalIneq RationalInequality {..} -> RationalIneq $ RationalInequality strictness (fr rationalIneqExpr)

substituteTensorEq ::
  (OriginalUserVariable, TensorEquality) ->
  Map RationalVariable RationalEquality ->
  Assertion ->
  MaybeTrivial Assertion
substituteTensorEq (var, solution) ratSolutions =
  mapAssertionExprs
    (eliminateVar (UserTensorVar var) (tensorEqExpr solution))
    eliminateRatVars
  where
    -- Usually the expression being substituted into is much smaller than the number of tensor
    -- variables so we traverse the expression instead of folding over the subsitutions
    eliminateRatVars :: LinearExpr RationalVariable Rational -> LinearExpr RationalVariable Rational
    eliminateRatVars expr = do
      let varExprs = lookupVar <$> Map.toList (coefficients expr)
      let constantExp = Sparse (mempty @(Map RationalVariable Coefficient)) (constantValue expr)
      foldr (addExprs 1 1) constantExp varExprs

    lookupVar :: (RationalVariable, Coefficient) -> LinearExpr RationalVariable Rational
    lookupVar (v, c) = do
      let vc = Sparse (Map.singleton v c) 0
      case Map.lookup v ratSolutions of
        Nothing -> vc
        Just sol -> eliminateVar v (rationalEqExpr sol) vc

substituteRationalEq :: UserRationalVariable -> RationalEquality -> Assertion -> MaybeTrivial Assertion
substituteRationalEq var solution =
  mapAssertionExprs id (eliminateVar (UserRationalVar var) (rationalEqExpr solution))

--------------------------------------------------------------------------------
-- Partitions

type AssertionTree = BooleanExpr Assertion

-- | One step in the process for transforming unreduced user variables into
-- reduced network input and output variables.
data UserVariableReconstructionStep
  = SolveTensorEquality OriginalUserVariable TensorEquality
  | SolveRationalEquality UserRationalVariable RationalEquality
  | SolveRationalInequalities UserRationalVariable FourierMotzkinVariableSolution
  | ReconstructTensor OriginalUserVariable [UserRationalVariable]
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserVariableReconstructionStep

instance FromJSON UserVariableReconstructionStep

instance Pretty UserVariableReconstructionStep where
  pretty = \case
    SolveTensorEquality v _s -> "SolveTensorEquality[" <+> pretty v <+> "]" -- "=" <+> pretty s <+> "]"
    SolveRationalEquality v _s -> "SolveRationalEquality[" <+> pretty v <+> "]" -- "=" <+> pretty s <+> "]"
    SolveRationalInequalities v _ -> "SolveRationalInequalities[" <+> pretty v <+> "]"
    ReconstructTensor v _vs -> "ReconstructTensor[" <+> pretty v <+> "]"

-- | The steps for transforming unreduced user variables into reduced network
-- input and output varibles.
-- These are used to recreate a satisfying assignment for the user variables
-- from the satisfying assignment for the network variables spat out by the
-- verifier.
--
-- The steps are stored in the same order they occured during compilation.
type UserVariableReconstruction = [UserVariableReconstructionStep]

type UserVarSolutions = UserVariableReconstruction

addRationalEqualitySolution ::
  UserRationalVariable ->
  RationalEquality ->
  UserVarSolutions ->
  UserVarSolutions
addRationalEqualitySolution var eq solutions =
  SolveRationalEquality var eq : solutions

addRationalInequalitySolution ::
  UserRationalVariable ->
  FourierMotzkinVariableSolution ->
  UserVarSolutions ->
  UserVarSolutions
addRationalInequalitySolution var eq solutions =
  SolveRationalInequalities var eq : solutions

addTensorEqualitySolution ::
  OriginalUserVariable ->
  TensorEquality ->
  UserVarSolutions ->
  UserVarSolutions
addTensorEqualitySolution var eq solutions =
  SolveTensorEquality var eq : solutions

unionSolutions :: UserVarSolutions -> UserVarSolutions -> UserVarSolutions
unionSolutions = (<>)

type Partition = (UserVarSolutions, AssertionTree)

newtype Partitions = Partitions (Map UserVarSolutions AssertionTree)

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

mkSinglePartition :: (UserVarSolutions, MaybeTrivial AssertionTree) -> MaybeTrivial Partitions
mkSinglePartition (solutions, maybeAssertion) =
  fmap (Partitions . Map.singleton solutions) maybeAssertion

mkTrivialPartition :: Assertion -> MaybeTrivial Partitions
mkTrivialPartition assertion = mkSinglePartition (mempty, NonTrivial $ Query assertion)

--------------------------------------------------------------------------------
-- Variable reconstruction

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data FourierMotzkinVariableSolution = FMSolution
  { lowerBounds :: [RationalInequality],
    upperBounds :: [RationalInequality]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON FourierMotzkinVariableSolution

instance FromJSON FourierMotzkinVariableSolution

--------------------------------------------------------------------------------
-- Monads

type MonadPropertyStructure m =
  ( MonadFreeContext QueryBuiltin m,
    MonadReader PropertyMetaData m
  )

type MonadQueryStructure m =
  ( MonadPropertyStructure m,
    MonadState GlobalCtx m
  )

getGlobalBoundCtx :: (MonadQueryStructure m) => m (BoundCtx QueryBuiltin)
getGlobalBoundCtx = gets (variableCtxToBoundCtx . (fmap snd . LinkedHashMap.toList . globalBoundVarCtx))

lookupVarByLevel :: (MonadQueryStructure m) => Lv -> m Variable
lookupVarByLevel lv = do
  GlobalCtx {..} <- get
  case LinkedHashMap.lookup lv globalBoundVarCtx of
    Nothing -> compilerDeveloperError "Cannout find variable var"
    Just v -> return v

getReducedVariableExprFor :: (MonadQueryStructure m) => Lv -> m (Maybe (WHNFValue QueryBuiltin))
getReducedVariableExprFor lv = do
  GlobalCtx {..} <- get
  var <- lookupVarByLevel lv
  case var of
    TensorVar (UserTensorVar v) -> return (snd <$> HashMap.lookup v userVariableReductions)
    TensorVar (NetworkTensorVar v) -> return (snd <$> HashMap.lookup v networkVariableReductions)
    _ -> return Nothing

getTensorVariable :: (MonadQueryStructure m) => Lv -> m TensorVariable
getTensorVariable lv = do
  var <- lookupVarByLevel lv
  case var of
    TensorVar v -> return v
    _ -> compilerDeveloperError "Expected tensor variable but found rational variable"

getRationalVariable :: (MonadQueryStructure m) => Lv -> m RationalVariable
getRationalVariable lv = do
  var <- lookupVarByLevel lv
  case var of
    RationalVar v -> return v
    _ -> compilerDeveloperError "Expected rational variable but found tensor variable"

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

getReducedVariablesFor :: (MonadCompile m) => GlobalCtx -> TensorVariable -> m [RationalVariable]
getReducedVariablesFor GlobalCtx {..} var = do
  case var of
    NetworkTensorVar v -> case HashMap.lookup v networkVariableReductions of
      Just (vars, _) -> return $ fmap NetworkRationalVar vars
      Nothing -> compilerDeveloperError $ "Network variable" <+> pretty var <+> "has no reductions"
    UserTensorVar v -> case HashMap.lookup v userVariableReductions of
      Just (vars, _) -> return $ fmap UserRationalVar vars
      Nothing -> compilerDeveloperError $ "User variable" <+> pretty var <+> "has no reductions"

appStdlibDef :: (MonadPropertyStructure m) => StdLibFunction -> WHNFSpine QueryBuiltin -> m (WHNFValue QueryBuiltin)
appStdlibDef fn spine = do
  freeCtx <- asks unalteredFreeContext
  (fnDef, _) <- lookupInFreeCtx "lookupStdlibDef" (identifierOf fn) freeCtx
  case bodyOf fnDef of
    Just fnBody -> normaliseApp fnBody spine
    Nothing -> compilerDeveloperError $ "Unexpected found" <+> quotePretty fn <+> "to have no body"

reduceTensorEquality ::
  (MonadCompile m) =>
  GlobalCtx ->
  TensorEquality ->
  m [RationalEquality]
reduceTensorEquality globalCtx (TensorEquality (Sparse coeff constant)) = do
  let constValues = Vector.toList $ tensorValues constant
  let numRatEqs = product (tensorDims constant)
  coeffList <- traverse (\(v, c) -> (,c) <$> getReducedVariablesFor globalCtx v) (Map.toList coeff)
  let asserts = fmap (mkRatEquality coeffList constValues) [0 .. numRatEqs - 1]
  return asserts
  where
    mkRatEquality ::
      [([RationalVariable], Coefficient)] ->
      [Rational] ->
      Int ->
      RationalEquality
    mkRatEquality coeffs consts i = do
      let expr = Sparse (Map.fromList (fmap (first (!! i)) coeffs)) (consts !! i)
      RationalEquality expr

--------------------------------------------------------------------------------
-- Context operations

variableCtxToBoundCtx :: (Pretty variable) => [variable] -> BoundCtx builtin
variableCtxToBoundCtx ctx = zipWith variableCtxToBoundCtxEntry [0 .. Ix (length ctx - 1)] ctx
  where
    variableCtxToBoundCtxEntry ix var = mkDefaultBinder (layoutAsText $ pretty var) (BoundVar mempty ix)

--------------------------------------------------------------------------------
-- Vector operation patterns

pattern VInfiniteQuantifier ::
  Quantifier ->
  [WHNFArg QueryBuiltin] ->
  WHNFBinder QueryBuiltin ->
  WHNFBoundEnv QueryBuiltin ->
  Expr Ix QueryBuiltin ->
  WHNFValue QueryBuiltin
pattern VInfiniteQuantifier q args binder env body <-
  VBuiltinFunction (Quantifier q) (reverse -> RelevantExplicitArg _ (VLam binder (WHNFBody env body)) : args)
  where
    VInfiniteQuantifier q args binder env body =
      VBuiltinFunction (Quantifier q) (reverse (Arg mempty Explicit Relevant (VLam binder (WHNFBody env body)) : args))

pattern VForall ::
  [WHNFArg QueryBuiltin] ->
  WHNFBinder QueryBuiltin ->
  WHNFBoundEnv QueryBuiltin ->
  Expr Ix QueryBuiltin ->
  WHNFValue QueryBuiltin
pattern VForall args binder env body = VInfiniteQuantifier Forall args binder env body

pattern VExists ::
  [WHNFArg QueryBuiltin] ->
  WHNFBinder QueryBuiltin ->
  WHNFBoundEnv QueryBuiltin ->
  Expr Ix QueryBuiltin ->
  WHNFValue QueryBuiltin
pattern VExists args binder env body = VInfiniteQuantifier Exists args binder env body

pattern VVecEqSpine ::
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFSpine QueryBuiltin
pattern VVecEqSpine t1 t2 dim sol x y <- [t1, t2, dim, argExpr -> sol, argExpr -> x, argExpr -> y]
  where
    VVecEqSpine t1 t2 dim sol x y = [t1, t2, dim, Arg mempty (Instance True) Relevant sol, Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern VVecOp2Spine ::
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFSpine QueryBuiltin
pattern VVecOp2Spine t1 t2 t3 dim sol x y <- [t1, t2, t3, dim, argExpr -> sol, argExpr -> x, argExpr -> y]

pattern VVecEqArgs ::
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFSpine QueryBuiltin
pattern VVecEqArgs x y <- VVecEqSpine _ _ _ _ x y

pattern VVectorEqualFull :: WHNFSpine QueryBuiltin -> WHNFValue QueryBuiltin
pattern VVectorEqualFull spine = VStandardLib StdEqualsVector spine

pattern VVectorNotEqualFull :: WHNFSpine QueryBuiltin -> WHNFValue QueryBuiltin
pattern VVectorNotEqualFull spine = VStandardLib StdNotEqualsVector spine

pattern VVectorEqual :: WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin
pattern VVectorEqual x y <- VVectorEqualFull (VVecEqArgs x y)

pattern VVectorNotEqual :: WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin
pattern VVectorNotEqual x y <- VVectorNotEqualFull (VVecEqArgs x y)

pattern VVectorAdd :: WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin
pattern VVectorAdd x y <- VStandardLib StdAddVector [_, _, _, _, _, argExpr -> x, argExpr -> y]

pattern VVectorSub :: WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin
pattern VVectorSub x y <- VStandardLib StdSubVector [_, _, _, _, _, argExpr -> x, argExpr -> y]

pattern VAt :: WHNFArg QueryBuiltin -> WHNFArg QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin
pattern VAt t n x y <- VBuiltinFunction At [t, n, argExpr -> x, argExpr -> y]
  where
    VAt t n x y = VBuiltinFunction At [t, n, Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

mkVVectorEquality ::
  TensorDimensions ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin
mkVVectorEquality dimensions e1 e2 = do
  mkVectorEquality (fmap VNatLiteral dimensions) (Arg mempty Explicit Relevant <$> [e1, e2])
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
                tensorType = foldr (\dim t -> mkVVectorType t dim) VRatType ds
                nonExplicitArgs =
                  [ Arg p (Implicit True) Relevant tensorType,
                    Arg p (Implicit True) Relevant tensorType,
                    Arg p (Implicit True) Irrelevant d,
                    Arg p (Instance True) Relevant (mkVectorEquality ds [])
                  ]

negExpr :: Expr Ix QueryBuiltin -> Expr Ix QueryBuiltin
negExpr body = NotExpr mempty [Arg mempty Explicit Relevant body]

-- | The set of vector operations that we sometimes want to avoid normalising
-- out in the property for efficiency reasons.
vectorOperations :: Set Identifier
vectorOperations =
  Set.fromList $
    identifierOf
      <$> [ StdAddVector,
            StdSubVector,
            StdEqualsVector,
            StdNotEqualsVector
          ]
