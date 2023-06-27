{-# LANGUAGE DeriveAnyClass #-}

module Vehicle.Compile.Queries.NetworkElimination
  ( MetaNetworkPartition (..),
    replaceNetworkApplications,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), evalStateT, gets, modify)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldlM)
import Data.Functor.Classes (Ord1 (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet (singleton, toList, union, unions)
import Data.Hashable (Hashable (..))
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Queries.LinearExpr (VectorEquality (..))
import Vehicle.Compile.Queries.QuerySetStructure (UnreducedAssertion (..))
import Vehicle.Compile.Queries.Variable (MixedVariable (..), MixedVariables (..), NetworkVariable (..), UserVariable (..), mixedVariableDBCtx, pattern VFiniteQuantifier)
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Boolean (BooleanExpr (..), DisjunctAll (..))
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Hashing ()
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (StdLibFunction (..))
import Vehicle.Verify.Core

-- | Okay so this is a wild ride. The Marabou query format has special variable
-- names for input and output variables, namely x1 ... xN and y1 ... yM but
-- otherwise has the standard SMTLib syntax. We refer to these variables as
-- "network IO variables".
--
-- This means that in theory you can only reason about a single network applied
-- to a single input per property. We get around this restriction by combining
-- multiple networks, or multiple applications of the same network into a
-- single "meta" network.
--
-- This function converts all user quantified variables to network I/O
-- variables, converting to DNF as minimally required so that there's a
-- consistent set of network applications within a single disjunct and returns:
--   - the ordered list of network applications
--   - the network variables
--   - the expressions that contain those network applications
--   - the final expression.
replaceNetworkApplications ::
  (MonadCompile m) =>
  DeclProvenance ->
  NetworkContext ->
  BoundCtx UserVariable ->
  BooleanExpr UnreducedAssertion ->
  m (DisjunctAll MetaNetworkPartition)
replaceNetworkApplications declProv networkCtx userVariables boolExpr = do
  logCompilerPass MidDetail "network input/output variable insertion" $ do
    -- let conjunctsDoc = prettyFriendly (WithContext boolExpr revUserVarCtx)
    -- logDebug MaxDetail $ "Initial conjunctions:" <> line <> indent 2 conjunctsDoc <> line

    let ctx = NetworkReaderCtx networkCtx userVariables
    flip runReaderT ctx $ do
      partitions <- partitionApplications boolExpr
      let numberOfPartitions = pretty (length partitions)
      logDebug MinDetail $ line <> "Found" <+> numberOfPartitions <+> "meta-network partition(s)" <> line
      case zip [1 ..] (HashMap.toList partitions) of
        [] -> throwError $ NoNetworkUsedInProperty declProv
        x : xs -> do
          substPartitions <- for (x :| xs) replaceApplications
          return $ DisjunctAll substPartitions

--------------------------------------------------------------------------------
-- Generic traversal of network applications

type MonadTraverseApplications m =
  ( MonadCompile m,
    MonadReader NetworkReaderCtx m
  )

data NetworkReaderCtx = NetworkReaderCtx
  { networkContext :: NetworkContext,
    userVariables :: BoundCtx UserVariable
  }

--------------------------------------------------------------------------------
-- Partioning

type NetworkApplication = (Name, StandardSpine)

applicationExpr :: NetworkApplication -> StandardNormExpr
applicationExpr (networkName, spine) = VFreeVar (Identifier User networkName) spine

-- | A tree of network applications that are contained within one another. e.g.
-- if `f`, `g` and `h` are networks then `f (g x + h y)` would be
-- `Node (f (g x + h y)) {Node (g x) {}, Node (h y) {}}`. The reason we use sets
-- at the nodes is that we want them to be invariant to the order of their
-- branches.
data NetworkApplicationTree = Node
  { nodeApp :: NetworkApplication,
    nodeSubApps :: NetworkApplicationForest
  }
  deriving (Eq, Generic, Hashable)

type NetworkApplicationForest = HashSet NetworkApplicationTree

-- | Compares two application trees. This could be done via implementing Ord
-- but it gets messy fast. In particular `Value` refers back to normal expressions
-- via lambda expressions and then you have to push it through *everywhere*
-- even though lambdas will never appear. Better to just implement it locally
-- here.
compareApplicationTree :: NetworkApplicationTree -> NetworkApplicationTree -> Ordering
compareApplicationTree a b = do
  let (f, x) = nodeApp a
  let (g, y) = nodeApp b
  compare f g <> compareSpine x y
  where
    compareSpine :: StandardSpine -> StandardSpine -> Ordering
    compareSpine x y = compareExplicitSpine (mapMaybe getExplicitArg x) (mapMaybe getExplicitArg y)

    compareExplicitSpine :: StandardExplicitSpine -> StandardExplicitSpine -> Ordering
    compareExplicitSpine = liftCompare compareValue

    compareValue :: StandardNormExpr -> StandardNormExpr -> Ordering
    compareValue x y = case (x, y) of
      (VBoundVar v1 spine1, VBoundVar v2 spine2) -> compare v1 v2 <> compareSpine spine1 spine2
      (VBoundVar {}, _) -> LT
      (_, VBoundVar {}) -> GT
      (VBuiltin b1 spine1, VBuiltin b2 spine2) -> compare b1 b2 <> compareExplicitSpine spine1 spine2
      (VBuiltin {}, _) -> LT
      (_, VBuiltin {}) -> GT
      (VFreeVar i1 spine1, VFreeVar i2 spine2) -> compare i1 i2 <> compareSpine spine1 spine2
      _ -> EQ

-- | Locate network applications and lift disjunctions as required to form
-- consistent partitions.
--
-- INVARIANT: All (NetworkApplicationForest, BooleanExpr) pairs within
-- within the map should have the property that the `booleanExpr`
-- is consistent with respect to the NetworkApplicationForest.
partitionApplications ::
  forall m.
  (MonadTraverseApplications m) =>
  BooleanExpr UnreducedAssertion ->
  m (HashMap NetworkApplicationForest (BooleanExpr UnreducedAssertion))
partitionApplications = go
  where
    go ::
      BooleanExpr UnreducedAssertion ->
      m (HashMap NetworkApplicationForest (BooleanExpr UnreducedAssertion))
    go = \case
      Query assertion -> do
        forest <- findApplicationsInAssertion assertion
        return $ HashMap.singleton forest (Query assertion)
      Conjunct e1 e2 -> do
        -- Cartesian product r1 and r2, unioning the two states, then we need find duplicate
        -- states and then internally conjunct their corresponding boolean expressions.
        r1 <- HashMap.toList <$> go e1
        r2 <- HashMap.toList <$> go e2
        let xs = [(HashSet.union apps1 app2, Conjunct expr1 expr2) | (apps1, expr1) <- r1, (app2, expr2) <- r2]
        return $ HashMap.fromListWith Conjunct xs
      Disjunct e1 e2 -> do
        -- Union r1 and r2 and find duplicates states and then internally
        -- disjunct their corresponding boolean expressions.
        r1 <- go e1
        r2 <- go e2
        return $ HashMap.unionWith Disjunct r1 r2

findApplicationsInAssertion ::
  (MonadTraverseApplications m) =>
  UnreducedAssertion ->
  m NetworkApplicationForest
findApplicationsInAssertion = \case
  VectorEqualityAssertion VectorEquality {..} -> do
    lhs <- findApplicationsInExpr assertionLHS
    rhs <- findApplicationsInExpr assertionRHS
    return $ HashSet.union lhs rhs
  NonVectorEqualityAssertion expr -> do
    findApplicationsInExpr expr

findApplicationsInExpr ::
  (MonadTraverseApplications m) =>
  StandardNormExpr ->
  m NetworkApplicationForest
findApplicationsInExpr expr = case expr of
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VMeta {} -> normalisationError currentPass "Lam"
  VLam {} -> normalisationError currentPass "Lam"
  VBoundVar _v spine -> findApplicationsInSpine (fmap argExpr spine)
  VBuiltin _b spine -> findApplicationsInSpine spine
  -- By construction, finite quantifiers are not left in if they contain
  -- references to a neural network.
  VFiniteQuantifier {} -> return mempty
  VFreeVar ident spine -> do
    spineForest <- findApplicationsInSpine (fmap argExpr spine)
    NetworkReaderCtx {..} <- ask
    if nameOf ident `Map.notMember` networkContext
      then return spineForest
      else return $ HashSet.singleton (Node (nameOf ident, spine) spineForest)

findApplicationsInSpine ::
  (MonadTraverseApplications m) =>
  StandardExplicitSpine ->
  m NetworkApplicationForest
findApplicationsInSpine spine =
  HashSet.unions <$> traverse findApplicationsInExpr spine

--------------------------------------------------------------------------------
-- Replace network applications.

data MetaNetworkPartition = MetaNetworkPartition
  { metaNetwork :: MetaNetwork,
    networkVars :: BoundCtx NetworkVariable,
    networkNormSteps :: VariableNormalisationSteps,
    partitionExpr :: BooleanExpr UnreducedAssertion
  }

type MetaNetworkVariableSubstitution = HashMap NetworkApplication StandardNormExpr

replaceApplications ::
  (MonadTraverseApplications m) =>
  (Int, (NetworkApplicationForest, BooleanExpr UnreducedAssertion)) ->
  m MetaNetworkPartition
replaceApplications (partitionID, (applications, expr)) = do
  let sectionDoc = "variable substitution for meta-network partition" <+> pretty partitionID
  logCompilerPass MaxDetail sectionDoc $ do
    appInfo <- flip evalStateT mempty $ lineariseNetworkApplicationForest mempty applications
    NetworkReaderCtx {..} <- ask

    -- Calculate the meta network and the network norm steps
    let metaNetwork = fmap (\(_, (_, _, _, metaNetworkEntry)) -> metaNetworkEntry) appInfo
    let networkVariables = getNetworkContext appInfo
    let networkNormSteps = fmap (Introduce . NetworkVar) networkVariables

    -- Substitute the applications in the original expression for the output variables
    let outputVariableMap = getOutputVarSubsitution appInfo
    networkFreeExpr <- traverse (replaceApplicationsInAssertion outputVariableMap) expr

    -- Add the set of input equalities to the boolean expression
    let inputEqualities = fmap (\(_, (inputEquality, _, _, _)) -> inputEquality) appInfo
    let finalBoolExpr = foldr (Conjunct . Query) networkFreeExpr (reverse inputEqualities)

    logDebug MaxDetail $
      "Final variables:" <+> pretty (fmap NetworkVar networkVariables <> fmap UserVar userVariables)

    logDebug MaxDetail $
      "Final expression:" <+> prettyFriendly (WithContext finalBoolExpr (mixedVariableDBCtx (MixedVariables userVariables networkVariables)))

    return $
      MetaNetworkPartition
        { metaNetwork = metaNetwork,
          networkVars = networkVariables,
          networkNormSteps = networkNormSteps,
          partitionExpr = finalBoolExpr
        }

type NetworkAppInfo =
  ( UnreducedAssertion,
    StandardNormExpr,
    [NetworkVariable],
    MetaNetworkEntry
  )

lineariseNetworkApplicationForest ::
  (MonadTraverseApplications m, MonadState (HashMap Name Int) m) =>
  [(NetworkApplication, NetworkAppInfo)] ->
  NetworkApplicationForest ->
  m [(NetworkApplication, NetworkAppInfo)]
lineariseNetworkApplicationForest appInfo forest = do
  let linearised = sortBy compareApplicationTree $ HashSet.toList forest
  foldlM lineariseNetworkApplicationTree appInfo linearised

lineariseNetworkApplicationTree ::
  (MonadTraverseApplications m, MonadState (HashMap Name Int) m) =>
  [(NetworkApplication, NetworkAppInfo)] ->
  NetworkApplicationTree ->
  m [(NetworkApplication, NetworkAppInfo)]
lineariseNetworkApplicationTree appInfo (Node app branches) = do
  branchInfo <- lineariseNetworkApplicationForest appInfo branches
  info <- getNetworkApplicationInfo app branchInfo
  return $ info : branchInfo

getNetworkApplicationInfo ::
  (MonadTraverseApplications m, MonadState (HashMap Name Int) m) =>
  NetworkApplication ->
  [(NetworkApplication, NetworkAppInfo)] ->
  m (NetworkApplication, NetworkAppInfo)
getNetworkApplicationInfo originalApp@(networkName, originalSpine) subAppInfo = do
  NetworkReaderCtx {..} <- ask
  let originalBoundCtx = mixedVariableDBCtx (MixedVariables userVariables mempty)
  let appDoc = prettyFriendly (WithContext (applicationExpr originalApp) originalBoundCtx)
  logCompilerPass MaxDetail ("processing network application:" <+> appDoc) $ do
    -- Update the number of times this particular network occurs
    seenApplications <- get
    let appID = sum seenApplications
    networkApplicationCount <- gets (fromMaybe 0 <$> HashMap.lookup networkName)
    modify (HashMap.insertWith (+) networkName 1)

    -- Use the applications inside the spine to calculate the updated spine for this application.
    let subst = getOutputVarSubsitution subAppInfo
    updatedSpine <- replaceApplicationsInSpine subst originalSpine
    let updatedApp = (networkName, updatedSpine)

    -- Create the input and output variable
    let numUserVars = length userVariables
    arg <- getNetworkApplicationArg updatedApp
    let mkVar j = VBoundVar (Lv (numUserVars + j)) []
    let inputVar = mkVar (2 * appID)
    let outputVar = mkVar (2 * appID + 1)

    -- Create the corresponding network variables
    (networkFile, networkType) <- getNetworkDetailsFromCtx networkContext networkName
    let inputDimensions = dimensions (inputTensor networkType)
    let outputDimensions = dimensions (outputTensor networkType)
    let inputNetworkVariable = NetworkVariable networkName networkApplicationCount inputDimensions Input Nothing
    let outputNetworkVariable = NetworkVariable networkName networkApplicationCount outputDimensions Output Nothing
    let networkVariables = [outputNetworkVariable, inputNetworkVariable]

    -- Create the input equality
    let mkInputVarEquality = mkInputVarEqualityExpr inputDimensions
    let inputEquality =
          VectorEqualityAssertion $
            VectorEquality
              { assertionLHS = inputVar,
                assertionRHS = arg,
                assertionDims = inputDimensions,
                assertionOriginalRel = mkInputVarEquality
              }

    -- Create the meta network entry
    let metaNetworkEntry =
          MetaNetworkEntry
            { metaNetworkEntryName = networkName,
              metaNetworkEntryType = networkType,
              metaNetworkEntryFilePath = networkFile
            }

    let newInfo = (updatedApp, (inputEquality, outputVar, networkVariables, metaNetworkEntry))

    let newBoundCtx = mixedVariableDBCtx (MixedVariables userVariables (getNetworkContext (newInfo : subAppInfo)))

    let updatedAppDoc = prettyFriendly (WithContext (applicationExpr updatedApp) newBoundCtx)
    let inputEqualityDoc = prettyFriendly (WithContext inputEquality newBoundCtx)
    let outputVarDoc = prettyFriendly (WithContext outputVar newBoundCtx)
    unless (null subAppInfo) $ do
      logDebug MaxDetail $ "Simplified network application to:" <+> updatedAppDoc
    logDebug MaxDetail $ "Introducing input variable:" <+> pretty inputNetworkVariable <+> parens (prettyVerbose inputVar)
    logDebug MaxDetail $ "Introducing output variable:" <+> pretty outputNetworkVariable <+> parens (prettyVerbose outputVar)
    logDebug MaxDetail $ "Adding input equality:" <+> inputEqualityDoc
    logDebug MaxDetail $ "Replacing" <+> squotes appDoc <+> "with output variable" <+> squotes outputVarDoc

    return newInfo

getOutputVarSubsitution ::
  [(NetworkApplication, NetworkAppInfo)] ->
  MetaNetworkVariableSubstitution
getOutputVarSubsitution applications =
  HashMap.fromList (fmap (second (\(_, outputVar, _, _) -> outputVar)) applications)

getNetworkContext ::
  [(NetworkApplication, NetworkAppInfo)] ->
  BoundCtx NetworkVariable
getNetworkContext = concatMap (\(_, (_, _, networkVars, _)) -> networkVars)

getNetworkApplicationArg :: (MonadCompile m) => NetworkApplication -> m StandardNormExpr
getNetworkApplicationArg (networkName, spine) = case spine of
  [ExplicitArg _ arg] -> return arg
  _ ->
    compilerDeveloperError $
      "Network" <+> quotePretty networkName <+> "does not seem to have a single explicit argument."

getNetworkDetailsFromCtx :: (MonadCompile m) => NetworkContext -> Name -> m (FilePath, NetworkType)
getNetworkDetailsFromCtx networkCtx name = do
  case Map.lookup name networkCtx of
    Just details -> return details
    Nothing ->
      compilerDeveloperError $
        "Either" <+> quotePretty name <+> "is not a network or it is not in scope"

mkInputVarEqualityExpr ::
  TensorDimensions ->
  StandardNormExpr ->
  StandardNormExpr ->
  StandardNormExpr
mkInputVarEqualityExpr dimensions e1 e2 = do
  mkVectorEquality (fmap VNatLiteral dimensions) [e1, e2]
  where
    -- Would definitely be nicer to somehow reuse the type-class resolution machinary here,
    -- but it seems incredibly complicated to setup...
    mkVectorEquality :: [StandardNormExpr] -> StandardExplicitSpine -> StandardNormExpr
    mkVectorEquality dims spine =
      let p = mempty
       in case dims of
            [] -> VBuiltinFunction (Equals EqRat Eq) spine
            d : ds -> VFreeVar (identifierOf StdEqualsVector) (nonExplicitArgs <> fmap (ExplicitArg p) spine)
              where
                -- TensorType VRatType (VVecLiteral (IntType p) (fmap (ExplicitArg p) (d : ds)))
                tensorType = VUnitLiteral
                nonExplicitArgs =
                  [ ImplicitArg p tensorType,
                    ImplicitArg p tensorType,
                    ImplicitArg p d,
                    InstanceArg p (mkVectorEquality ds [])
                  ]

replaceApplicationsInAssertion ::
  (MonadTraverseApplications m) =>
  MetaNetworkVariableSubstitution ->
  UnreducedAssertion ->
  m UnreducedAssertion
replaceApplicationsInAssertion subst = \case
  VectorEqualityAssertion VectorEquality {..} -> do
    updatedLHS <- replaceApplicationsInExpr subst assertionLHS
    updatedRHS <- replaceApplicationsInExpr subst assertionRHS
    return $
      VectorEqualityAssertion $
        VectorEquality
          { assertionLHS = updatedLHS,
            assertionRHS = updatedRHS,
            ..
          }
  NonVectorEqualityAssertion expr -> do
    updatedExpr <- replaceApplicationsInExpr subst expr
    return $ NonVectorEqualityAssertion updatedExpr

replaceApplicationsInExpr ::
  (MonadTraverseApplications m) =>
  MetaNetworkVariableSubstitution ->
  StandardNormExpr ->
  m StandardNormExpr
replaceApplicationsInExpr subst expr = case expr of
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VMeta {} -> normalisationError currentPass "Lam"
  VLam {} -> normalisationError currentPass "Lam"
  VBoundVar v spine -> VBoundVar v <$> replaceApplicationsInSpine subst spine
  VBuiltin b spine -> VBuiltin b <$> traverse (replaceApplicationsInExpr subst) spine
  -- By construction, finite quantifiers are not left in if they contain
  -- references to a neural network.
  VFiniteQuantifier {} -> return expr
  VFreeVar ident spine -> do
    NetworkReaderCtx {..} <- ask
    spine' <- replaceApplicationsInSpine subst spine
    if nameOf ident `Map.notMember` networkContext
      then return $ VFreeVar ident spine'
      else replaceApplication subst (nameOf ident, spine')

replaceApplicationsInSpine ::
  (MonadTraverseApplications m) =>
  MetaNetworkVariableSubstitution ->
  StandardSpine ->
  m StandardSpine
replaceApplicationsInSpine subst =
  traverse (traverse (replaceApplicationsInExpr subst))

replaceApplication ::
  forall m.
  (MonadTraverseApplications m) =>
  MetaNetworkVariableSubstitution ->
  NetworkApplication ->
  m StandardNormExpr
replaceApplication subst app = do
  let expr = applicationExpr app
  case HashMap.lookup app subst of
    Just output -> return output
    Nothing -> do
      let appDoc = prettyVerbose expr
      compilerDeveloperError $ "Unable to find network application" <+> appDoc

currentPass :: Doc a
currentPass = "insertion of magic network variables"
