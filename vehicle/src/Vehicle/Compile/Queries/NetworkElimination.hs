{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
module Vehicle.Compile.Queries.NetworkElimination
  ( InputEqualities,
    replaceNetworkApplications,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
  ( insertWith,
    lookup,
  )
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (reeval, runEmptyNormT)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Boolean (ConjunctAll (unConjunctAll))
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Hashing ()
import Vehicle.Expr.Normalised
import Vehicle.Verify.Specification (MetaNetwork)

-- Pairs of (input variable == expression)
-- TODO push back through this file once changing NormalisableExpr to Value
type InputEqualities = [(Lv, StandardNormExpr)]

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
-- variables, and returns:
--   - the ordered list of network applications
--   - the network variables
--   - equalities relating network input variables to the corresponding expressions
--   - the final expression.
replaceNetworkApplications ::
  (MonadCompile m) =>
  NetworkContext ->
  BoundDBCtx ->
  ConjunctAll StandardNormExpr ->
  m (MetaNetwork, [NetworkVariable], InputEqualities, ConjunctAll StandardNormExpr)
replaceNetworkApplications networkCtx boundCtx conjunctions = do
  logCompilerPass MinDetail "input/output variable insertion" $ do
    let initialState = IOVarState mempty mempty mempty 0 0
    let processApplicationFn = processNetworkApplication networkCtx boundCtx

    (networkFreeConjunctions, IOVarState {..}) <-
      runStateT (traverse (go processApplicationFn) conjunctions) initialState
    let finalMetaNetwork = reverse metaNetwork
    let finalInputEqualities = concat (reverse inputEqualities)

    -- We can now calculate the meta-network and the network variables.
    networkVariables <- getNetworkVariables networkCtx finalMetaNetwork
    logDebug MinDetail $ "Generated meta-network" <+> pretty finalMetaNetwork <> line

    normQueryExpr <- traverse reevalute networkFreeConjunctions

    logCompilerPassOutput $ prettyVerbose (NonEmpty.toList $ unConjunctAll normQueryExpr)
    return (finalMetaNetwork, networkVariables, finalInputEqualities, normQueryExpr)
  where
    go ::
      (MonadCompile m) =>
      (Identifier -> StandardNormExpr -> m StandardNormExpr) ->
      StandardNormExpr ->
      m StandardNormExpr
    go k expr = case expr of
      VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
      VPi {} -> unexpectedTypeInExprError currentPass "Pi"
      VMeta {} -> normalisationError currentPass "Lam"
      VLam {} -> normalisationError currentPass "Lam"
      VBoundVar v spine -> VBoundVar v <$> goSpine k spine
      VBuiltin b spine -> VBuiltin b <$> traverse (go k) spine
      VFreeVar network spine -> do
        spine' <- goSpine k spine
        case spine' of
          [ExplicitArg _ arg] -> k network arg
          _ ->
            compilerDeveloperError $
              "Network" <+> quotePretty network <+> "seems to have multiple arguments"

    goSpine ::
      (MonadCompile m) =>
      (Identifier -> StandardNormExpr -> m StandardNormExpr) ->
      StandardSpine ->
      m StandardSpine
    goSpine k = traverse (traverse (go k))

reevalute :: (MonadCompile m) => StandardNormExpr -> m StandardNormExpr
reevalute expr = runEmptyNormT @StandardBuiltinType (reeval expr)

-- | The current state of the input/output network variables.
data IOVarState = IOVarState
  { applicationCache :: HashMap (Identifier, StandardNormExpr) StandardNormExpr,
    metaNetwork :: MetaNetwork,
    inputEqualities :: [[(Lv, StandardNormExpr)]],
    magicInputVarCount :: Int,
    magicOutputVarCount :: Int
  }

processNetworkApplication ::
  (MonadCompile m, MonadState IOVarState m) =>
  NetworkContext ->
  BoundDBCtx ->
  Identifier ->
  StandardNormExpr ->
  m StandardNormExpr
processNetworkApplication networkCtx boundCtx ident inputVector = do
  let sectionLog = "Replacing application:" <+> pretty ident <+> prettyVerbose inputVector
  logCompilerSection MaxDetail sectionLog $ do
    IOVarState {..} <- get
    case HashMap.lookup (ident, inputVector) applicationCache of
      Just result -> return result
      Nothing -> do
        (networkFile, NetworkType inputs outputs) <- getNetworkDetailsFromCtx networkCtx (nameOf ident)
        let inputSize = tensorSize inputs
        let outputSize = tensorSize outputs
        let outputType = baseType outputs

        let numberOfUserVariables = length boundCtx
        let inputStartingDBLevel = Lv $ numberOfUserVariables + magicInputVarCount + magicOutputVarCount
        let outputStartingDBLevel = inputStartingDBLevel + Lv inputSize
        let outputEndingDBLevel = outputStartingDBLevel + Lv outputSize
        let inputVarIndices = [inputStartingDBLevel .. outputStartingDBLevel - 1]
        let outputVarIndices = [outputStartingDBLevel .. outputEndingDBLevel - 1]

        logDebug MaxDetail $ "starting level:            " <+> pretty inputStartingDBLevel
        logDebug MaxDetail $ "number of input variables: " <+> pretty inputSize
        logDebug MaxDetail $ "number of output variables:" <+> pretty outputSize
        logDebug MaxDetail $ "input levels:             " <+> pretty inputVarIndices
        logDebug MaxDetail $ "output levels:            " <+> pretty outputVarIndices

        inputVarEqualities <- createInputVarEqualities (dimensions inputs) inputVarIndices inputVector

        outputVarsExpr <- mkMagicVariableSeq outputType (dimensions outputs) outputVarIndices

        put $
          IOVarState
            { applicationCache = HashMap.insert (ident, inputVector) outputVarsExpr applicationCache,
              inputEqualities = inputVarEqualities : inputEqualities,
              metaNetwork = (identifierName ident, networkFile) : metaNetwork,
              magicInputVarCount = magicInputVarCount + inputSize,
              magicOutputVarCount = magicInputVarCount + outputSize
            }

        return outputVarsExpr

createInputVarEqualities ::
  (MonadCompile m) =>
  TensorDimensions ->
  [Lv] ->
  StandardNormExpr ->
  m [(Lv, StandardNormExpr)]
createInputVarEqualities [] [i] e = return [(i, e)]
createInputVarEqualities (_dim : dims) inputVarIndices (VVecLiteral xs) = do
  let inputVarIndicesChunks = chunksOf (product dims) inputVarIndices
  concat <$> zipWithM (createInputVarEqualities dims) inputVarIndicesChunks xs
createInputVarEqualities dims d xs =
  compilerDeveloperError $
    "apparently miscalculated number of magic input variables:"
      <+> pretty dims
      <+> pretty d
      <+> prettyVerbose xs

mkMagicVariableSeq ::
  (MonadCompile m) =>
  NetworkBaseType ->
  TensorDimensions ->
  [Lv] ->
  m StandardNormExpr
mkMagicVariableSeq tElem = go
  where
    go :: (MonadCompile m) => TensorDimensions -> [Lv] -> m StandardNormExpr
    go (_dim : dims) outputVarIndices = do
      let outputVarIndicesChunks = chunksOf (product dims) outputVarIndices
      elems <- traverse (go dims) outputVarIndicesChunks
      return $ mkVLVec elems
    go [] [outputVar] =
      return $ VBoundVar outputVar []
    go dims outputVarIndices =
      compilerDeveloperError $
        "apparently miscalculated number of magic output variables:"
          <+> pretty tElem
          <+> pretty dims
          <+> pretty outputVarIndices

type Applications = Map Name Int

getNetworkVariables :: (MonadCompile m) => NetworkContext -> MetaNetwork -> m [NetworkVariable]
getNetworkVariables networkCtx metaNetwork = do
  let applicationCounts = countNetworkApplications metaNetwork
  metaNetworkDetails <- getTypedMetaNetwork networkCtx metaNetwork
  let (_, result) = foldr (forNetwork applicationCounts) (mempty, mempty) metaNetworkDetails
  return result
  where
    forNetwork ::
      Applications ->
      (Name, NetworkType) ->
      (Applications, [NetworkVariable]) ->
      (Applications, [NetworkVariable])
    forNetwork totalApplications (networkName, NetworkType inputs outputs) (applicationsSoFar, result) = do
      let application = case Map.lookup networkName totalApplications of
            Nothing -> Nothing
            Just 1 -> Nothing
            Just _ -> Map.lookup networkName applicationsSoFar
      let newApplicationsSoFar = Map.insertWith (+) networkName 1 applicationsSoFar

      let inputIndices = [0 .. tensorSize inputs - 1]
      let outputIndices = [0 .. tensorSize outputs - 1]
      let inputNames = [NetworkVariable networkName application Input i | i <- inputIndices]
      let outputNames = [NetworkVariable networkName application Output i | i <- outputIndices]

      (newApplicationsSoFar, result <> inputNames <> outputNames)

getNetworkDetailsFromCtx :: (MonadCompile m) => NetworkContext -> Name -> m (FilePath, NetworkType)
getNetworkDetailsFromCtx networkCtx name = do
  case Map.lookup name networkCtx of
    Just details -> return details
    Nothing ->
      compilerDeveloperError $
        "Either" <+> squotes (pretty name) <+> "is not a network or it is not in scope"

getTypedMetaNetwork :: (MonadCompile m) => NetworkContext -> MetaNetwork -> m [(Name, NetworkType)]
getTypedMetaNetwork ctx = traverse $ \(name, _file) -> do
  (_file, networkType) <- getNetworkDetailsFromCtx ctx name
  return (name, networkType)

countNetworkApplications :: MetaNetwork -> Applications
countNetworkApplications = foldr (\(n, _f) m -> Map.insertWith (+) n 1 m) mempty

currentPass :: Doc a
currentPass = "insertion of magic network variables"
