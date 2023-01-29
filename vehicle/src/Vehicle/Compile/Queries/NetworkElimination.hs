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
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
  ( insertWith,
    lookup,
  )
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Resource
import Vehicle.Expr.AlphaEquivalence ()
import Vehicle.Expr.DeBruijn
import Vehicle.Verify.Specification (MetaNetwork)

-- Pairs of (input variable == expression)
type InputEqualities = [(Int, CheckedExpr)]

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
  MonadCompile m =>
  NetworkContext ->
  BoundDBCtx ->
  CheckedExpr ->
  m (MetaNetwork, [NetworkVariable], InputEqualities, CheckedExpr)
replaceNetworkApplications networkCtx boundCtx query = do
  logCompilerPass MinDetail "input/output variable insertion" $ do
    let initialState = IOVarState mempty mempty mempty 0 0
    let processApplicationFn = processNetworkApplication networkCtx boundCtx

    (queryExpr, IOVarState {..}) <- runStateT (go query processApplicationFn) initialState
    let finalMetaNetwork = reverse metaNetwork
    let finalInputEqualities = concat (reverse inputEqualities)

    -- We can now calculate the meta-network and the network variables.
    networkVariables <- getNetworkVariables networkCtx finalMetaNetwork
    logDebug MinDetail $ "Generated meta-network" <+> pretty finalMetaNetwork <> line
    return (finalMetaNetwork, networkVariables, finalInputEqualities, queryExpr)
  where
    go ::
      MonadCompile m =>
      CheckedExpr ->
      (Identifier -> CheckedExpr -> m CheckedExpr) ->
      m CheckedExpr
    go expr k = case expr of
      Universe {} -> unexpectedTypeInExprError currentPass "Universe"
      Pi {} -> unexpectedTypeInExprError currentPass "Pi"
      Hole {} -> resolutionError currentPass "Hole"
      Meta {} -> resolutionError currentPass "Meta"
      Ann {} -> normalisationError currentPass "Ann"
      Lam {} -> normalisationError currentPass "Lam"
      Let {} -> normalisationError currentPass "Let"
      Var {} -> return expr
      Literal {} -> return expr
      Builtin {} -> return expr
      LVec p xs -> LVec p <$> traverse (flip go k) xs
      App p fun args -> do
        fun' <- go fun k
        args' <- traverse (traverse (flip go k)) args
        case (fun', args') of
          (FreeVar _ network, [ExplicitArg _ arg]) -> k network arg
          (FreeVar _ network, _) ->
            compilerDeveloperError $
              "Network" <+> quotePretty network <+> "seems to have multiple arguments"
          _ -> do
            runSilentLoggerT $
              normaliseExpr (normApp p fun' args') $
                fullNormalisationOptions
                  { boundContext = boundCtx
                  }

-- | The current state of the input/output network variables.
data IOVarState = IOVarState
  { applicationCache :: HashMap (Identifier, CheckedExpr) CheckedExpr,
    metaNetwork :: MetaNetwork,
    inputEqualities :: [InputEqualities],
    magicInputVarCount :: Int,
    magicOutputVarCount :: Int
  }

processNetworkApplication ::
  (MonadCompile m, MonadState IOVarState m) =>
  NetworkContext ->
  BoundDBCtx ->
  Identifier ->
  CheckedExpr ->
  m CheckedExpr
processNetworkApplication networkCtx boundCtx ident inputVector = do
  let sectionLog = "Replacing application:" <+> pretty ident <+> prettyVerbose inputVector
  logCompilerSection MaxDetail sectionLog $ do
    IOVarState {..} <- get
    let p = mempty
    case HashMap.lookup (ident, inputVector) applicationCache of
      Just result -> return result
      Nothing -> do
        (networkFile, NetworkType inputs outputs) <- getNetworkDetailsFromCtx networkCtx (nameOf ident)
        let inputSize = tensorSize inputs
        let outputSize = tensorSize outputs
        let outputType = baseType outputs

        let numberOfUserVariables = length boundCtx
        let inputStartingDBIndex = numberOfUserVariables + magicInputVarCount + magicOutputVarCount
        let outputStartingDBIndex = inputStartingDBIndex + inputSize
        let outputEndingDBIndex = outputStartingDBIndex + outputSize
        let inputVarIndices = [inputStartingDBIndex .. outputStartingDBIndex - 1]
        let outputVarIndices = [outputStartingDBIndex .. outputEndingDBIndex - 1]

        logDebug MaxDetail $ "starting index:            " <+> pretty inputStartingDBIndex
        logDebug MaxDetail $ "number of input variables: " <+> pretty inputSize
        logDebug MaxDetail $ "number of output variables:" <+> pretty outputSize
        logDebug MaxDetail $ "input indices:             " <+> pretty inputVarIndices
        logDebug MaxDetail $ "output indices:            " <+> pretty outputVarIndices

        inputVarEqualities <- createInputVarEqualities (dimensions inputs) inputVarIndices inputVector

        outputVarsExpr <- mkMagicVariableSeq p outputType (dimensions outputs) outputVarIndices

        put $
          IOVarState
            { applicationCache = HashMap.insert (ident, inputVector) outputVarsExpr applicationCache,
              inputEqualities = inputVarEqualities : inputEqualities,
              metaNetwork = (identifierName ident, networkFile) : metaNetwork,
              magicInputVarCount = magicInputVarCount + inputSize,
              magicOutputVarCount = magicInputVarCount + outputSize
            }

        return outputVarsExpr

createInputVarEqualities :: MonadCompile m => [Int] -> [Int] -> CheckedExpr -> m InputEqualities
createInputVarEqualities (_dim : dims) inputVarIndices (VecLiteral _ _ xs) = do
  let inputVarIndicesChunks = chunksOf (product dims) inputVarIndices
  concat <$> zipWithM (createInputVarEqualities dims) inputVarIndicesChunks xs
createInputVarEqualities [] [i] e = return [(i, e)]
createInputVarEqualities dims d xs =
  compilerDeveloperError $
    "apparently miscalculated number of magic input variables:"
      <+> pretty dims
      <+> pretty d
      <+> prettyVerbose xs

mkMagicVariableSeq ::
  MonadCompile m =>
  Provenance ->
  NetworkBaseType ->
  [Int] ->
  [Int] ->
  m CheckedExpr
mkMagicVariableSeq p tElem = go
  where
    baseElemType = reconstructNetworkBaseType tElem p

    go :: MonadCompile m => [Int] -> [Int] -> m CheckedExpr
    go (_dim : dims) outputVarIndices = do
      let outputVarIndicesChunks = chunksOf (product dims) outputVarIndices
      elems <- traverse (go dims) outputVarIndicesChunks
      let elemType = mkTensorType p baseElemType (mkTensorDims p dims)
      return (mkVec p elemType elems)
    go [] [outputVarIndex] =
      return $ BoundVar p $ DBIndex outputVarIndex
    go dims outputVarIndices =
      compilerDeveloperError $
        "apparently miscalculated number of magic output variables:"
          <+> pretty tElem
          <+> pretty dims
          <+> pretty outputVarIndices

type Applications = Map Name Int

getNetworkVariables :: MonadCompile m => NetworkContext -> MetaNetwork -> m [NetworkVariable]
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

getNetworkDetailsFromCtx :: MonadCompile m => NetworkContext -> Name -> m (FilePath, NetworkType)
getNetworkDetailsFromCtx networkCtx name = do
  case Map.lookup name networkCtx of
    Just details -> return details
    Nothing ->
      compilerDeveloperError $
        "Either" <+> squotes (pretty name) <+> "is not a network or it is not in scope"

getTypedMetaNetwork :: MonadCompile m => NetworkContext -> MetaNetwork -> m [(Name, NetworkType)]
getTypedMetaNetwork ctx = traverse $ \(name, _file) -> do
  (_file, networkType) <- getNetworkDetailsFromCtx ctx name
  return (name, networkType)

countNetworkApplications :: MetaNetwork -> Applications
countNetworkApplications = foldr (\(n, _f) m -> Map.insertWith (+) n 1 m) mempty

currentPass :: Doc a
currentPass = "insertion of magic network variables"
