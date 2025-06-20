module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkTensorType (dimensions), NetworkType (inputTensor, outputTensor))
import Vehicle.Data.QuantifiedVariable (prettyRationalAsFloat)
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface

--------------------------------------------------------------------------------
-- Marabou query format

-- | The query format accepted by the Marabou verifier.
vnnlibQueryFormat :: QueryFormat
vnnlibQueryFormat =
  QueryFormat
    { queryFormatID = VNNLibQueries,
      supportsStrictInequalities = True,
      supportsMultipleNetworks = True,
      compileVariable = compileVNNLibVar,
      compileQuery = compileVNNLibQuery,
      queryOutputFormat = outputFormat
    }

outputFormat :: ExternalOutputFormat
outputFormat =
  ExternalOutputFormat
    { formatName = pretty VNNLibQueries,
      formatVersion = Nothing,
      commentStyle = Line ";",
      emptyLines = True
    }

-- | Compile network variable name
compileNetworkVariableName :: Name -> Maybe Int -> InputOrOutput -> Doc a
compileNetworkVariableName networkName networkIndex inputOrOutput =
  compileNetworkName networkName networkIndex <> "_" <> if inputOrOutput == Input then "X" else "Y"

-- | Compile network name using _ and application index
compileNetworkName :: Name -> Maybe Int -> Doc a
compileNetworkName networkName Nothing = pretty networkName
compileNetworkName networkName (Just networkIndex) = pretty networkName <> "_" <> pretty networkIndex

-- | Compiles an individual variable
compileVNNLibVar :: CompileQueryVariable
compileVNNLibVar QueryVariableInfo {..} = do
  let networkVariableName = case numberOfNetworkApps of
        1 -> compileNetworkVariableName networkName Nothing inputOrOutput
        _ -> compileNetworkVariableName networkName (Just networkAppIndex) inputOrOutput
  layoutAsText $ networkVariableName <> pretty parentVariableIndices

-- | Compiles a network input
compileNetworkInput :: Name -> TensorShape -> Doc a
compileNetworkInput name shape = parens ("declare-input" <+> pretty name <+> "Real" <+> pretty shape)

-- | Compile a network output
compileNetworkOutput :: Name -> TensorShape -> Doc a
compileNetworkOutput name shape = parens ("declare-output" <+> pretty name <+> "Real" <+> pretty shape)

-- | Generates the variable name and fetches the shape of the the input or output network tensor
networkTensor :: MetaNetworkEntry -> InputOrOutput -> Maybe Int -> (Name, TensorShape)
networkTensor MetaNetworkEntry {..} inputOrOutput metaNetworkIndex = do
  let networkTensors = networkType metaNetworkEntryInfo
  case inputOrOutput of
    Input -> (layoutAsText $ compileNetworkVariableName metaNetworkEntryName metaNetworkIndex inputOrOutput, dimensions $ inputTensor networkTensors)
    Output -> (layoutAsText $ compileNetworkVariableName metaNetworkEntryName metaNetworkIndex inputOrOutput, dimensions $ outputTensor networkTensors)

-- | Compiles the declarations for a network query
compileNetworkEntry :: MetaNetworkEntry -> Maybe Int -> Doc a
compileNetworkEntry metaNetworkEntry@MetaNetworkEntry {..} metaNetworkIndex = do
  let networkInputDocs = indent 2 $ uncurry compileNetworkInput $ networkTensor metaNetworkEntry Input metaNetworkIndex
  let networkOutputDocs = indent 2 $ uncurry compileNetworkOutput $ networkTensor metaNetworkEntry Output metaNetworkIndex
  parens ("declare-network" <+> compileNetworkName metaNetworkEntryName metaNetworkIndex <> line <> networkInputDocs <> line <> networkOutputDocs)

-- | Compiles "all" (network) entries in the MetaNetwork
compileNetworks :: (MonadLogger m) => MetaNetwork -> m (Doc a)
compileNetworks metaNetwork =
  return $
    vsep
      ( zipWith compileNetworkEntry metaNetwork $
          if length metaNetwork == 1 then [Nothing] else map Just [0 ..]
      )

-- | Compiles an expression representing a single VNNLib query.
compileVNNLibQuery :: CompileQuery
compileVNNLibQuery _address metaNetwork _variables assertions = do
  networkDocs <- compileNetworks metaNetwork
  assertionDocs <- forM assertions compileAssertion
  let assertionsDoc = networkDocs <> line <> vsep assertionDocs
  return $ layoutAsText assertionsDoc

compileAssertion :: (MonadLogger m) => QueryAssertion QueryVariable -> m (Doc a)
compileAssertion QueryAssertion {..} = do
  let compiledRel = compileRel rel
  let (headVar NonEmpty.:| tailVars) = lhs
  let compiledLHS = foldl compileCoefVar (compileCoefFirstVar headVar) tailVars
  let compiledRHS = prettyRationalAsFloat rhs
  return $ parens ("assert" <+> parens (compiledRel <+> parens compiledLHS <+> compiledRHS))

compileRel :: QueryRelation -> Doc a
compileRel = \case
  EqRel -> "=="
  LeRel -> "<="
  GeRel -> ">="
  LtRel -> "<"
  GtRel -> ">"

compileCoefFirstVar :: (Coefficient, QueryVariable) -> Doc a
compileCoefFirstVar (coef, var)
  | coef == 1 = "+" <+> pretty var
  | coef == -1 = "-" <+> pretty var
  | coef < 0 = "-" <+> parens ("*" <+> prettyRationalAsFloat (-coef) <+> pretty var)
  | otherwise = "*" <+> prettyRationalAsFloat coef <+> pretty var

compileCoefVar :: Doc a -> (Coefficient, QueryVariable) -> Doc a
compileCoefVar r (coef, var)
  | coef == 1 = "+" <+> parens r <+> pretty var
  | coef == -1 = "-" <+> parens r <+> pretty var
  | coef < 0 = "-" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat (-coef) <+> pretty var)
  | otherwise = "+" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat coef <+> pretty var)
