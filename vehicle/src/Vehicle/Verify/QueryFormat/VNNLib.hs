module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Version (Version (..))
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
      formatVersion = Just vnnlibVersion,
      commentStyle = Line lineComment,
      emptyLines = True
    }

vnnlibVersion :: Version
vnnlibVersion = Version [2, 0] []

lineComment :: Doc a
lineComment = ";"

-- | Compiles an expression representing a single VNNLib query.
compileVNNLibQuery :: CompileQuery
compileVNNLibQuery _address metaNetwork _variables assertions = do
  networkDocs <- compileNetworks metaNetwork
  assertionDocs <- forM assertions compileAssertion
  let assertionsDoc =
        "(vnnlib-version <"
          <> pretty vnnlibVersion
          <> ">)"
          <> line
          <> line
          <> networkDocs
          <> line
          <> line
          <> vsep assertionDocs
  return $ layoutAsText assertionsDoc

-- | Compiles all network entries in the MetaNetwork
compileNetworks :: (MonadLogger m) => MetaNetwork -> m (Doc a)
compileNetworks metaNetwork = return $ vsep $ fmap compileNetwork metaNetwork

compileNetwork :: (Name, NetworkContextInfo, Int) -> Doc a
compileNetwork (name, info, apps) = do
  let compileFn = compileNetworkApp name (networkType info)
  vsep (fmap compileFn ([1 .. apps] :: [Int]))

-- | Compiles the declarations for a network query
compileNetworkApp :: Name -> NetworkType -> Int -> Doc a
compileNetworkApp networkName networkType appIndex = do
  let equalToDoc
        | appIndex > 1 = "(equalTo" <+> compileNetworkName networkName 1 <> ")" <> line
        | otherwise = ""
  let networkVarName = compileNetworkName networkName appIndex
  let networkInputDocs = uncurry compileNetworkInput $ networkTensor networkName networkType appIndex Input
  let networkOutputDocs = uncurry compileNetworkOutput $ networkTensor networkName networkType appIndex Output
  "(declare-network" <+> networkVarName
    <> line
    <> indent
      2
      ( equalToDoc
          <> networkInputDocs
          <> line
          <> networkOutputDocs
      )
    <> line
    <> ")"

-- | Generates the variable name and fetches the shape of the the input or output network tensor
networkTensor :: Name -> NetworkType -> Int -> InputOrOutput -> (Name, TensorShape)
networkTensor networkName networkType appIndex inputOrOutput = do
  let name = layoutAsText $ compileNetworkVariableName networkName appIndex inputOrOutput
  case inputOrOutput of
    Input -> (name, dimensions $ inputTensor networkType)
    Output -> (name, dimensions $ outputTensor networkType)

-- | Compile network name. Prefixes an index to all subsequent network applications
compileNetworkName :: Name -> Int -> Doc a
compileNetworkName networkName appIndex
  | appIndex == 1 = pretty networkName
  | otherwise = pretty networkName <> "_" <> pretty appIndex

-- | Compile network variable name
compileNetworkVariableName :: Name -> Int -> InputOrOutput -> Doc a
compileNetworkVariableName networkName appIndex inputOrOutput =
  compileNetworkName networkName appIndex <> "_" <> if inputOrOutput == Input then "X" else "Y"

-- | Compiles a network input
compileNetworkInput :: Name -> TensorShape -> Doc a
compileNetworkInput name shape = parens ("declare-input" <+> pretty name <+> "Real" <+> pretty shape)

-- | Compile a network output
compileNetworkOutput :: Name -> TensorShape -> Doc a
compileNetworkOutput name shape = parens ("declare-output" <+> pretty name <+> "Real" <+> pretty shape)

-- | Compiles an individual variable
compileVNNLibVar :: CompileQueryVariable
compileVNNLibVar QueryVariableInfo {..} = do
  let networkVariableName = compileNetworkVariableName networkName networkAppIndex inputOrOutput
  layoutAsText $ networkVariableName <> pretty parentVariableIndices

compileAssertion :: (MonadLogger m) => QueryAssertion QueryVariable -> m (Doc a)
compileAssertion QueryAssertion {..} = do
  let compiledRel = compileRel rel
  let (headVar NonEmpty.:| tailVars) = lhs
  let compiledLHS = case tailVars of
        [] -> compileCoefVar headVar
        _ -> parens ("+" <+> hsep (fmap compileCoefVar lhs))
  let compiledRHS = prettyRationalAsFloat rhs
  return $ parens ("assert" <+> parens (compiledRel <+> compiledLHS <+> compiledRHS))

compileRel :: QueryRelation -> Doc a
compileRel = \case
  EqRel -> "=="
  LeRel -> "<="
  GeRel -> ">="
  LtRel -> "<"
  GtRel -> ">"

compileCoefVar :: (Coefficient, QueryVariable) -> Doc a
compileCoefVar (coef, var)
  | coef == 1 = pretty var
  | coef == -1 = parens ("-" <+> pretty var)
  | otherwise = parens ("*" <+> prettyRationalAsFloat coef <+> pretty var)
