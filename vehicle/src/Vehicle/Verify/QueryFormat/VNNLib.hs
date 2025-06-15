module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Prelude
import Vehicle.Data.QuantifiedVariable (prettyRationalAsFloat)
import Vehicle.Syntax.Tensor (flattenIndices)
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
      commentToken = ";",
      emptyLines = True
    }

-- | Compiles an individual variable
compileVNNLibVar :: CompileQueryVariable
compileVNNLibVar QueryVariableInfo {..} = do
  let io = if inputOrOutput == Input then "X" else "Y"
  let networkIndex = if numberOfNetworkApps > 1 then pretty (networkAppIndex + 1) else ""
  let name = pretty networkName <> "@" <> networkIndex <> "@" <> io
  let index = flattenIndices parentVariableShape parentVariableIndices
  layoutAsText $ name <> "_" <> pretty index

-- | Compiles an expression representing a single VNNLib query.
compileVNNLibQuery :: CompileQuery
compileVNNLibQuery _address (QueryContents variables assertions) = do
  variableDocs <- forM variables compileVariableDecl
  assertionDocs <- forM assertions compileAssertion
  let assertionsDoc = vsep assertionDocs <> line <> vsep variableDocs
  return $ layoutAsText assertionsDoc

compileVariableDecl :: (MonadLogger m) => QueryVariable -> m (Doc a)
compileVariableDecl var = return $ parens ("declare-fun" <+> pretty var <+> "Real")

compileAssertion :: (MonadLogger m) => QueryAssertion QueryVariable -> m (Doc a)
compileAssertion QueryAssertion {..} = do
  let compiledRel = compileRel rel
  let (headVar NonEmpty.:| tailVars) = lhs
  let compiledLHS = foldl compileCoefVar (compileCoefFirstVar headVar) tailVars
  let compiledRHS = prettyRationalAsFloat rhs
  return $ parens ("assert" <+> compiledRel <+> parens compiledLHS <+> compiledRHS)

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
