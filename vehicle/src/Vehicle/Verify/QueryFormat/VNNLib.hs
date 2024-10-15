module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.QuantifiedVariable (prettyRationalAsFloat)
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
compileVNNLibVar inputOrOutput ioIndex = do
  let name = if inputOrOutput == Input then "X" else "Y"
  layoutAsText $ name <> "_" <> pretty ioIndex

-- | Compiles an expression representing a single VNNLib query.
compileVNNLibQuery :: CompileQuery
compileVNNLibQuery _address (QueryContents variables assertions) = do
  variableDocs <- forM variables compileVariableDecl
  assertionDocs <- forM assertions compileAssertion
  let assertionsDoc = vsep assertionDocs <> line <> vsep variableDocs
  return $ layoutAsText assertionsDoc

compileVariableDecl :: (MonadLogger m) => QueryVariable -> m (Doc a)
compileVariableDecl var = return $ parens ("declare-fun" <+> pretty var <+> "() Real")

compileAssertion :: (MonadLogger m) => QueryAssertion QueryVariable -> m (Doc a)
compileAssertion QueryAssertion {..} = do
  let compiledRel = compileRel rel
  let compiledLHS = foldl compileCoefVar "" (NonEmpty.tail lhs)
  let compiledRHS = prettyRationalAsFloat rhs
  return $ parens "assert" <+> parens (compiledRel <+> parens compiledLHS <+> compiledRHS)

compileRel :: QueryRelation -> Doc a
compileRel = \case
  EqualRel -> "=="
  OrderRel Le -> "<="
  OrderRel Ge -> ">="
  OrderRel Lt -> "<"
  OrderRel Gt -> ">"

compileCoefVar :: Doc a -> (Coefficient, QueryVariable) -> Doc a
compileCoefVar r (coef, var)
  | coef == 1 = "+" <+> parens r <+> pretty var
  | coef == -1 = "-" <+> parens r <+> pretty var
  | coef < 0 = "-" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat (-coef) <+> pretty var)
  | otherwise = "+" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat coef <+> pretty var)
