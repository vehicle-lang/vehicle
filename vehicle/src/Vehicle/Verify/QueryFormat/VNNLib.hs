module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Prelude
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Syntax.Builtin
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
      formatQuery = compileVNNLibQuery,
      supportsStrictInequalities = True,
      compileVar = compileVNNLibVar,
      queryOutputFormat =
        ExternalOutputFormat
          { formatName = pretty VNNLibQueries,
            formatVersion = Nothing,
            commentToken = ";",
            emptyLines = True
          }
    }

-- | Compiles an expression representing a single Marabou query. The expression
-- passed should only have conjunctions and existential quantifiers at the boolean
-- level.
compileVNNLibQuery :: (MonadLogger m) => QueryAddress -> QueryContents -> m QueryText
compileVNNLibQuery _address (QueryContents variables assertions) = do
  variableDocs <- forM variables compileVariableDecl
  assertionDocs <- forM assertions compileAssertion
  let assertionsDoc = vsep assertionDocs <> line <> vsep variableDocs
  return $ layoutAsText assertionsDoc

compileVariableDecl :: (MonadLogger m) => NetworkRationalVariable -> m (Doc a)
compileVariableDecl var = return $ parens ("declare-fun" <+> compileVNNLibVar var <+> "() Real")

compileAssertion :: (MonadLogger m) => QueryAssertion -> m (Doc a)
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

compileCoefVar :: Doc a -> (Coefficient, NetworkRationalVariable) -> Doc a
compileCoefVar r (coef, var)
  | coef == 1 = "+" <+> parens r <+> compileVNNLibVar var
  | coef == -1 = "-" <+> parens r <+> compileVNNLibVar var
  | coef < 0 = "-" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat (-coef) <+> compileVNNLibVar var)
  | otherwise = "+" <+> parens r <+> parens ("*" <+> prettyRationalAsFloat coef <+> compileVNNLibVar var)

compileVNNLibVar :: NetworkRationalVariable -> Doc a
compileVNNLibVar var = do
  let name = if inputOrOutput (originalVar var) == Input then "X" else "Y"
  let index = computeAbsoluteIndex var
  name <> "_" <> pretty index
