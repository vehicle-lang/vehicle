module Vehicle.Verify.QueryFormat.VNNLib where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Sequence (Seq)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Marabou query format

-- | The query format accepted by the Marabou verifier.
vnnlibQueryFormat :: QueryFormat
vnnlibQueryFormat =
  QueryFormat
    { queryFormatID = VNNLibQueryFormat,
      compileQuery = compileVNNLibQuery,
      queryOutputFormat =
        ExternalOutputFormat
          { formatName = pretty VNNLibQueryFormat,
            formatVersion = Nothing,
            commentToken = ";"
          }
    }

-- | Compiles an expression representing a single Marabou query. The expression
-- passed should only have conjunctions and existential quantifiers at the boolean
-- level.
compileVNNLibQuery :: MonadLogger m => CLSTProblem NetworkVariable -> m QueryText
compileVNNLibQuery (CLSTProblem variables assertions) = do
  let variableNames = sequentialIONetworkVariableNaming "X_" "Y_" variables
  variableDocs <- forM variableNames compileVariable
  assertionDocs <- forM assertions (compileAssertion variableNames)
  let assertionsDoc = vsep assertionDocs <> line <> vsep variableDocs
  return $ layoutAsText assertionsDoc

compileVariable :: MonadLogger m => Name -> m (Doc a)
compileVariable varName = return $ parens ("declare-fun" <+> pretty varName <+> "() Real")

compileAssertion ::
  MonadLogger m =>
  Seq Name ->
  Assertion SolvingLinearExpr ->
  m (Doc a)
compileAssertion variableNames assertion = do
  let (coeffVars, rel, constant) = convertToSparseFormat assertion variableNames

  let compiledRel = compileRel rel
  let compiledLHS = foldl compileVar "" (NonEmpty.tail coeffVars)
  let compiledRHS = prettyCoefficient constant
  return $ parens "assert" <+> parens (compiledRel <+> parens compiledLHS <+> compiledRHS)

compileRel :: Either () OrderOp -> Doc a
compileRel = \case
  Left () -> "=="
  Right Le -> "<="
  Right Ge -> ">="
  Right Lt -> "<"
  Right Gt -> ">"

compileVar :: Doc a -> (Coefficient, Name) -> Doc a
compileVar r (coef, var)
  | coef == 1 = "+" <+> parens r <+> pretty var
  | coef == -1 = "-" <+> parens r <+> pretty var
  | coef < 0 = "-" <+> parens r <+> parens ("*" <+> prettyCoefficient (-coef) <+> pretty var)
  | otherwise = "+" <+> parens r <+> parens ("*" <+> prettyCoefficient coef <+> pretty var)
