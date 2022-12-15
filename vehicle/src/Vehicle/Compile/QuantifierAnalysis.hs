module Vehicle.Compile.QuantifierAnalysis
  ( checkQuantifiersAndNegateIfNecessary,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (catMaybes)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Queries.DNF (lowerNot)
import Vehicle.Verify.Core (VerifierIdentifier)

checkQuantifiersAndNegateIfNecessary ::
  MonadCompile m =>
  VerifierIdentifier ->
  Identifier ->
  CheckedExpr ->
  m (Bool, CheckedExpr)
checkQuantifiersAndNegateIfNecessary backend ident expr =
  logCompilerPass MinDetail "quantifier analysis" $ do
    quantifier <- checkQuantifiersAreHomogeneous backend ident expr
    logDebug MinDetail $ "Quantifier type: " <> pretty quantifier

    (negated, outputExpr) <- case quantifier of
      Just Forall -> do
        -- If the property is universally quantified then we need to negate the expression
        logDebug MinDetail "Negating property..."
        return (True, lowerNot expr)
      _ -> return (False, expr)

    logCompilerPassOutput (prettyFriendly outputExpr)
    return (negated, outputExpr)

-- | Checks that the quantifiers within the expression are homogeneous,
-- returning the quantifier. Defaults to returning `All` if the expression
-- contains no quantifiers.
checkQuantifiersAreHomogeneous ::
  forall m.
  MonadCompile m =>
  VerifierIdentifier ->
  Identifier ->
  CheckedExpr ->
  m (Maybe Quantifier)
checkQuantifiersAreHomogeneous target ident expr = fmap fst <$> go expr
  where
    go :: CheckedExpr -> m (Maybe (Quantifier, Provenance))
    go e = case e of
      Ann {} -> normalisationError currentPass "Ann"
      Let {} -> normalisationError currentPass "Let"
      Lam {} -> normalisationError currentPass "Lam"
      Universe {} -> unexpectedTypeInExprError currentPass "Universe"
      Pi {} -> unexpectedTypeInExprError currentPass "Pi"
      Hole {} -> visibilityError currentPass "Hole"
      Meta {} -> resolutionError currentPass "Meta"
      Literal {} -> return Nothing
      Builtin {} -> return Nothing
      Var {} -> return Nothing
      LVec {} -> return Nothing
      ExistsRatExpr p _ body -> checkEqual p Exists =<< go body
      ForallRatExpr p _ body -> checkEqual p Forall =<< go body
      ExistsNatExpr p binder _ -> unsupportedVariableType p binder
      ForallNatExpr p binder _ -> unsupportedVariableType p binder
      ExistsIntExpr p binder _ -> unsupportedVariableType p binder
      ForallIntExpr p binder _ -> unsupportedVariableType p binder
      App _ann _fun args -> do
        xs <- traverse go (onlyExplicit args)
        return $ catMaybes xs !!? 0

    checkEqual ::
      Provenance ->
      Quantifier ->
      Maybe (Quantifier, Provenance) ->
      m (Maybe (Quantifier, Provenance))
    checkEqual p q = \case
      Just (q', _p)
        | q /= q' ->
            compilerDeveloperError $
              "Mixed quantifiers found while compiling property"
                <+> pretty ident
                <+> "to"
                <+> pretty target
                <+> "which should have been caught by"
                <+> "the polarity type system before reaching this point."
      _ -> return (Just (q, p))

    unsupportedVariableType :: Provenance -> CheckedBinder -> m a
    unsupportedVariableType p binder =
      throwError $ UnsupportedVariableType target ident p (getBinderName binder) (binderType binder) [Constructor Rat]

currentPass :: Doc a
currentPass = "quantifier analysis"
