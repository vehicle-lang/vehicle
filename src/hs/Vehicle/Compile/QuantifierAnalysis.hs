
module Vehicle.Compile.QuantifierAnalysis
  ( checkQuantifiersAndNegateIfNecessary
  ) where

import Data.Maybe (catMaybes)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Backend.Prelude (Backend)
import Vehicle.Language.Print (prettyFriendly)
import Vehicle.Compile.Normalise.Core ( nfNot )

checkQuantifiersAndNegateIfNecessary :: MonadCompile m
                                     => Backend
                                     -> Identifier
                                     -> CheckedExpr
                                     -> m (Bool, CheckedExpr)
checkQuantifiersAndNegateIfNecessary backend ident expr =
  logCompilerPass MinDetail "quantifier analysis" $ do
    quantifier <- checkQuantifiersAreHomogeneous backend ident expr
    logDebug MinDetail $ "Quantifier type: " <> pretty quantifier

    outputExpr <- case quantifier of
      Exists  -> return expr
      Forall  -> do
        -- If the property is universally quantified then we need to negate the expression
        logDebug MinDetail "Negating property..."
        let p = provenanceOf expr
        return $ nfNot p (ExplicitArg p expr)

    logCompilerPassOutput (prettyFriendly outputExpr)
    return (quantifier == Forall, outputExpr)

-- | Checks that the quantifiers within the expression are homogeneous,
-- returning the quantifier. Defaults to returning `All` if the expression
-- contains no quantifiers.
checkQuantifiersAreHomogeneous :: forall m . MonadCompile m
                               => Backend
                               -> Identifier
                               -> CheckedExpr
                               -> m Quantifier
checkQuantifiersAreHomogeneous target ident expr = maybe Forall fst <$> go expr
  where
    go :: CheckedExpr -> m (Maybe (Quantifier, Provenance))
    go e = case e of
      Ann{}       -> normalisationError currentPass "Ann"
      Let{}       -> normalisationError currentPass "Let"
      Lam{}       -> normalisationError currentPass "Lam"
      Universe{}  -> typeError          currentPass "Universe"
      Pi{}        -> typeError          currentPass "Pi"
      Hole{}      -> visibilityError    currentPass "Hole"
      Meta{}      -> resolutionError    currentPass "Meta"

      Literal{}   -> return Nothing
      Builtin{}   -> return Nothing
      Var{}       -> return Nothing
      LVec{}      -> return Nothing

      ExistsRatExpr p _ body -> checkEqual p Exists =<< go body
      ForallRatExpr p _ body -> checkEqual p Forall =<< go body

      App _ann _fun args -> do
        xs <- traverse go (onlyExplicit args)
        return $ catMaybes xs !!? 0

    checkEqual :: Provenance
               -> Quantifier
               -> Maybe (Quantifier, Provenance)
               -> m (Maybe (Quantifier, Provenance))
    checkEqual p q = \case
      Just (q', _p)
        | q /= q' -> compilerDeveloperError $
          "Mixed quantifiers found while compiling property" <+> pretty ident <+>
          "to" <+> pretty target <+> "which should have been caught by" <+>
          "the polarity type system before reaching this point."
      _ -> return (Just (q, p))

currentPass :: Doc a
currentPass = "quantifier analysis"