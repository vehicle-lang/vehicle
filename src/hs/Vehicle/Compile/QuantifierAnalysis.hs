
module Vehicle.Compile.QuantifierAnalysis
  ( checkQuantifiersAndNegateIfNecessary
  ) where

import Data.Maybe (catMaybes)
import Control.Monad.Except (MonadError(throwError))

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Backend.Prelude (Backend)
import Vehicle.Language.Print (prettyFriendly)

checkQuantifiersAndNegateIfNecessary :: MonadCompile m
                                     => Backend
                                     -> Identifier
                                     -> CheckedExpr
                                     -> m (Bool, CheckedExpr)
checkQuantifiersAndNegateIfNecessary backend ident expr =
  logCompilerPass "quantifier analysis" $ do
    quantifier <- checkQuantifiersAreHomogeneous backend ident expr
    logDebug MinDetail $ "Quantifier type: " <> pretty (Quant quantifier)

    outputExpr <- case quantifier of
      Exists  -> return expr
      Forall  -> do
        -- If the property is universally quantified then we need to negate the expression
        logDebug MinDetail "Negating property..."
        let ann = provenanceOf expr
        return $ NotExpr ann [ExplicitArg ann expr]

    logCompilerPassOutput (prettyFriendly outputExpr)
    return (quantifier == Forall, outputExpr)

-- | Checks that the quantifiers within the expression are homogeneous,
-- returning the quantifier. Defaults to returning `All` if the expression
-- contains no quantifiers.
checkQuantifiersAreHomogeneous :: MonadCompile m
                               => Backend
                               -> Identifier
                               -> CheckedExpr
                               -> m Quantifier
checkQuantifiersAreHomogeneous target ident expr = maybe Forall fst <$> go expr
  where
    go :: MonadCompile m => CheckedExpr -> m (Maybe (Quantifier, Provenance))
    go e = case e of
      Ann{}       -> normalisationError currentPass "Ann"
      Let{}       -> normalisationError currentPass "Let"
      Lam{}       -> normalisationError currentPass "Lam"
      Type{}      -> typeError          currentPass "Type"
      Pi{}        -> typeError          currentPass "Pi"
      PrimDict{}  -> visibilityError    currentPass "PrimDict"
      Hole{}      -> visibilityError    currentPass "Hole"
      Meta{}      -> resolutionError    currentPass "Meta"

      Literal{}   -> return Nothing
      Builtin{}   -> return Nothing
      Var{}       -> return Nothing
      LSeq{}      -> return Nothing

      QuantifierExpr q ann _ body -> do
        recResult <- go body
        case recResult of
          Just (q', p)
            | q /= q' ->
              throwError $ UnsupportedQuantifierSequence target p ident q'
          _ -> return (Just (q, provenanceOf ann))

      App _ann _fun args -> do
        xs <- traverse go (onlyExplicit args)
        return $ catMaybes xs !!? 0

currentPass :: Doc a
currentPass = "quantifier analysis"