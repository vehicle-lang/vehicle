
module Vehicle.Compile.QuantifierAnalysis where

import Data.Maybe (catMaybes)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Control.Monad.Except (MonadError(throwError))

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Backend.Prelude (OutputTarget)

-- | Checks that the quantifiers within the expression are homogeneous,
-- returning the quantifier. Defaults to returning `All` if the expression
-- contains no quantifiers.
checkQuantifiersAreHomogeneous :: MonadCompile m
                               => OutputTarget
                               -> Identifier
                               -> CheckedExpr
                               -> m Quantifier
checkQuantifiersAreHomogeneous target ident expr = maybe All fst <$> go expr
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
        let explicitArgs = filter isExplicit (NonEmpty.toList args)
        xs <- traverse go (fmap argExpr explicitArgs)
        return $ catMaybes xs !!? 0

currentPass :: Doc a
currentPass = "quantifier analysis"