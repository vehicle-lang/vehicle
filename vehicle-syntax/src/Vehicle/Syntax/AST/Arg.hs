{-# LANGUAGE CPP          #-}
{-# LANGUAGE InstanceSigs #-}

module Vehicle.Syntax.AST.Arg where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Binder (GenericBinder (Binder))
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance)
import Vehicle.Syntax.AST.Relevance (HasRelevance (..), Relevance (..))
import Vehicle.Syntax.AST.Visibility (HasVisibility (..), Visibility (..),
                                      isInstance)

#if nothunks
import NoThunks.Class (NoThunks)
#endif

--------------------------------------------------------------------------------
-- Function arguments

-- | An argument to a function, parameterised by the type of expression it
-- stores.
data GenericArg expr = Arg
  { -- | Has the argument been auto-inserted by the type-checker?
    argProvenance :: !Provenance,
    -- | The visibility of the argument
    argVisibility :: !Visibility,
    -- | The relevancy of the argument
    argRelevance  :: !Relevance,
    -- | The argument expression
    argExpr       :: !expr
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

#if nothunks
instance NoThunks expr => NoThunks (GenericArg expr)
#endif

instance NFData expr => NFData (GenericArg expr)

instance ToJSON expr => ToJSON (GenericArg expr)

instance FromJSON expr => FromJSON (GenericArg expr)

instance HasProvenance (GenericArg expr) where
  provenanceOf :: GenericArg expr -> Provenance
  provenanceOf = argProvenance

instance HasVisibility (GenericArg expr) where
  visibilityOf :: GenericArg expr -> Visibility
  visibilityOf = argVisibility

instance HasRelevance (GenericArg expr) where
  relevanceOf :: GenericArg expr -> Relevance
  relevanceOf = argRelevance

--------------------------------------------------------------------------------
-- Patterns

pattern ExplicitArg :: Provenance -> expr -> GenericArg expr
pattern ExplicitArg p e = Arg p Explicit Relevant e

pattern ImplicitArg :: Provenance -> expr -> GenericArg expr
pattern ImplicitArg p e = Arg p Implicit Relevant e

pattern IrrelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantImplicitArg p e = Arg p Implicit Irrelevant e

pattern InstanceArg :: Provenance -> expr -> GenericArg expr
pattern InstanceArg p e = Arg p Instance Relevant e

pattern IrrelevantInstanceArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantInstanceArg p e = Arg p Instance Irrelevant e

--------------------------------------------------------------------------------
-- Helper functions

pairArg :: (GenericArg a, b) -> GenericArg (a, b)
pairArg (Arg p v r x, y) = Arg p v r (x, y)

unpairArg :: GenericArg (a, b) -> (GenericArg a, b)
unpairArg (Arg p v r (x, y)) = (Arg p v r x, y)

replaceArgExpr :: expr1 -> GenericArg expr2 -> GenericArg expr1
replaceArgExpr e = fmap (const e)

traverseNonInstanceArgExpr ::
  Monad m =>
  (expr -> m expr) ->
  GenericArg expr ->
  m (GenericArg expr)
traverseNonInstanceArgExpr f arg
  | isInstance arg = return arg
  | otherwise = traverse f arg

argFromBinder :: GenericBinder binder expr -> expr -> GenericArg expr
argFromBinder (Binder p v r _ _) = Arg p v r
