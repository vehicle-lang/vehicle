module Vehicle.Syntax.AST.Arg where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Binder
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Relevance
import Vehicle.Syntax.AST.Visibility

--------------------------------------------------------------------------------
-- Function arguments

-- | An argument to a function, parameterised by the type of expression it
-- stores.
data GenericArg expr = Arg
  { -- | The location of the arg in the source file.
    argProvenance :: Provenance,
    -- | The visibility of the argument
    argVisibility :: Visibility,
    -- | The relevancy of the argument
    argRelevance :: Relevance,
    -- | The argument expression
    argExpr :: expr
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericArg expr)

instance (Serialize expr) => Serialize (GenericArg expr)

instance HasProvenance (GenericArg expr) where
  provenanceOf = argProvenance

instance HasVisibility (GenericArg expr) where
  visibilityOf = argVisibility

instance HasRelevance (GenericArg expr) where
  relevanceOf = argRelevance

--------------------------------------------------------------------------------
-- Patterns

-- NOTE: these are all unidirectional pattern synonyms because we want to force
-- the user to consider all arguments when constructing them.

pattern ExplicitArg :: Provenance -> Relevance -> expr -> GenericArg expr
pattern ExplicitArg p r e <- Arg p Explicit r e

pattern RelevantExplicitArg :: Provenance -> expr -> GenericArg expr
pattern RelevantExplicitArg p e <- Arg p Explicit Relevant e

pattern IrrelevantExplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantExplicitArg p e <- Arg p Explicit Irrelevant e

pattern RelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern RelevantImplicitArg p e <- Arg p Implicit {} Relevant e

pattern IrrelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantImplicitArg p e <- Arg p Implicit {} Irrelevant e

pattern InstanceArg :: Provenance -> Relevance -> expr -> GenericArg expr
pattern InstanceArg p r e <- Arg p (Instance True) r e

pattern RelevantInstanceArg :: Provenance -> expr -> GenericArg expr
pattern RelevantInstanceArg p e <- Arg p Instance {} Relevant e

--------------------------------------------------------------------------------
-- Helper functions

pairArg :: (GenericArg a, b) -> GenericArg (a, b)
pairArg (Arg p v r x, y) = Arg p v r (x, y)

unpairArg :: GenericArg (a, b) -> (GenericArg a, b)
unpairArg (Arg p v r (x, y)) = (Arg p v r x, y)

replaceArgExpr :: expr1 -> GenericArg expr2 -> GenericArg expr1
replaceArgExpr e = fmap (const e)

traverseNonInstanceArgExpr ::
  (Monad m) =>
  (expr -> m expr) ->
  GenericArg expr ->
  m (GenericArg expr)
traverseNonInstanceArgExpr f arg
  | isInstance arg = return arg
  | otherwise = traverse f arg

argFromBinder :: GenericBinder expr -> expr -> GenericArg expr
argFromBinder (Binder p i v r _) = Arg p v r
