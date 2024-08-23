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
  setVisibility r Arg {..} = Arg {argVisibility = r, ..}

instance HasRelevance (GenericArg expr) where
  relevanceOf = argRelevance
  setRelevance r Arg {..} = Arg {argRelevance = r, ..}

--------------------------------------------------------------------------------
-- Patterns

-- NOTE: these are all unidirectional pattern synonyms because we want to force
-- the user to consider all arguments when constructing them.

pattern ExplicitArg :: Provenance -> Relevance -> expr -> GenericArg expr
pattern ExplicitArg p r e <- Arg p Explicit r e

pattern RelevantExplicitArg :: Provenance -> expr -> GenericArg expr
pattern RelevantExplicitArg p e <- Arg p Explicit Relevant e

pattern ImplicitArg :: Provenance -> Relevance -> expr -> GenericArg expr
pattern ImplicitArg p r e <- Arg p Implicit {} r e

pattern IrrelevantExplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantExplicitArg p e <- Arg p Explicit Irrelevant e

pattern RelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern RelevantImplicitArg p e <- Arg p Implicit {} Relevant e

pattern IrrelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantImplicitArg p e <- Arg p Implicit {} Irrelevant e

pattern InstanceArg :: Provenance -> Relevance -> expr -> GenericArg expr
pattern InstanceArg p r e <- Arg p Instance {} r e

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

traverseExplicitArgExpr ::
  (Monad m) =>
  (expr -> m expr) ->
  GenericArg expr ->
  m (GenericArg expr)
traverseExplicitArgExpr f arg
  | isExplicit arg = traverse f arg
  | otherwise = return arg

argFromBinder :: GenericBinder expr -> expr -> GenericArg expr
argFromBinder (Binder p _ v r _) = Arg p v r

-- | Constructs an explicit relevant argument
explicit :: expr -> GenericArg expr
explicit = Arg mempty Explicit Relevant

-- | Constructs an implicit relevant argument marked as being inserted by
-- the compiler.
implicit :: expr -> GenericArg expr
implicit = Arg mempty (Implicit True) Relevant

--------------------------------------------------------------------------------
-- Args

traverseArgs ::
  (Monad m) =>
  (expr1 -> m expr2) ->
  [GenericArg expr1] ->
  m [GenericArg expr2]
traverseArgs f = traverse (traverse f)

mapArgs ::
  (expr1 -> expr2) ->
  [GenericArg expr1] ->
  [GenericArg expr2]
mapArgs f = fmap (fmap f)
