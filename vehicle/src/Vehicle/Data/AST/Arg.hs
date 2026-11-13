module Vehicle.Data.AST.Arg where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Data.AST.Binder
import Vehicle.Data.AST.Provenance
import Vehicle.Data.AST.Relevance
import Vehicle.Data.AST.Visibility

--------------------------------------------------------------------------------
-- Function arguments

-- | An argument to a function, parameterised by the type of expression it
-- stores.
data GenericArg expr = Arg
  { -- | The visibility of the argument
    argVisibility :: Visibility,
    -- | The relevancy of the argument
    argRelevance :: Relevance,
    -- | The argument expression
    argExpr :: expr
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericArg expr)

instance (Serialize expr) => Serialize (GenericArg expr)

instance (HasProvenance expr) => HasProvenance (GenericArg expr) where
  provenanceOf Arg {..} =
    expandByArgVisibility argVisibility (provenanceOf argExpr)

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

pattern ExplicitArg :: Relevance -> expr -> GenericArg expr
pattern ExplicitArg r e <- Arg Explicit r e

pattern RelevantExplicitArg :: expr -> GenericArg expr
pattern RelevantExplicitArg e <- Arg Explicit Relevant e

pattern ImplicitArg :: Relevance -> expr -> GenericArg expr
pattern ImplicitArg r e <- Arg Implicit {} r e

pattern IrrelevantExplicitArg :: expr -> GenericArg expr
pattern IrrelevantExplicitArg e <- Arg Explicit Irrelevant e

pattern RelevantImplicitArg :: expr -> GenericArg expr
pattern RelevantImplicitArg e <- Arg Implicit {} Relevant e

pattern IrrelevantImplicitArg :: expr -> GenericArg expr
pattern IrrelevantImplicitArg e <- Arg Implicit {} Irrelevant e

pattern InstanceArg :: Relevance -> expr -> GenericArg expr
pattern InstanceArg r e <- Arg Instance {} r e

pattern RelevantInstanceArg :: expr -> GenericArg expr
pattern RelevantInstanceArg e <- Arg Instance {} Relevant e

--------------------------------------------------------------------------------
-- Helper functions

pairArg :: (GenericArg a, b) -> GenericArg (a, b)
pairArg (Arg v r x, y) = Arg v r (x, y)

unpairArg :: GenericArg (a, b) -> (GenericArg a, b)
unpairArg (Arg v r (x, y)) = (Arg v r x, y)

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
argFromBinder (Binder _ v r _) = Arg v r

-- | Constructs a relevant explicit argument
explicit :: expr -> GenericArg expr
explicit = Arg Explicit Relevant

-- | Constructs an irrelevant explicit argument
explicitIrrelevant :: expr -> GenericArg expr
explicitIrrelevant = Arg Explicit Irrelevant

-- | Constructs a relevant implicit argument marked as being inserted by
-- the compiler.
implicit :: expr -> GenericArg expr
implicit = Arg (Implicit True) Relevant

-- | Constructs an irrelevant implicit argument marked as being inserted by
-- the compiler.
implicitIrrelevant :: expr -> GenericArg expr
implicitIrrelevant = Arg (Implicit True) Irrelevant

-- | Constructs an irrelevant instance argument marked as being inserted by
-- the compiler.
instanceIrrelevant :: expr -> GenericArg expr
instanceIrrelevant = Arg (Instance True) Irrelevant

-- | Constructs a relevant implicit argument marked as being inserted by
-- the compiler.
instanceArg :: expr -> GenericArg expr
instanceArg = Arg (Instance True) Relevant

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
