module Vehicle.Syntax.AST.Arg where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
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

instance (ToJSON expr) => ToJSON (GenericArg expr)

instance (Serialize expr) => Serialize (GenericArg expr)

instance HasProvenance (GenericArg expr) where
  provenanceOf = argProvenance

instance HasVisibility (GenericArg expr) where
  visibilityOf = argVisibility

instance HasRelevance (GenericArg expr) where
  relevanceOf = argRelevance

--------------------------------------------------------------------------------
-- Patterns

pattern ExplicitArg :: Provenance -> expr -> GenericArg expr
pattern ExplicitArg p e = Arg p Explicit Relevant e

pattern ImplicitArg :: Provenance -> expr -> GenericArg expr
pattern ImplicitArg p e <- Arg p Implicit {} Relevant e
  where
    ImplicitArg p e = Arg p (Implicit True) Relevant e

pattern IrrelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantImplicitArg p e <- Arg p Implicit {} Irrelevant e
  where
    IrrelevantImplicitArg p e = Arg p (Implicit True) Irrelevant e

pattern InstanceArg :: Provenance -> expr -> GenericArg expr
pattern InstanceArg p e <- Arg p Instance {} Relevant e
  where
    InstanceArg p e = Arg p (Instance True) Relevant e

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
