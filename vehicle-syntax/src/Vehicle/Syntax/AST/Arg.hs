module Vehicle.Syntax.AST.Arg where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

import Vehicle.Syntax.AST.Binder
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Relevance
import Vehicle.Syntax.AST.Visibility

--------------------------------------------------------------------------------
-- Function arguments

-- | An argument to a function, parameterised by the type of expression it
-- stores.
data GenericArg expr = Arg
  { argProvenance :: !Provenance
    -- ^ Has the argument been auto-inserted by the type-checker?
  , argVisibility :: !Visibility
    -- ^ The visibility of the argument
  , argRelevance  :: !Relevance
    -- ^ The relevancy of the argument
  , argExpr       :: !expr
    -- ^ The argument expression
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NoThunks)

instance NFData   expr => NFData   (GenericArg expr)
instance ToJSON   expr => ToJSON   (GenericArg expr)
instance FromJSON expr => FromJSON (GenericArg expr)

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

traverseExplicitArgExpr :: Monad m
                        => (expr -> m expr)
                        -> GenericArg expr
                        -> m (GenericArg expr)
traverseExplicitArgExpr f (ExplicitArg i e) = ExplicitArg i <$> f e
traverseExplicitArgExpr _ arg               = return arg

argFromBinder :: GenericBinder binder expr -> expr -> GenericArg expr
argFromBinder (Binder p v r _ _) = Arg p v r
