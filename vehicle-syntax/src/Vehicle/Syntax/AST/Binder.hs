module Vehicle.Syntax.AST.Binder where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Vehicle.Syntax.AST.Name
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Relevance
import Vehicle.Syntax.AST.Visibility

--------------------------------------------------------------------------------
-- Binders

-- | Binder for lambda and let expressions
--
-- The binder stores the optional type annotation in order to ensure
-- reversibility during delaboration, and that as the type annotation was
-- manually provided by the user it never needs to be updated after unification
-- and type-class resolution.
data GenericBinder binder expr = Binder
  { binderProvenance     :: Provenance
  , binderVisibility     :: Visibility
  -- ^ The visibility of the binder
  , binderRelevance      :: Relevance
  -- ^ The relevancy of the binder
  , binderRepresentation :: binder
  -- ^ The representation of the bound variable
  , binderType           :: expr
  -- The type of the bound variable
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData expr) => NFData (GenericBinder binder expr)

instance HasProvenance (GenericBinder binder expr) where
  provenanceOf = binderProvenance

instance HasVisibility (GenericBinder binder expr) where
  visibilityOf = binderVisibility

instance HasRelevance (GenericBinder binder expr) where
  relevanceOf = binderRelevance

instance HasName (GenericBinder binder expr) binder where
  nameOf = binderRepresentation

--------------------------------------------------------------------------------
-- Binders

pattern ExplicitBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern ExplicitBinder p n t = Binder p Explicit Relevant n t

pattern ImplicitBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern ImplicitBinder p n t = Binder p Implicit Relevant n t

pattern InstanceBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern InstanceBinder p n t = Binder p Instance Relevant  n t

pattern IrrelevantInstanceBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern IrrelevantInstanceBinder p n t = Binder p Instance Irrelevant  n t

--------------------------------------------------------------------------------
-- Helper functions

pairBinder :: (GenericBinder binder a, b) -> GenericBinder binder (a, b)
pairBinder (Binder p v r b x, y) = Binder p v r b (x, y)

unpairBinder :: GenericBinder binder (a, b) -> (GenericBinder binder a, b)
unpairBinder (Binder p v r b (x, y)) = (Binder p v r b x, y)

unpairBinderRep :: GenericBinder (a, b) e -> (GenericBinder a e, b)
unpairBinderRep (Binder p v r (x, y) t) = (Binder p v r x t, y)

mapBinderRep :: (a -> b) -> GenericBinder a e -> GenericBinder b e
mapBinderRep f (Binder p v r b t) = Binder p v r (f b) t

replaceBinderRep :: b -> GenericBinder a e -> GenericBinder b e
replaceBinderRep b' (Binder p v r _b t) = Binder p v r b' t

replaceBinderType :: expr1
                  -> GenericBinder binder expr2
                  -> GenericBinder binder expr1
replaceBinderType e = fmap (const e)
