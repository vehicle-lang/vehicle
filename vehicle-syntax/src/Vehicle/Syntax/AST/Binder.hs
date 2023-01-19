module Vehicle.Syntax.AST.Binder where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Name (HasName (..), Name)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance)
import Vehicle.Syntax.AST.Relevance (HasRelevance (..), Relevance (..))
import Vehicle.Syntax.AST.Visibility (HasVisibility (..), Visibility (..))

--------------------------------------------------------------------------------
-- Binder naming forms

-- | What form the binder's name appears in the user expression
data BinderNamingForm
  = -- | Both name and type appear (e.g. {x : A})
    NameAndType Name
  | -- | Only name appears (e.g. {x})
    OnlyName Name
  | -- | Only type appears (e.g. {{HasEq A}})
    OnlyType
  deriving (Eq, Show, Generic)

instance NFData BinderNamingForm

instance ToJSON BinderNamingForm

instance Serialize BinderNamingForm

instance Hashable BinderNamingForm where
  -- We deliberately ignore the binder naming form when hashing
  -- in order to be agnostic to the name the user provides.
  hashWithSalt d _ = d

instance HasName BinderNamingForm (Maybe Name) where
  nameOf = \case
    NameAndType name -> Just name
    OnlyName name -> Just name
    OnlyType -> Nothing

mapBindingNamingFormName :: (Name -> Name) -> BinderNamingForm -> BinderNamingForm
mapBindingNamingFormName f = \case
  NameAndType name -> NameAndType $ f name
  OnlyName name -> OnlyName name
  OnlyType -> OnlyType

--------------------------------------------------------------------------------
-- Binder folding form

-- | Indicates whether the binder should be folded into the previous binder
-- (if possible).
type BinderFoldingForm = Bool

--------------------------------------------------------------------------------
-- Binder form

data BinderDisplayForm = BinderDisplayForm
  { namingForm :: BinderNamingForm,
    foldingForm :: BinderFoldingForm
  }
  deriving (Eq, Show, Generic)

instance NFData BinderDisplayForm

instance ToJSON BinderDisplayForm

instance Hashable BinderDisplayForm

instance HasName BinderDisplayForm (Maybe Name) where
  nameOf = nameOf . namingForm

instance Serialize BinderDisplayForm

mapBinderFormName :: (Name -> Name) -> BinderDisplayForm -> BinderDisplayForm
mapBinderFormName f binderDisplayForm =
  BinderDisplayForm
    { namingForm = mapBindingNamingFormName f $ namingForm binderDisplayForm,
      foldingForm = foldingForm binderDisplayForm
    }

--------------------------------------------------------------------------------
-- Binders

-- | Binder for lambda and let expressions
--
-- The binder stores the optional type annotation in order to ensure
-- reversibility during delaboration, and that as the type annotation was
-- manually provided by the user it never needs to be updated after unification
-- and type-class resolution.
data GenericBinder binder expr = Binder
  { -- | Location of the binder in the source file
    binderProvenance :: Provenance,
    -- | What form the binder should take when displayed
    binderDisplayForm :: BinderDisplayForm,
    -- | The visibility of the binder
    binderVisibility :: Visibility,
    -- | The relevancy of the binder
    binderRelevance :: Relevance,
    -- | The representation of the bound variable
    binderRepresentation :: binder,
    binderType :: expr
    -- The type of the bound variable
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData expr) => NFData (GenericBinder binder expr)

instance (ToJSON binder, ToJSON expr) => ToJSON (GenericBinder binder expr)

instance (Serialize binder, Serialize expr) => Serialize (GenericBinder binder expr)

instance HasProvenance (GenericBinder binder expr) where
  provenanceOf = binderProvenance

instance HasVisibility (GenericBinder binder expr) where
  visibilityOf = binderVisibility

instance HasRelevance (GenericBinder binder expr) where
  relevanceOf = binderRelevance

instance HasName (GenericBinder binder expr) (Maybe Name) where
  nameOf = nameOf . binderNamingForm

--------------------------------------------------------------------------------
-- Binders

pattern ExplicitBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern ExplicitBinder p n t <- Binder p _ Explicit Relevant n t

pattern ImplicitBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern ImplicitBinder p n t <- Binder p _ Implicit {} Relevant n t

pattern InstanceBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern InstanceBinder p n t <- Binder p _ Instance {} Relevant n t

pattern IrrelevantInstanceBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern IrrelevantInstanceBinder p n t <- Binder p _ Instance {} Irrelevant n t

--------------------------------------------------------------------------------
-- Helper functions

pairBinder :: (GenericBinder binder a, b) -> GenericBinder binder (a, b)
pairBinder (Binder p u v r b x, y) = Binder p u v r b (x, y)

unpairBinder :: GenericBinder binder (a, b) -> (GenericBinder binder a, b)
unpairBinder (Binder p u v r b (x, y)) = (Binder p u v r b x, y)

unpairBinderRep :: GenericBinder (a, b) e -> (GenericBinder a e, b)
unpairBinderRep (Binder p u v r (x, y) t) = (Binder p u v r x t, y)

mapBinderRep :: (a -> b) -> GenericBinder a e -> GenericBinder b e
mapBinderRep f (Binder p u v r b t) = Binder p u v r (f b) t

replaceBinderRep :: b -> GenericBinder a e -> GenericBinder b e
replaceBinderRep b' = mapBinderRep (const b')

replaceBinderType ::
  expr1 ->
  GenericBinder binder expr2 ->
  GenericBinder binder expr1
replaceBinderType e = fmap (const e)

wantsToFold :: GenericBinder binder expr -> Bool
wantsToFold = foldingForm . binderDisplayForm

binderNamingForm :: GenericBinder binder expr -> BinderNamingForm
binderNamingForm = namingForm . binderDisplayForm
