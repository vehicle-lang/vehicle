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
data GenericBinder expr = Binder
  { -- | Location of the binder in the source file
    binderProvenance :: Provenance,
    -- | What form the binder should take when displayed
    binderDisplayForm :: BinderDisplayForm,
    -- | The visibility of the binder
    binderVisibility :: Visibility,
    -- | The relevancy of the binder
    binderRelevance :: Relevance,
    -- | The representation of the bound variable
    binderType :: expr
    -- The type of the bound variable
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericBinder expr)

instance (ToJSON expr) => ToJSON (GenericBinder expr)

instance (Serialize expr) => Serialize (GenericBinder expr)

instance HasProvenance (GenericBinder expr) where
  provenanceOf = binderProvenance

instance HasVisibility (GenericBinder expr) where
  visibilityOf = binderVisibility

instance HasRelevance (GenericBinder expr) where
  relevanceOf = binderRelevance

instance HasName (GenericBinder expr) (Maybe Name) where
  nameOf = nameOf . binderNamingForm

--------------------------------------------------------------------------------
-- Binders

pattern ExplicitBinder :: Provenance -> expr -> GenericBinder expr
pattern ExplicitBinder p t <- Binder p _ Explicit Relevant t

pattern ImplicitBinder :: Provenance -> expr -> GenericBinder expr
pattern ImplicitBinder p t <- Binder p _ Implicit {} Relevant t

pattern InstanceBinder :: Provenance -> expr -> GenericBinder expr
pattern InstanceBinder p t <- Binder p _ Instance {} Relevant t

pattern IrrelevantInstanceBinder :: Provenance -> expr -> GenericBinder expr
pattern IrrelevantInstanceBinder p t <- Binder p _ Instance {} Irrelevant t

--------------------------------------------------------------------------------
-- Helper functions

pairBinder :: (GenericBinder a, b) -> GenericBinder (a, b)
pairBinder (Binder p u v r x, y) = Binder p u v r (x, y)

unpairBinder :: GenericBinder (a, b) -> (GenericBinder a, b)
unpairBinder (Binder p u v r (x, y)) = (Binder p u v r x, y)

replaceBinderType ::
  expr1 ->
  GenericBinder expr2 ->
  GenericBinder expr1
replaceBinderType e = fmap (const e)

wantsToFold :: GenericBinder expr -> Bool
wantsToFold = foldingForm . binderDisplayForm

binderNamingForm :: GenericBinder expr -> BinderNamingForm
binderNamingForm = namingForm . binderDisplayForm
