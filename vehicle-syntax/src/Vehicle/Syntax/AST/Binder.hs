{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Vehicle.Syntax.AST.Binder where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Name (HasName (..), Name)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance, fillInProvenance)
import Vehicle.Syntax.AST.Relevance (HasRelevance (..), Relevance (..))
import Vehicle.Syntax.AST.Type
import Vehicle.Syntax.AST.Visibility (HasVisibility (..), Visibility (..), expandByArgVisibility)

--------------------------------------------------------------------------------
-- Binder naming forms

-- | What form the binder's name appears in the user expression
data BinderNamingForm
  = -- | Both name and type appear (e.g. {x : A})
    NameAndType Name Provenance
  | -- | Only name appears (e.g. {x})
    OnlyName Name Provenance
  | -- | Only type appears (e.g. {{HasEq A}})
    OnlyType
  deriving (Eq, Ord, Show, Generic)

instance NFData BinderNamingForm

instance Serialize BinderNamingForm

instance Hashable BinderNamingForm where
  -- We deliberately ignore the binder naming form when hashing
  -- in order to be agnostic to the name the user provides.
  hashWithSalt d _ = d

instance HasName BinderNamingForm (Maybe Name) where
  nameOf = \case
    NameAndType name _ -> Just name
    OnlyName name _ -> Just name
    OnlyType -> Nothing

mapBindingNamingFormName :: (Name -> Name) -> BinderNamingForm -> BinderNamingForm
mapBindingNamingFormName f = \case
  NameAndType name p -> NameAndType (f name) p
  OnlyName name p -> OnlyName name p
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
  deriving (Eq, Ord, Show, Generic)

instance NFData BinderDisplayForm

instance Hashable BinderDisplayForm

instance HasName BinderDisplayForm (Maybe Name) where
  nameOf = nameOf . namingForm

instance Serialize BinderDisplayForm

--------------------------------------------------------------------------------
-- Binders

-- | Binder for introducing new variables in lambda and let expressions
--
-- The binder stores the optional type annotation in order to ensure
-- reversibility during delaboration, and that as the type annotation was
-- manually provided by the user it never needs to be updated after unification
-- and type-class resolution.
data GenericBinder expr = Binder
  { -- | What form the binder should take when displayed
    binderDisplayForm :: BinderDisplayForm,
    -- | The visibility of the binder
    binderVisibility :: Visibility,
    -- | The relevancy of the binder
    binderRelevance :: Relevance,
    -- | The value associated with the bound variable.
    -- Usually (but not always) its type.
    binderValue :: expr
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericBinder expr)

instance (Serialize expr) => Serialize (GenericBinder expr)

instance (HasProvenance expr) => HasProvenance (GenericBinder expr) where
  provenanceOf Binder {..} = do
    let typeProv = provenanceOf binderValue
    let nameAndTypeProv = case namingForm binderDisplayForm of
          NameAndType _n p -> fillInProvenance (p :| [typeProv])
          OnlyName _n p -> fillInProvenance (p :| [typeProv])
          OnlyType -> typeProv
    expandByArgVisibility binderVisibility nameAndTypeProv

instance HasVisibility (GenericBinder expr) where
  visibilityOf :: GenericBinder expr -> Visibility
  visibilityOf = binderVisibility
  setVisibility r Binder {..} = Binder {binderVisibility = r, ..}

instance HasRelevance (GenericBinder expr) where
  relevanceOf = binderRelevance
  setRelevance r Binder {..} = Binder {binderRelevance = r, ..}

instance HasName (GenericBinder expr) (Maybe Name) where
  nameOf = nameOf . binderNamingForm

instance HasType (GenericBinder expr) expr where
  typeOf = binderValue

--------------------------------------------------------------------------------
-- Pattern synonyms for binders

pattern ExplicitBinder :: expr -> GenericBinder expr
pattern ExplicitBinder t <- Binder _ Explicit Relevant t

pattern ImplicitBinder :: expr -> GenericBinder expr
pattern ImplicitBinder t <- Binder _ Implicit {} Relevant t

pattern InstanceBinder :: expr -> GenericBinder expr
pattern InstanceBinder t <- Binder _ Instance {} Relevant t

pattern IrrelevantInstanceBinder :: expr -> GenericBinder expr
pattern IrrelevantInstanceBinder t <- Binder _ Instance {} Irrelevant t

--------------------------------------------------------------------------------
-- Helper functions

pairBinder :: (GenericBinder a, b) -> GenericBinder (a, b)
pairBinder (Binder u v r x, y) = Binder u v r (x, y)

unpairBinder :: GenericBinder (a, b) -> (GenericBinder a, b)
unpairBinder (Binder u v r (x, y)) = (Binder u v r x, y)

replaceBinderType ::
  expr1 ->
  GenericBinder expr2 ->
  GenericBinder expr1
replaceBinderType e = fmap (const e)

wantsToFold :: GenericBinder expr -> Bool
wantsToFold = foldingForm . binderDisplayForm

binderNamingForm :: GenericBinder expr -> BinderNamingForm
binderNamingForm = namingForm . binderDisplayForm

setBinderRelevance :: GenericBinder expr -> Relevance -> GenericBinder expr
setBinderRelevance (Binder u v _r x) r = Binder u v r x

mapBinderNamingForm :: (BinderNamingForm -> BinderNamingForm) -> GenericBinder expr -> GenericBinder expr
mapBinderNamingForm f Binder {..} =
  Binder
    { binderDisplayForm =
        BinderDisplayForm
          { namingForm = f $ namingForm binderDisplayForm,
            foldingForm = foldingForm binderDisplayForm
          },
      ..
    }

mkDefaultBinderDisplayForm :: Maybe (Provenance, Name) -> BinderDisplayForm
mkDefaultBinderDisplayForm = \case
  Just (p, name) -> BinderDisplayForm (OnlyName name p) True
  Nothing -> BinderDisplayForm OnlyType True

mkExplicitBinder :: expr -> Maybe (Provenance, Name) -> GenericBinder expr
mkExplicitBinder typ name = Binder (mkDefaultBinderDisplayForm name) Explicit Relevant typ

mkImplicitBinder :: expr -> Maybe (Provenance, Name) -> GenericBinder expr
mkImplicitBinder typ name = Binder (mkDefaultBinderDisplayForm name) (Implicit True) Relevant typ

--------------------------------------------------------------------------------
-- Telescope

type GenericTelescope expr = [GenericBinder expr]
