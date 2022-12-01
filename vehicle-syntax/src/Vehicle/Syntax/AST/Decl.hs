module Vehicle.Syntax.AST.Decl where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Data.Text (Text)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Name (HasIdentifier (..), Identifier)
import Vehicle.Syntax.AST.Provenance
import Data.Aeson (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Declarations

-- | Type of top-level declarations.
data GenericDecl expr
  = DefResource
    Provenance             -- Location in source file.
    Resource           -- Type of resource.
    Identifier             -- Name of resource.
    expr                   -- Vehicle type of the resource.

  | DefFunction
    Provenance             -- Location in source file.
    Identifier             -- Bound function name.
    expr                   -- Bound function type.
    expr                   -- Bound function body.

  | DefPostulate
    Provenance
    Identifier
    expr
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData   expr => NFData   (GenericDecl expr)
instance ToJSON   expr => ToJSON   (GenericDecl expr)
instance FromJSON expr => FromJSON (GenericDecl expr)

instance HasProvenance (GenericDecl expr) where
  provenanceOf = \case
    DefResource p _ _ _  -> p
    DefFunction p _  _ _ -> p
    DefPostulate p _ _   -> p

instance HasIdentifier (GenericDecl expr) where
  identifierOf = \case
    DefResource  _ _ i _  -> i
    DefFunction  _  i _ _ -> i
    DefPostulate _ i _    -> i

bodyOf :: GenericDecl expr -> Maybe expr
bodyOf = \case
  DefResource{}       -> Nothing
  DefFunction _ _ _ e -> Just e
  DefPostulate{}      -> Nothing

traverseDeclType :: Monad m => (expr -> m expr) -> GenericDecl expr -> m (GenericDecl expr)
traverseDeclType f = \case
  DefResource p r n t -> DefResource p r n <$> f t
  DefFunction p n t e -> DefFunction p n <$> f t <*> pure e
  DefPostulate p n t  -> DefPostulate p n <$> f t

--------------------------------------------------------------------------------
-- Annotations options

pattern InferableOption :: Text
pattern InferableOption = "infer"

data Annotation
  = PropertyAnnotation
  | ResourceAnnotation Resource

instance Pretty Annotation where
  pretty annotation = "@" <> case annotation of
    PropertyAnnotation          -> "property"
    ResourceAnnotation resource -> pretty resource


--------------------------------------------------------------------------------
-- The different types of resources supported

data Resource
  = Network
  | Dataset
  | Parameter
  | InferableParameter
  deriving (Eq, Show, Generic)

instance NFData   Resource
instance ToJSON   Resource
instance FromJSON Resource

instance Pretty Resource where
  pretty = \case
    Network            -> "network"
    Dataset            -> "dataset"
    Parameter          -> "parameter"
    InferableParameter -> "inferable parameter"
