module Vehicle.Syntax.AST.Decl where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Name (HasIdentifier (..), Identifier)
import Vehicle.Syntax.AST.Provenance

--------------------------------------------------------------------------------
-- Declarations

-- | Type of top-level declarations.
data GenericDecl expr
  = DefResource
      Provenance -- Location in source file.
      Identifier -- Name of resource.
      Resource -- Type of resource.
      expr -- Vehicle type of the resource.
  | DefFunction
      Provenance -- Location in source file.
      Identifier -- Bound function name.
      Bool -- Is it a property.
      expr -- Bound function type.
      expr -- Bound function body.
  | DefPostulate
      Provenance
      Identifier
      expr
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData expr => NFData (GenericDecl expr)

instance ToJSON expr => ToJSON (GenericDecl expr)

instance Serialize expr => Serialize (GenericDecl expr)

instance HasProvenance (GenericDecl expr) where
  provenanceOf = \case
    DefResource p _ _ _ -> p
    DefFunction p _ _ _ _ -> p
    DefPostulate p _ _ -> p

instance HasIdentifier (GenericDecl expr) where
  identifierOf = \case
    DefResource _ i _ _ -> i
    DefFunction _ i _ _ _ -> i
    DefPostulate _ i _ -> i

bodyOf :: GenericDecl expr -> Maybe expr
bodyOf = \case
  DefFunction _ _ _ _ e -> Just e
  DefResource {} -> Nothing
  DefPostulate {} -> Nothing

-- | Traverses the type and body of a declaration using the first and
-- second provided functions respectively.
-- Use |traverse| if you want to traverse them using the same function.
traverseDeclTypeAndExpr ::
  Monad m =>
  (expr1 -> m expr2) ->
  (expr1 -> m expr2) ->
  GenericDecl expr1 ->
  m (GenericDecl expr2)
traverseDeclTypeAndExpr f1 f2 = \case
  DefResource p n r t -> DefResource p n r <$> f1 t
  DefFunction p n b t e -> DefFunction p n b <$> f1 t <*> f2 e
  DefPostulate p n t -> DefPostulate p n <$> f1 t

-- | Traverses the type of the declaration.
traverseDeclType ::
  Monad m =>
  (expr -> m expr) ->
  GenericDecl expr ->
  m (GenericDecl expr)
traverseDeclType f = traverseDeclTypeAndExpr f return

--------------------------------------------------------------------------------
-- Annotations options

pattern InferableOption :: Text
pattern InferableOption = "infer"

data Annotation
  = PropertyAnnotation
  | ResourceAnnotation Resource

instance Pretty Annotation where
  pretty annotation =
    "@" <> case annotation of
      PropertyAnnotation -> "property"
      ResourceAnnotation resource -> pretty resource

--------------------------------------------------------------------------------
-- The different types of resources supported

data Resource
  = Network
  | Dataset
  | Parameter
  | InferableParameter
  deriving (Eq, Show, Generic)

instance NFData Resource

instance ToJSON Resource

instance FromJSON Resource

instance Serialize Resource

instance Pretty Resource where
  pretty = \case
    Network -> "network"
    Dataset -> "dataset"
    Parameter -> "parameter"
    InferableParameter -> "inferable parameter"
