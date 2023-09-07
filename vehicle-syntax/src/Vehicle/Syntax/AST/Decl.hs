module Vehicle.Syntax.AST.Decl where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Name
import Vehicle.Syntax.AST.Provenance

--------------------------------------------------------------------------------
-- Declarations

-- | Type of top-level declarations.
data GenericDecl expr
  = -- | Abstract definitions that require no body
    DefAbstract
      Provenance -- Location in source file.
      Identifier -- Name of definition.
      DefAbstractSort -- The sort of abstract definition.
      expr -- Type of the definition.
  | -- | Function definitions with a body
    DefFunction
      Provenance -- Location in source file.
      Identifier -- Name of definition.
      [Annotation] -- List of annotations.
      expr -- Type of the definition.
      expr -- Body of the definition.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericDecl expr)

instance (Serialize expr) => Serialize (GenericDecl expr)

instance HasProvenance (GenericDecl expr) where
  provenanceOf = \case
    DefAbstract p _ _ _ -> p
    DefFunction p _ _ _ _ -> p

instance HasIdentifier (GenericDecl expr) where
  identifierOf = \case
    DefAbstract _ i _ _ -> i
    DefFunction _ i _ _ _ -> i

instance HasName (GenericDecl expr) Name where
  nameOf = nameOf . identifierOf

bodyOf :: GenericDecl expr -> Maybe expr
bodyOf = \case
  DefFunction _ _ _ _ e -> Just e
  DefAbstract {} -> Nothing

annotationsOf :: GenericDecl expr -> [Annotation]
annotationsOf = \case
  DefFunction _ _ anns _ e -> anns
  DefAbstract {} -> []

-- | Traverses the type and body of a declaration using the first and
-- second provided functions respectively.
-- Use |traverse| if you want to traverse them using the same function.
traverseDeclTypeAndExpr ::
  (Monad m) =>
  (expr1 -> m expr2) ->
  (expr1 -> m expr2) ->
  GenericDecl expr1 ->
  m (GenericDecl expr2)
traverseDeclTypeAndExpr f1 f2 = \case
  DefAbstract p n r t -> DefAbstract p n r <$> f1 t
  DefFunction p n b t e -> DefFunction p n b <$> f1 t <*> f2 e

-- | Traverses the type of the declaration.
traverseDeclType ::
  (Monad m) =>
  (expr -> m expr) ->
  GenericDecl expr ->
  m (GenericDecl expr)
traverseDeclType f = traverseDeclTypeAndExpr f return

isPropertyDecl :: GenericDecl expr -> Bool
isPropertyDecl = \case
  DefAbstract {} -> False
  DefFunction _ _ anns _ _ -> AnnProperty `elem` anns

--------------------------------------------------------------------------------
-- Abstract definition types options

data ParameterSort
  = Inferable
  | NonInferable
  deriving (Eq, Show, Generic)

instance NFData ParameterSort

instance Serialize ParameterSort

instance Pretty ParameterSort where
  pretty = \case
    Inferable -> "(infer=True)"
    NonInferable -> ""

data DefAbstractSort
  = NetworkDef
  | DatasetDef
  | ParameterDef ParameterSort
  | PostulateDef
  deriving (Eq, Show, Generic)

instance NFData DefAbstractSort

instance Serialize DefAbstractSort

instance Pretty DefAbstractSort where
  pretty t =
    "@" <> case t of
      NetworkDef -> "network"
      DatasetDef -> "dataset"
      ParameterDef paramTyp -> "parameter"
      PostulateDef -> "property"

isExternalResourceSort :: DefAbstractSort -> Bool
isExternalResourceSort = \case
  NetworkDef -> True
  DatasetDef -> True
  ParameterDef {} -> True
  PostulateDef -> False

--------------------------------------------------------------------------------
-- Annotations options

data Annotation
  = AnnProperty
  deriving (Eq, Show, Generic)

instance NFData Annotation

instance Serialize Annotation

instance Pretty Annotation where
  pretty = \case
    AnnProperty -> "@property"

isProperty :: [Annotation] -> Bool
isProperty anns = AnnProperty `elem` anns
