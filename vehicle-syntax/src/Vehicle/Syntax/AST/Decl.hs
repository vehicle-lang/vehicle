module Vehicle.Syntax.AST.Decl where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Name
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Type (HasType (..))

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
      DefFunctionSort -- List of annotations.
      expr -- Type of the definition.
      expr -- Body of the definition.
  deriving (Show, Functor, Foldable, Traversable, Generic)

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

instance HasType (GenericDecl expr) expr where
  typeOf = \case
    DefAbstract _ _ _ t -> t
    DefFunction _ _ _ t _ -> t

bodyOf :: GenericDecl expr -> Maybe expr
bodyOf = \case
  DefFunction _ _ _ _ e -> Just e
  DefAbstract {} -> Nothing

abstractSortOf :: GenericDecl expr -> Maybe DefAbstractSort
abstractSortOf decl = case decl of
  DefAbstract _ _ sort _ -> Just sort
  DefFunction {} -> Nothing

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

mapIdentifier ::
  (Identifier -> Identifier) ->
  GenericDecl expr ->
  GenericDecl expr
mapIdentifier f = \case
  DefAbstract p n r t -> DefAbstract p (f n) r t
  DefFunction p n b t e -> DefFunction p (f n) b t e

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
  DefFunction _ _ anns _ _ -> isAnnotatedAsProperty anns

isAbstractDecl :: GenericDecl expr -> Bool
isAbstractDecl = \case
  DefAbstract {} -> True
  DefFunction {} -> False

--------------------------------------------------------------------------------
-- Abstract definition types options

data ParameterSort
  = Inferable
  | NonInferable
  deriving (Eq, Ord, Show, Generic)

instance NFData ParameterSort

instance Serialize ParameterSort

instance Hashable ParameterSort

instance Pretty ParameterSort where
  pretty = \case
    Inferable -> "(infer=True)"
    NonInferable -> ""

isInferable :: ParameterSort -> Bool
isInferable = \case
  Inferable -> True
  NonInferable -> False

data DefAbstractSort
  = NetworkDef
  | DatasetDef
  | ParameterDef ParameterSort
  | BuiltinDef
  deriving (Eq, Show, Generic)

instance NFData DefAbstractSort

instance Serialize DefAbstractSort

instance Pretty DefAbstractSort where
  pretty t =
    "@" <> case t of
      NetworkDef -> "network"
      DatasetDef -> "dataset"
      ParameterDef {} -> "parameter"
      BuiltinDef {} -> "postulate"

isExternalResourceSort :: DefAbstractSort -> Bool
isExternalResourceSort = \case
  NetworkDef -> True
  DatasetDef -> True
  ParameterDef parameterType -> parameterType == NonInferable
  BuiltinDef {} -> False

isExternalResourceDecl :: GenericDecl expr -> Bool
isExternalResourceDecl decl = maybe False isExternalResourceSort (abstractSortOf decl)

--------------------------------------------------------------------------------
-- Annotations options

-- | How many arguments the function declaration is expecting
-- on the LHS.
--
-- e.g.
--   f : Nat -> Nat -> Nat
--   f x = \y -> x + y
--
-- should have a value of 1.
type LHSBinderCount = Int

-- | Possible declaration modes for the `DefFunction` node.
data DefFunctionSort
  = -- | The function was declared using `type ... = ...` syntax
    TypeDecl LHSBinderCount
  | -- | The function was declared with `record X .... where` syntax.
    RecordDecl (Maybe RecordDeclAnnotation)
  | -- | The function was declared as a standard function
    FunctionDecl LHSBinderCount (Maybe FunctionDeclAnnotation)
  deriving (Eq, Show, Generic)

instance NFData DefFunctionSort

instance Serialize DefFunctionSort

-- | Possible annotations for records.
data RecordDeclAnnotation
  = -- | The record was annotated with @record
    AnnTensor
  | -- | The record was annotated with @instance
    AnnInstance
  | -- | The record was annotated with @typeclass
    AnnTypeClass
  deriving (Eq, Show, Generic)

instance NFData RecordDeclAnnotation

instance Serialize RecordDeclAnnotation

instance Pretty RecordDeclAnnotation where
  pretty = \case
    AnnTensor -> "@tensor"
    AnnInstance -> "@instance"
    AnnTypeClass -> "@typeclass"

-- | Possible annotations for ordinatary functions.
data FunctionDeclAnnotation
  = -- | The function was annotated with @property
    AnnProperty
  deriving (Eq, Show, Generic)

instance NFData FunctionDeclAnnotation

instance Serialize FunctionDeclAnnotation

instance Pretty FunctionDeclAnnotation where
  pretty = \case
    AnnProperty -> "@property"

isDeclaredAsRecord :: DefFunctionSort -> Bool
isDeclaredAsRecord = \case
  RecordDecl {} -> True
  _ -> False

isDeclaredAsType :: DefFunctionSort -> Bool
isDeclaredAsType = \case
  TypeDecl {} -> True
  _ -> False

isAnnotatedAsProperty :: DefFunctionSort -> Bool
isAnnotatedAsProperty = \case
  FunctionDecl _ (Just AnnProperty) -> True
  _ -> False

isAnnotatedAsTensor :: DefFunctionSort -> Bool
isAnnotatedAsTensor = \case
  RecordDecl (Just AnnTensor) -> True
  _ -> False

isAnnotatedAsInstance :: DefFunctionSort -> Bool
isAnnotatedAsInstance = \case
  RecordDecl (Just AnnInstance) -> True
  _ -> False
