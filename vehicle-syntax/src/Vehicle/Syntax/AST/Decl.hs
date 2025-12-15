module Vehicle.Syntax.AST.Decl where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Binder (GenericTelescope)
import Vehicle.Syntax.AST.Name
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Record (GenericRecordFields)

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
  | -- | Function definitions with a body
    DefRecord
      Provenance -- Location in source file.
      Identifier -- Name of definition.
      (Maybe DefRecordSort) -- List of annotations.
      (GenericTelescope expr) -- Type parameters.
      (GenericRecordFields expr) -- Fields.
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericDecl expr)

instance (Serialize expr) => Serialize (GenericDecl expr)

instance HasProvenance (GenericDecl expr) where
  provenanceOf = \case
    DefAbstract p _ _ _ -> p
    DefFunction p _ _ _ _ -> p
    DefRecord p _ _ _ _ -> p

instance HasIdentifier (GenericDecl expr) where
  identifierOf = \case
    DefAbstract _ i _ _ -> i
    DefFunction _ i _ _ _ -> i
    DefRecord _ i _ _ _ -> i

instance HasName (GenericDecl expr) Name where
  nameOf = nameOf . identifierOf

mapIdentifier ::
  (Identifier -> Identifier) ->
  GenericDecl expr ->
  GenericDecl expr
mapIdentifier f = \case
  DefAbstract p n r t -> DefAbstract p (f n) r t
  DefFunction p n b t e -> DefFunction p (f n) b t e
  DefRecord p n b t e -> DefRecord p (f n) b t e

isPropertyDecl :: GenericDecl expr -> Bool
isPropertyDecl = \case
  DefAbstract {} -> False
  DefFunction _ _ anns _ _ -> isAnnotatedAsProperty anns
  DefRecord {} -> False

isTypeDecl :: GenericDecl expr -> Bool
isTypeDecl = \case
  DefAbstract {} -> False
  DefFunction _ _ anns _ _ -> isDeclaredAsType anns
  DefRecord {} -> False

isAbstractDecl :: GenericDecl expr -> Bool
isAbstractDecl = \case
  DefAbstract {} -> True
  DefFunction {} -> False
  DefRecord {} -> False

isExternalResourceDecl :: GenericDecl expr -> Bool
isExternalResourceDecl = \case
  DefAbstract _ _ sort _ -> isExternalResourceSort sort
  DefFunction {} -> False
  DefRecord {} -> False

--------------------------------------------------------------------------------
-- DefAbstract

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

--------------------------------------------------------------------------------
-- DefFunction

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
  | -- | The function was declared as a standard function
    FunctionDecl LHSBinderCount (Maybe FunctionDeclAnnotation)
  | -- | The function was generated as a projection from a record
    ProjectionDecl LHSBinderCount
  deriving (Eq, Show, Generic)

instance NFData DefFunctionSort

instance Serialize DefFunctionSort

-- | Possible annotations for ordinatary functions.
data FunctionDeclAnnotation
  = -- | The function was annotated with @property
    AnnProperty
  | -- | The function was annotated with @instance
    AnnInstance
  deriving (Eq, Show, Generic)

instance NFData FunctionDeclAnnotation

instance Serialize FunctionDeclAnnotation

instance Pretty FunctionDeclAnnotation where
  pretty = \case
    AnnProperty -> "@property"
    AnnInstance -> "@instance"

isDeclaredAsType :: DefFunctionSort -> Bool
isDeclaredAsType = \case
  TypeDecl {} -> True
  _ -> False

isAnnotatedAsProperty :: DefFunctionSort -> Bool
isAnnotatedAsProperty = \case
  FunctionDecl _ (Just AnnProperty) -> True
  _ -> False

isAnnotatedAsInstance :: DefFunctionSort -> Bool
isAnnotatedAsInstance = \case
  FunctionDecl _ (Just AnnInstance) -> True
  _ -> False

--------------------------------------------------------------------------------
-- DefRecord

-- | Possible annotations for records.
data DefRecordSort
  = -- | The record definition was annotated with @record
    AnnTensor
  | -- | The record definition was annotated with @typeclass
    AnnTypeClass
  deriving (Eq, Show, Generic)

instance NFData DefRecordSort

instance Serialize DefRecordSort

instance Pretty DefRecordSort where
  pretty = \case
    AnnTensor -> "@tensor"
    AnnTypeClass -> "@typeclass"

isAnnotatedAsTensor :: Maybe DefRecordSort -> Bool
isAnnotatedAsTensor = \case
  Just AnnTensor -> True
  _ -> False

isAnnotatedAsTypeClass :: Maybe DefRecordSort -> Bool
isAnnotatedAsTypeClass = \case
  Just AnnTypeClass -> True
  _ -> False
