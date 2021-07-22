
module Vehicle.Backend.ITP.Core where

import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader, MonadReader(..), ReaderT(..))
import Data.Text as Text (Text, intercalate, pack, append)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Version (Version, showVersion)
import Prettyprinter as Pretty hiding (squotes)

import Vehicle.Frontend.AST
import Vehicle.Frontend.Print ()
import Vehicle.Prelude
import Vehicle.Error

-- * Utilities when compiling to an interactive theorem prover backend

--------------------------------------------------------------------------------
-- Backends

data Backend
  = Agda

instance Pretty Backend where
  pretty Agda = "Agda"

--------------------------------------------------------------------------------
-- Options

-- |The options that are specificable by the user when compiling to an ITP
-- backend
data ITPOptions backendOpts = ITPOptions
  { vehicleUIDs  :: Map Text Text
  , aisecVersion :: Version
  , backendOpts  :: backendOpts
  }

-- |Generate the file header given the token used to start comments in the
-- target language
fileHeader :: ITPOptions backendOpts -> Text -> Text
fileHeader options commentToken = intercalate "\n" $
  map (append (commentToken <> " "))
    [ "This file was generated automatically by the AISEC tool"
    , "and should not be modified manually!"
    , "Metadata"
    , " - AISEC version: " <> pack (showVersion (aisecVersion options))
    , " - Date generated: ???"
    ]

--------------------------------------------------------------------------------
-- AST abbreviations

annotatedType :: OutputAnn 'EXPR -> OutputExpr
annotatedType (t :*: _) = unInfo t

--------------------------------------------------------------------------------
-- Control

-- |Constraint for the monad stack used by the Compiler.
type MonadCompile m options =
  (MonadError CompileError m, MonadReader (ITPOptions options) m)

type Compile a options = ReaderT (ITPOptions options) (Except CompileError) a

-- * Type of errors that can be thrown during compilation
data CompileError
  = CompilationUnsupported Provenance Symbol
  | TensorIndexOutOfBounds Provenance OutputExpr Integer

instance MeaningfulError CompileError where
  details (CompilationUnsupported p symbol) = UError $ UserError
    { provenance = p
    , problem    = "compilation of" <+> squotes symbol <+> "is not supported"
    , fix        = "see user manual for details"
    }

  details (TensorIndexOutOfBounds p tensorTyp index) = UError $ UserError
    { provenance = p
    , problem    = "index" <+> pretty index <+> "is larger than the first dimension of the type" <+> pretty tensorTyp
    , fix        = "check your indexing"
    }

unexpectedType :: Provenance -> OutputExpr -> OutputExpr -> [OutputExpr] -> a
unexpectedType p expr actualType expectedTypes = developerError $
  "unexpected type found for expression" <+> pretty expr <> "." <> line <>
  "Was expecting one of" <+> list (map pretty expectedTypes) <+>
  "but found" <+> pretty actualType

unexpectedExpr :: Provenance -> OutputExpr -> [OutputExpr] -> a
unexpectedExpr p actualExpr expectedExprs = developerError $
  "Was expecting something of the form" <+> list (map pretty expectedExprs) <+>
  "but found" <+> pretty actualExpr <> "."

--------------------------------------------------------------------------------
-- Subcategories of types/expressions

-- * Types of numeric data supported
data NumericType
  = Int
  | Real

numericType :: MonadCompile m options => OutputExpr -> m NumericType
numericType expr = go $ annotatedType (annotation expr)
  where
    go :: MonadCompile m options => OutputExpr -> m NumericType
    go = \case
      Int  _ann        -> return Int
      Real _ann        -> return Real
      Fun  _ann _t1 t2 -> go t2
      typ               -> throwError $ UnexpectedType (prov expr) expr typ ["Real", "Int", "X -> Bool", "X -> Prop"]

truthType :: MonadCompile m options => OutputExpr -> m PrimitiveTruth
truthType expr = go (annotatedType (annotation expr))
  where
    go :: MonadCompile m options => OutputExpr -> m PrimitiveTruth
    go = \case
      Bool _ann        -> return Bool
      Prop _ann        -> return Prop
      Fun  _ann _t1 t2 -> go t2
      typ              -> throwError $ UnexpectedType (prov expr) expr typ ["Bool", "Prop", "X -> Bool", "X -> Prop"]

containerType :: MonadCompile m options => OutputExpr -> m PrimitiveContainer
containerType expr = let ann = annotation expr in case annotatedType ann of
  List   _ _            -> return List
  Tensor _ _ (Seq _ ds) -> Tensor <$> traverse fromLit ds
    where
      fromLit :: MonadCompile m options => OutputExpr -> m Integer
      fromLit (LitInt _ i) = return i
      fromLit t            = throwError $ UnexpectedType (prov ann) expr t ["a literal"]
  t              -> throwError $ UnexpectedType (prov ann) expr t ["List", "Tensor"]

-- |Types of numeric orders
data OrderType
  = Leq
  | Lt
  | Geq
  | Gt

-- |Types of numeric unary operations
data NumericOp1
  = Neg

-- |Types of numeric binary operations
data NumericOp2
  = Mul
  | Div
  | Add
  | Sub

-- |Types of boolean unary operations
data BooleanOp1
  = Not

-- |Types of boolean binary operations
data BooleanOp2
  = Impl
  | And
  | Or

type Precedence = Int
