
module Vehicle.Backend.Core where

import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader, MonadReader(..), ReaderT(..))
import Data.Text as Text (Text, intercalate, pack, append)
import Data.Map (Map)
import Data.Version (Version, showVersion)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print

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
-- Control

-- |Constraint for the monad stack used by the Compiler.
type MonadCompile options m =
  (MonadError CompileError m, MonadReader (ITPOptions options) m)

type Compile a options = ReaderT (ITPOptions options) (Except CompileError) a

-- * Type of errors that can be thrown during compilation
data CompileError
  = CompilationUnsupported Provenance Symbol
  | TensorIndexOutOfBounds Provenance OutputExpr Integer

instance MeaningfulError CompileError where
  details (CompilationUnsupported p symbol) = UError $ UserError
    { provenance = p
    , problem    = "compilation of" <+> squotes (pretty symbol) <+> "is not supported"
    , fix        = "see user manual for details"
    }

  details (TensorIndexOutOfBounds p tensorTyp index) = UError $ UserError
    { provenance = p
    , problem    = "index" <+> pretty index <+> "is larger than the first dimension of the type" <+> prettyFriendly tensorTyp
    , fix        = "check your indexing"
    }

unexpectedTypeError :: OutputExpr -> OutputExpr -> [String] -> a
unexpectedTypeError expr actualType expectedTypes = developerError $
  "Unexpected type found for expression" <+> prettyFriendly expr <> "." <> line <>
  "Was expecting one of" <+> pretty expectedTypes <+>
  "but found" <+> prettyFriendly actualType <+>
  "at" <+> pretty (provenanceOf expr) <> "."

unexpectedExprError :: Provenance -> OutputExpr -> [OutputExpr] -> a
unexpectedExprError p actualExpr expectedExprs = developerError $
  "Was expecting something of the form" <+> prettyFriendly expectedExprs <+>
  "but found" <+> prettyFriendly actualExpr <+>
  "at" <+> pretty p <> "."

--------------------------------------------------------------------------------
-- Generic language features

type Precedence = Int
{-
--------------------------------------------------------------------------------
-- Subcategories of types/expressions

data NumericType
  = TNat
  | TInt
  | TReal
  deriving (Eq, Ord, Show, Read, Enum)

data TruthType
  = TBool
  | TProp
  deriving (Eq, Ord, Show, Read, Enum)

data ContainerType
  = TList
  | TTensor [Int]
  deriving (Eq, Ord, Show)

numericType :: OutputExpr -> NumericType
numericType expr = go $ getType expr
  where
    go :: OutputExpr -> NumericType
    go = \case
      Int   _ann        -> TInt
      Real  _ann        -> TReal
      Fun   _ann _t1 t2 -> go t2
      typ               -> unexpectedTypeError expr typ ["Real", "Int", "X -> Bool", "X -> Prop"]

truthType :: OutputExpr -> TruthType
truthType expr = go $ getType expr
  where
    go :: OutputExpr -> TruthType
    go = \case
      Bool _ann        -> TBool
      Prop _ann        -> TProp
      Fun  _ann _t1 t2 -> go t2
      typ              -> unexpectedTypeError expr typ ["Bool", "Prop", "X -> Bool", "X -> Prop"]

containerType :: OutputExpr -> ContainerType
containerType expr = case getType expr of
  List   _ _            -> TList
  Tensor _ _ (Seq _ ds) -> TTensor (map fromLit ds)
    where
      fromLit :: OutputExpr -> Int
      fromLit (LitInt _ i) = i
      fromLit t            = unexpectedTypeError expr t ["a literal"]
  t              -> unexpectedTypeError expr t ["List", "Tensor"]

-- |Types of numeric orders
data OrderType
  = LeqOp
  | LtOp
  | GeqOp
  | GtOp

-- |Types of numeric unary operations
data NumericOp1
  = NegOp

-- |Types of numeric binary operations
data NumericOp2
  = MulOp
  | DivOp
  | AddOp
  | SubOp

-- |Types of boolean unary operations
data BooleanOp1
  = NotOp

-- |Types of boolean binary operations
data BooleanOp2
  = ImplOp
  | AndOp
  | OrOp

-}