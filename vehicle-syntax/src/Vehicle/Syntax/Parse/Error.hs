module Vehicle.Syntax.Parse.Error
  ( ParseError (..),
  )
where

import Control.Monad.Except (MonadError (..), liftEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Vehicle.Syntax.Parse.Token

--------------------------------------------------------------------------------
-- Parsing

data ParseError
  = -- Parse errors
    RawParseError String
  | -- Declarations
    UnannotatedAbstractDef Provenance Name
  | MultiplyAnnotatedAbstractDef Provenance Name DefAbstractSort DefAbstractSort
  | NonAbstractDefWithAbstractAnnotation Provenance Name DefAbstractSort
  | AbstractDefWithNonAbstractAnnotation Provenance Name Annotation
  | AnnotationWithNoDef Provenance Name
  | FunctionWithMismatchedNames Provenance Name Name
  | -- Annotations
    InvalidAnnotationOption Provenance Name Name [Name]
  | InvalidAnnotationOptionValue Name (Expr Name Builtin)
  | MissingAnnotationOption Provenance Text Name
  | DuplicateAnnotationOption Provenance Text Name
  | -- Other
    UnknownBuiltin Provenance Text
  | MissingVariables Provenance Name
  | UnchainableOrders Provenance OrderOp OrderOp
  deriving (Show)
