module Vehicle.Syntax.Parse.Error
  ( ParseError (..),
  )
where

import Data.Text (Text)
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin

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
  | InvalidAnnotationOptionValue Name Expr
  | MissingAnnotationOption Provenance Text Name
  | DuplicateAnnotationOption Provenance Text Name
  | -- Other
    UnknownBuiltin Provenance Text
  | MissingVariables Provenance Name
  | UnchainableOrders Provenance OrderOp OrderOp
  deriving (Show)
