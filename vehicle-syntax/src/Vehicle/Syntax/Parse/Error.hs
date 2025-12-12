module Vehicle.Syntax.Parse.Error
  ( ParseError (..),
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter (Doc)
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin

--------------------------------------------------------------------------------
-- Parsing

data ParseError
  = -- Parse errors
    RawParseError String
  | FunctionWithMismatchedNames Provenance Identifier Identifier
  | -- Annotations
    UnannotatedAbstractDef Provenance Identifier
  | MultiplyAnnotatedDef Provenance Identifier (Doc Void) (Doc Void)
  | TypeDefWithAnnotation Provenance Identifier (Doc Void)
  | FunctionDefWithRecordAnnotation Provenance Identifier (Doc Void)
  | RecordDefWithFunctionAnnotation Provenance Identifier (Doc Void)
  | AbstractDefWithNonAbstractAnnotation Provenance Identifier (Doc Void)
  | NonAbstractDefWithAbstractAnnotation Provenance Identifier (Doc Void)
  | AnnotationWithNoDef Provenance Name
  | TensorAnnotationWithParameters Provenance Identifier
  | -- Annotation options
    InvalidAnnotationOption Provenance Name Name [Name]
  | InvalidAnnotationOptionValue Name Expr
  | MissingAnnotationOption Provenance Text Name
  | DuplicateAnnotationOption Provenance Text Name
  | -- Other
    UnknownBuiltin Provenance Text
  | MissingVariables Provenance Name
  | UnchainableComparisons Provenance ComparisonOp ComparisonOp
  deriving (Show)
