module Vehicle.Syntax.Parse.Error
  ( ParseError(..)
  ) where

import Control.Monad.Except (MonadError (..), liftEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.IO qualified as T

import Vehicle.Syntax.AST
import Vehicle.Syntax.Parse.Token
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------
-- Parsing

data ParseError
  -- Parse errors
  = RawParseError String

  | UnknownBuiltin               Provenance Text
  | FunctionNotGivenBody         Provenance Name
  | PropertyNotGivenBody         Provenance Name
  | ResourceGivenBody            Provenance Name Name
  | AnnotationWithNoDeclaration  Provenance Name
  | FunctionWithMismatchedNames  Provenance Name Name
  | MissingVariables             Provenance Name
  | UnchainableOrders            Provenance OrderOp OrderOp
  | InvalidAnnotationOption      Provenance Name Name [Name]
  | InvalidAnnotationOptionValue Provenance Name Name
  deriving (Show)
