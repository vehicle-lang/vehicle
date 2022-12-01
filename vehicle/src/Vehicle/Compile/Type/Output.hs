module Vehicle.Compile.Type.Output where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import Vehicle.Compile.Prelude
import Vehicle.Expr.Normalised (GluedExpr(..))

--------------------------------------------------------------------------------
-- Typed expressions

-- | A typed-expression. Outside of the type-checker, the contents of this
-- should not be inspected directly but instead use
newtype TypedExpr = TypedExpr
  { glued :: GluedExpr
  -- |^ Stores the both the unnormalised and normalised expression, WITH
  -- auxiliary annotations.
  } deriving (Generic)

instance ToJSON   TypedExpr
instance FromJSON TypedExpr

type TypedDecl = GenericDecl TypedExpr
type TypedProg = GenericProg TypedExpr
