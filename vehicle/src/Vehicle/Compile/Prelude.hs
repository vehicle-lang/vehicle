module Vehicle.Compile.Prelude
  ( module X,
    module Vehicle.Compile.Prelude,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Vehicle.Compile.Dependency.Graph as X
import Vehicle.Compile.Prelude.Contexts as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.PositionTree (PositionTree)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (GluedExpr)
import Vehicle.Expr.Patterns as X
import Vehicle.Prelude as X
import Vehicle.Resource as X
import Vehicle.Syntax.AST as X

--------------------------------------------------------------------------------
-- Type synonyms

type NamedBinding = ()

type NamedVar = Name

type NamedArg builtin = Arg NamedBinding NamedVar builtin

type NamedBinder builtin = Binder NamedBinding NamedVar builtin

type NamedExpr builtin = Expr NamedBinding NamedVar builtin

type NamedDecl builtin = Decl NamedBinding NamedVar builtin

type NamedProg builtin = Prog NamedBinding NamedVar builtin

-- * Types post type-checking

type TypeCheckedBinder = DBBinder Builtin

type TypeCheckedArg = DBArg Builtin

type TypeCheckedExpr = DBExpr Builtin

type TypeCheckedType = DBExpr Builtin

type TypeCheckedDecl = DBDecl Builtin

type TypeCheckedProg = DBProg Builtin

-- * Type of annotations attached to the AST that are output by the compiler

type OutputBinding = ()

type OutputVar = Name

type OutputBinder = Binder OutputBinding OutputVar Builtin

type OutputArg = Arg OutputBinding OutputVar Builtin

type OutputExpr = Expr OutputBinding OutputVar Builtin

type OutputDecl = Decl OutputBinding OutputVar Builtin

type OutputProg = Prog OutputBinding OutputVar Builtin

-- | An expression paired with a position tree represting positions within it.
-- Currently used mainly for pretty printing position trees.
data PositionsInExpr = PositionsInExpr CoDBExpr PositionTree
  deriving (Show)

type DeclProvenance = (Identifier, Provenance)

--------------------------------------------------------------------------------
-- Typed expressions

-- | A typed-expression. Outside of the type-checker, the contents of this
-- should not be inspected directly but instead use
newtype TypedExpr builtin = StandardTypedExpr
  { glued :: GluedExpr builtin
  }
  -- \|^ Stores the both the unnormalised and normalised expression, WITH
  -- auxiliary annotations.
  deriving (Generic)

type TypedProg builtin = GenericProg (TypedExpr builtin)

type TypedDecl builtin = GenericDecl (TypedExpr builtin)

--------------------------------------------------------------------------------
-- Property annotations

-- | A marker for how a declaration is used as part of a quantified property
-- and therefore needs to be lifted to the type-level when being exported, or
-- whether it is only used unquantified and therefore needs to be computable.
data PropertyInfo
  = PropertyInfo Linearity Polarity
  deriving (Show, Eq, Generic)

instance NFData PropertyInfo

instance ToJSON PropertyInfo

instance Pretty PropertyInfo where
  pretty (PropertyInfo lin pol) = pretty lin <+> pretty pol

--------------------------------------------------------------------------------
-- Other

data Contextualised object context = WithContext
  { objectIn :: object,
    contextOf :: context
  }
  deriving (Show)

type family WithContext a

class HasType expr typ | expr -> typ where
  typeOf :: expr -> typ

instance HasType (GenericBinder binder expr) expr where
  typeOf = binderType

instance HasType (GenericDecl expr) expr where
  typeOf = \case
    DefResource _ _ _ t -> t
    DefFunction _ _ _ t _ -> t
    DefPostulate _ _ t -> t

mapObject :: (a -> b) -> Contextualised a ctx -> Contextualised b ctx
mapObject f WithContext {..} = WithContext {objectIn = f objectIn, ..}
