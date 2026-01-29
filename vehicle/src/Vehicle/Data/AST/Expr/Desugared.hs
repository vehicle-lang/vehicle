{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal" #-}
module Vehicle.Data.AST.Expr.Desugared
  ( -- * Generic expressions
    Arg,
    Binder,
    Telescope,
    RecordField,
    RecordFields,
    Decl,
    Module,
    Expr
      ( Universe,
        App,
        Pi,
        Builtin,
        Var,
        Hole,
        Let,
        Lam,
        Record,
        RecordAcc
      ),
    Type,

    -- * Utilities
    isTypeSynonym,
    mkHole,
    normAppList,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Vehicle.Compile.Sugar.Binders
import Vehicle.Data.AST.Arg
import Vehicle.Data.AST.Binder
import Vehicle.Data.AST.Decl (GenericDecl)
import Vehicle.Data.AST.Module (GenericModule)
import Vehicle.Data.AST.Name (Name)
import Vehicle.Data.AST.Provenance (HasProvenance (..), Provenance, fillInProvenance)
import Vehicle.Data.AST.Record (FieldName, GenericRecordField, GenericRecordFields)
import Vehicle.Data.Builtin.Standard.Core (Builtin (..), TypeClassOp (..))

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle internal expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr
  = -- | A universe, used to type types.
    Universe
      Provenance
  | -- | Application of one term to another. Doesn't have provenance as it has no syntax in the grammar.
    UnsafeApp
      Expr -- Function.
      (NonEmpty Arg) -- Arguments.
  | -- | Dependent product (subsumes both functions and universal quantification).
    Pi
      Provenance
      Binder -- The bound name
      Expr -- (Dependent) result type.
  | -- | Terms consisting of constants that are built into the language.
    Builtin
      Provenance
      Builtin -- Builtin name.
  | -- | Variables in the program.
    Var
      Provenance
      Name -- Variable name.
  | -- | A hole in the program.
    Hole
      Provenance
      Name -- Hole name.
  | -- | Let expressions. We have these in the core syntax because we want to
    -- cross compile them to various backends.
    --
    -- NOTE: that the order of the bound expression and the binder is reversed
    -- to better mimic the flow of the context, which makes writing monadic
    -- operations concisely much easier.
    Let
      Provenance
      Expr -- Bound expression body.
      Binder -- Bound expression name.
      Expr -- Expression body.
  | -- | Lambda expressions (i.e. anonymous functions).
    Lam
      Provenance
      Binder -- Bound expression name.
      Expr -- Expression body.
  | -- | Records
    Record
      Provenance
      RecordFields
  | -- | Record accessors.
    --
    -- NOTE: we could replace `RecordAcc` with `App Identifier Record`
    -- but difficult to elaborate back afterwards
    RecordAcc
      Provenance
      Expr -- The record
      FieldName -- The field to access
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- The AST datatypes specialised to the Expr type

type Type = Expr

type Binder = GenericBinder Expr

type Telescope = GenericTelescope Expr

type Arg = GenericArg Expr

type RecordField = GenericRecordField Expr

type RecordFields = GenericRecordFields Expr

type Decl = GenericDecl Expr

type Module = GenericModule Expr

--------------------------------------------------------------------------------
-- Safe applications

-- | Smart constructor for applications with possibly no arguments.
normAppList :: Expr -> [Arg] -> Expr
normAppList f [] = f
normAppList f (x : xs) = App f (x :| xs)

-- | Smart constructor for applications.
normApp :: Expr -> NonEmpty Arg -> Expr
normApp (UnsafeApp f xs) ys = UnsafeApp f (xs <> ys)
normApp f xs = UnsafeApp f xs

-- | Safe pattern synonym for applications.
pattern App :: Expr -> NonEmpty Arg -> Expr
pattern App f xs <- UnsafeApp f xs
  where
    App f xs = normApp f xs

{-# COMPLETE Universe, App, Pi, Builtin, Var, Hole, Let, Lam, Record, RecordAcc #-}

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance Expr where
  provenanceOf = \case
    Universe p -> p
    Hole p _ -> p
    App e xs -> fillInProvenance (provenanceOf e :| provenanceOf xs : [])
    Pi p _ _ -> p
    Builtin p _ -> p
    Var p _ -> p
    Let p _ _ _ -> p
    Lam p _ _ -> p
    Record p _ -> p
    RecordAcc p _ _ -> p

instance HasBasicBinders Expr where
  getPiBinder = \case
    Pi _ binder body -> Just (binder, body)
    _ -> Nothing

  getLamBinder = \case
    Lam _ binder body -> Just (binder, body)
    _ -> Nothing

  getLetBinder = \case
    Let _ value binder body -> Just (value, binder, body)
    _ -> Nothing

instance HasBuiltinBinders Expr where
  getQuantifierBinder q = \case
    App (Builtin _ (TypeClassOp (QuantifierTC q'))) ((argExpr -> Lam _ binder body) :| []) | q == q' -> Just (binder, body)
    _ -> Nothing

  getForeachBinder = \case
    App (Builtin _ (TypeClassOp ForeachTC)) ((argExpr -> Lam _ binder body) :| []) -> Just (binder, body)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Utilities

mkHole :: Provenance -> Name -> Expr
mkHole p name = Hole p ("_" <> name)

-- | Tests if a definition's type indicates that the definition is a type
-- synonym.
isTypeSynonym :: Type -> Bool
isTypeSynonym = \case
  Universe {} -> True
  Pi _ _ res -> isTypeSynonym res
  _ -> False
