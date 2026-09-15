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
    mkHole,
    normAppList,
    headOf,
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
import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Data.Builtin.Standard.Core (Builtin (..), TypeClassOp (..))

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle internal expressions.
data Expr builtin
  = -- | A universe, used to type types.
    Universe
      Provenance
  | -- | Application of one term to another. Doesn't have provenance as it has no syntax in the grammar.
    UnsafeApp
      (Expr builtin) -- Function.
      (NonEmpty (Arg builtin)) -- Arguments.
  | -- | Dependent product (subsumes both functions and universal quantification).
    Pi
      Provenance
      (Binder builtin) -- The bound name
      (Expr builtin) -- (Dependent) result type.
  | -- | Terms consisting of constants that are built into the language.
    Builtin
      Provenance
      builtin -- Builtin name.
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
      (Expr builtin) -- Bound expression body.
      (Binder builtin) -- Bound expression name.
      (Expr builtin) -- Expression body.
  | -- | Lambda expressions (i.e. anonymous functions).
    Lam
      Provenance
      (Binder builtin) -- Bound expression name.
      (Expr builtin) -- Expression body.
  | -- | Records
    Record
      Provenance
      (RecordFields builtin)
  | -- | Record accessors.
    --
    -- NOTE: we could replace `RecordAcc` with `App Identifier Record`
    -- but difficult to elaborate back afterwards
    RecordAcc
      Provenance
      (Expr builtin) -- The record
      FieldName -- The field to access
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- The AST datatypes specialised to the Expr type

type Type = Expr

type Binder builtin = GenericBinder (Expr builtin)

type Telescope builtin = GenericTelescope (Expr builtin)

type Arg builtin = GenericArg (Expr builtin)

type RecordField builtin = GenericRecordField (Expr builtin)

type RecordFields builtin = GenericRecordFields (Expr builtin)

type Decl builtin = GenericDecl (Expr builtin)

type Module builtin = GenericModule (Expr builtin)

--------------------------------------------------------------------------------
-- Safe applications

-- | Smart constructor for applications with possibly no arguments.
normAppList :: Expr builtin -> [Arg builtin] -> Expr builtin
normAppList f [] = f
normAppList f (x : xs) = App f (x :| xs)

-- | Smart constructor for applications.
normApp :: Expr builtin -> NonEmpty (Arg builtin) -> Expr builtin
normApp (UnsafeApp f xs) ys = UnsafeApp f (xs <> ys)
normApp f xs = UnsafeApp f xs

-- | Safe pattern synonym for applications.
pattern App :: Expr builtin -> NonEmpty (Arg builtin) -> Expr builtin
pattern App f xs <- UnsafeApp f xs
  where
    App f xs = normApp f xs

{-# COMPLETE Universe, App, Pi, Builtin, Var, Hole, Let, Lam, Record, RecordAcc #-}

headOf :: Expr builtin -> Expr builtin
headOf = \case
  UnsafeApp f _ -> f
  e -> e

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance (Expr builtin) where
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

instance HasBasicBinders (Expr builtin) where
  getPiBinder = \case
    Pi _ binder body -> Just (binder, body)
    _ -> Nothing

  getLamBinder = \case
    Lam _ binder body -> Just (binder, body)
    _ -> Nothing

  getLetBinder = \case
    Let _ value binder body -> Just (value, binder, body)
    _ -> Nothing

instance HasBuiltinBinders (Expr Builtin) where
  getQuantifierBinder q = \case
    App (Var _ "existsTC") ((argExpr -> Lam _ binder body) :| []) | q == Exists -> Just (binder, body)
    App (Var _ "forallTC") ((argExpr -> Lam _ binder body) :| []) | q == Forall -> Just (binder, body)
    _ -> Nothing

  getForeachBinder = \case
    App (Builtin _ (TypeClassOp ForeachTC)) ((argExpr -> Lam _ binder body) :| []) -> Just (binder, body)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Utilities

mkHole :: Provenance -> Name -> Expr builtin
mkHole p name = Hole p ("_" <> name)
