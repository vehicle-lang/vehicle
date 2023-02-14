module Vehicle.Compile.Type.Core where

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint (Constraint, UnificationConstraint)
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Expr.DeBruijn

-- * Types pre type-checking

type UncheckedBinding = DBBinding

type UncheckedVar = DBIndexVar

type UncheckedBinder builtin = DBBinder builtin

type UncheckedArg builtin = DBArg builtin

type UncheckedExpr builtin = DBExpr builtin

type UncheckedType builtin = DBExpr builtin

type UncheckedDecl builtin = DBDecl builtin

type UncheckedProg builtin = DBProg builtin

-- * Types post type-checking

type CheckedBinding = DBBinding

type CheckedVar = DBIndexVar

type CheckedBinder builtin = DBBinder builtin

type CheckedArg builtin = DBArg builtin

type CheckedExpr builtin = DBExpr builtin

type CheckedType builtin = CheckedExpr builtin

type CheckedDecl builtin = DBDecl builtin

type CheckedProg builtin = DBProg builtin

type Imports builtin = [TypedProg builtin]

-- | Errors in bidirectional type-checking
data TypingError builtin
  = MissingExplicitArgument (TypingBoundCtx builtin) (CheckedBinder builtin) (UncheckedArg builtin)
  | FunctionTypeMismatch (TypingBoundCtx builtin) (CheckedExpr builtin) [UncheckedArg builtin] (CheckedExpr builtin) [UncheckedArg builtin]
  | FailedUnification (NonEmpty (WithContext (UnificationConstraint builtin)))
  | UnsolvableConstraints (NonEmpty (WithContext (Constraint builtin)))
