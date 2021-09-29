{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.AST.Utils where

import Vehicle.Prelude
import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn

--------------------------------------------------------------------------------
-- Patterns

pattern Type0 :: Expr var ann
pattern Type0 = Type 0

pattern Type1 :: Expr var ann
pattern Type1 = Type 1

pattern LitNat :: ann -> Int -> Expr var ann
pattern LitNat ann n = Literal ann (LNat n)

pattern LitInt :: ann -> Int -> Expr var ann
pattern LitInt ann n = Literal ann (LInt n)

pattern LitRat :: ann -> Double -> Expr var ann
pattern LitRat ann n = Literal ann (LRat n)

pattern LitBool :: ann -> Bool -> Expr var ann
pattern LitBool ann n = Literal ann (LBool n)

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

type InputVar  = Name
type InputAnn  = Provenance

type InputArg       = Arg    InputVar InputAnn
type InputBinder    = Binder InputVar InputAnn
type InputExpr      = Expr   InputVar InputAnn
type InputDecl      = Decl   InputVar InputAnn
type InputProg      = Prog   InputVar InputAnn

-- * Types pre type-checking

type UncheckedVar    = Var
type UncheckedAnn    = Provenance

type UncheckedBinder  = DeBruijnBinder UncheckedAnn
type UncheckedArg     = DeBruijnArg    UncheckedAnn
type UncheckedExpr    = DeBruijnExpr   UncheckedAnn
type UncheckedDecl    = DeBruijnDecl   UncheckedAnn
type UncheckedProg    = DeBruijnProg   UncheckedAnn

-- * Types post type-checking

type CheckedVar    = Var
type CheckedAnn    = Provenance

type CheckedBinder  = DeBruijnBinder  CheckedAnn
type CheckedArg     = DeBruijnArg     CheckedAnn
type CheckedExpr    = DeBruijnExpr    CheckedAnn
type CheckedDecl    = DeBruijnDecl    CheckedAnn
type CheckedProg    = DeBruijnProg    CheckedAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputVar  = Name
type OutputAnn  = Provenance

type OutputBinder  = Binder OutputVar OutputAnn
type OutputArg     = Arg    OutputVar OutputAnn
type OutputExpr    = Expr   OutputVar OutputAnn
type OutputDecl    = Decl   OutputVar OutputAnn
type OutputProg    = Prog   OutputVar OutputAnn

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance ann => HasProvenance (Expr var ann) where
  prov (Hole p _) = p
  prov e          = prov (annotation e)

--------------------------------------------------------------------------------
-- Utility functions

-- |Extract a binder's name
binderName :: Binder var ann -> Name
binderName (Binder _ _ name _) = name

binderType :: Binder var ann -> Expr var ann
binderType (Binder _ _ _ t) = t

argExpr :: Arg var ann -> Expr var ann
argExpr (Arg _ _ e) = e

-- |Extract a term's annotation
annotation :: Expr name ann -> ann
annotation = \case
  Type     _         -> developerError "Should not be requesting an annotation from Type"
  Hole     _   _     -> developerError "Should not be requesting an annotation from Hole"
  PrimDict _         -> developerError "Should not be requesting an annotation from PrimitiveDict"
  Meta     ann _     -> ann
  Ann      ann _ _   -> ann
  App      ann _ _   -> ann
  Pi       ann _ _   -> ann
  Builtin  ann _     -> ann
  Var      ann _     -> ann
  Let      ann _ _ _ -> ann
  Lam      ann _ _   -> ann
  Literal  ann _     -> ann
  Seq      ann _     -> ann

decomposeApp :: CheckedExpr -> (CheckedExpr, [CheckedArg])
decomposeApp = go []
  where go args (App _ann fun arg) = go (arg:args) fun
        go args e                  = (e, args)

composeApp :: CheckedAnn -> CheckedExpr -> [CheckedArg] -> CheckedExpr
composeApp ann = foldl (App ann)

decomposeExplicitApp :: CheckedExpr -> (CheckedExpr, [CheckedArg])
decomposeExplicitApp = go []
  where
    go args = \case
      (App _ann fun arg@(Arg _ Explicit _)) -> go (arg : args) fun
      (App _ann fun _arg)                   -> go args fun
      e                                     -> (e, args)