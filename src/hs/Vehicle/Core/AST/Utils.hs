{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.AST.Utils where

import Numeric.Natural (Natural)

import Vehicle.Prelude
import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn

-- * Type synonyms for Type levels

pattern Type0 :: Expr binder var ann
pattern Type0 = Type 0

pattern Type1 :: Expr binder var ann
pattern Type1 = Type 1

-- * Type synonyms for literals

pattern LitNat :: ann -> Natural -> Expr binder var ann
pattern LitNat ann n = Literal ann (LNat n)

pattern LitInt :: ann -> Integer -> Expr binder var ann
pattern LitInt ann n = Literal ann (LInt n)

pattern LitReal :: ann -> Double -> Expr binder var ann
pattern LitReal ann n = Literal ann (LReal n)

pattern LitBool :: ann -> Bool -> Expr binder var ann
pattern LitBool ann n = Literal ann (LBool n)

-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

type InputBind = Symbol
type InputVar  = Symbol
type InputAnn  = Provenance

type InputArg    = Arg    InputBind InputVar InputAnn
type InputBinder = Binder InputBind InputVar InputAnn
type InputExpr   = Expr   InputBind InputVar InputAnn
type InputDecl   = Decl   InputBind InputVar InputAnn
type InputProg   = Prog   InputBind InputVar InputAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputBind = Symbol
type OutputVar  = Symbol
type OutputAnn  = RecAnn OutputBind OutputVar Provenance

type OutputArg    = Arg    OutputBind OutputVar OutputAnn
type OutputBinder = Binder OutputBind OutputVar OutputAnn
type OutputExpr   = Expr   OutputBind OutputVar OutputAnn
type OutputDecl   = Decl   OutputBind OutputVar OutputAnn
type OutputProg   = Prog   OutputBind OutputVar OutputAnn

-- * Types pre type-checking

type UncheckedAnn  = Provenance
type UncheckedExpr = DeBruijnExpr UncheckedAnn
type UncheckedDecl = DeBruijnDecl UncheckedAnn
type UncheckedProg = DeBruijnProg UncheckedAnn

-- * Types post type-checking

type CheckedAnn  = DeBruijnAnn Provenance
type CheckedExpr = DeBruijnExpr CheckedAnn
type CheckedDecl = DeBruijnDecl CheckedAnn
type CheckedProg = DeBruijnProg CheckedAnn


-- |Extract a term's annotation
annotation :: Expr name binder ann -> ann
annotation = \case
  Type     _         -> error "Should not be requesting an annotation from Type"
  Constraint         -> error "Should not be requesting an annotation from Constraint"
  Meta     _         -> error "Should not be requesting an annotation from Meta"
  Hole     _   _     -> error "Should not be requesting an annotation from Hole"
  Ann      ann _ _   -> ann
  App      ann _ _   -> ann
  Pi       ann _ _   -> ann
  Builtin  ann _     -> ann
  Bound    ann _     -> ann
  Free     ann _     -> ann
  Let      ann _ _ _ -> ann
  Lam      ann _ _   -> ann
  Literal  ann _     -> ann
  Seq      ann _     -> ann

-- | Extracts the type of the term from the term's annotation.
getType :: Expr binder var (RecAnn binder var ann) -> Expr binder var (RecAnn binder var ann)
getType (Type l)   = Type (l + 1)
getType Constraint = Type1
getType e          = let RecAnn t _ = annotation e in t

isConstraint :: CheckedExpr -> Bool
isConstraint e = case getType e of
  Constraint -> True
  _          -> False

-- * Instances

instance HasProvenance DeclName where
  prov (DeclName p _) = p

