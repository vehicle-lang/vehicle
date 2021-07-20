{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.AST.Utils where

import Numeric.Natural (Natural)

import Vehicle.Prelude
import Vehicle.Core.AST.Core

instance HasProvenance DeclName where
  prov (DeclName p _) = p

-- |Extract a term's annotation
annotation :: Expr name binder ann -> ann
annotation = \case
  Kind               -> error "Should not be requesting an annotation from Kind"
  Meta     _         -> error "Should not be requesting an annotation from Meta"
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

-- | An annotation that stores both the type of the expression and some other arbitrary annotations.
-- Used post-type checking. Avoids unrestricted type-level recursion.
data RecAnn binder var ann = RecAnn (Expr binder var (RecAnn binder var ann)) ann

-- | Extracts the type of the term from the term's annotation.
getType :: Expr binder var (RecAnn binder var ann) -> Expr binder var (RecAnn binder var ann)
getType e = let RecAnn t _ = annotation e in t

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