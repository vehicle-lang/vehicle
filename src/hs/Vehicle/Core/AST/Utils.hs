{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.AST.Utils where

import Vehicle.Prelude ( Provenance, Symbol )
import Vehicle.Core.AST.Core (Expr(..), Arg, Ident, Decl, Prog, PiBinder, LamBinder)

-- |Extract the annotation
annotation :: Expr name binder ann -> ann
annotation = \case
  Kind               -> error "Should not be requesting an annotation from Kind"
  App      ann _ _   -> ann
  Pi       ann _ _   -> ann
  Builtin  ann _     -> ann
  Bound    ann _     -> ann
  Free     ann _     -> ann
  Meta     ann _     -> ann
  Let      ann _ _ _ -> ann
  Lam      ann _ _   -> ann
  Literal  ann _     -> ann
  Seq      ann _     -> ann

-- | An annotation that stores both the type of the expression and some other arbitrary annotations.
-- Used to avoid unrestricted type-level recursion.
data RecAnn binder var ann = RecAnn (Expr binder var (RecAnn binder var ann)) ann

-- | Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler
type InputBind = Symbol
type InputVar  = Symbol
type InputAnn  = Provenance

type InputArg       = Arg       InputBind InputVar InputAnn
type InputPiBinder  = PiBinder  InputBind InputVar InputAnn
type InputLamBinder = LamBinder InputBind InputVar InputAnn
type InputExpr      = Expr      InputBind InputVar InputAnn
type InputIdent     = Ident     InputAnn
type InputDecl      = Decl      InputBind InputVar InputAnn
type InputProg      = Prog      InputBind InputVar InputAnn

-- | Type of annotations attached to the Core AST that are output by the compiler
type OutputBind = Symbol
type OutputVar  = Symbol
type OutputAnn  = RecAnn OutputBind OutputVar Provenance

type OutputArg       = Arg       OutputBind OutputVar OutputAnn
type OutputPiBinder  = PiBinder  OutputBind OutputVar OutputAnn
type OutputLamBinder = LamBinder OutputBind OutputVar OutputAnn
type OutputExpr      = Expr      OutputBind OutputVar OutputAnn
type OutputIdent     = Ident     OutputAnn
type OutputDecl      = Decl      OutputBind OutputVar OutputAnn
type OutputProg      = Prog      OutputBind OutputVar OutputAnn