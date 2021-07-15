{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.AST.Utils where

import Vehicle.Prelude ( Provenance, Symbol )
import Vehicle.Core.AST.Core (Expr(..), Decl(..), Prog(..), Binder(..))

-- |Extract the annotation
annotation :: Expr name binder ann -> ann
annotation = \case
  App      ann _ _   -> ann
  Pi       ann _ _ _ -> ann
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
data TypedAnn binder var ann = TypedAnn (Expr binder var (TypedAnn binder var ann)) ann

-- | Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler
type InputBind = Symbol
type InputVar  = Symbol
type InputAnn  = Provenance

type InputBinder = Binder InputBind          InputAnn
type InputExpr   = Expr   InputBind InputVar InputAnn
type InputDecl   = Decl   InputBind InputVar InputAnn
type InputProg   = Prog   InputBind InputVar InputAnn

-- | Type of annotations attached to the Core AST that are output by the compiler
type OutputBind = Symbol
type OutputVar  = Symbol
type OutputAnn  = TypedAnn OutputBind OutputVar Provenance

type OutputBinder = Binder OutputBind           OutputAnn
type OutputExpr   = Expr   OutputBind OutputVar OutputAnn
type OutputDecl   = Decl   OutputBind OutputVar OutputAnn
type OutputProg   = Prog   OutputBind OutputVar OutputAnn