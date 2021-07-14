{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Vehicle.Core.AST.Utils where

import Vehicle.Prelude ( Provenance, Symbol )
import Vehicle.Core.AST.Core (Expr(..), Decl(..), Prog(..), Binder(..))

-- |Extract the annotation
annotation :: Expr name binder ann -> ann
annotation = \case
  Star    ann       -> ann
  App     ann _ _   -> ann
  Fun     ann _ _   -> ann
  Builtin ann _     -> ann
  Bound   ann _     -> ann
  Free    ann _     -> ann
  Meta    ann _     -> ann
  Forall  ann _ _ _ -> ann
  Let     ann _ _ _ -> ann
  Lam     ann _ _   -> ann
  Literal ann _     -> ann
  Seq     ann _     -> ann

-- | Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler
type InputName = Symbol
type InputBind = Symbol
type InputAnn  = Provenance

type InputBinder = Binder InputBind InputAnn
type InputExpr   = Expr InputName InputBind InputAnn
type InputDecl   = Decl InputName InputBind InputAnn
type InputProg   = Prog InputName InputBind InputAnn

-- | Type of annotations attached to the Core AST that are output by the compiler
type OutputName   = Symbol
type OutputBind   = Symbol
data OutputAnn    = OutputAnn (Expr OutputName OutputBind OutputAnn) Provenance

type OutputBinder = Binder InputBind InputAnn
type OutputExpr   = Expr OutputName OutputBind OutputAnn
type OutputDecl   = Decl OutputName OutputBind OutputAnn
type OutputProg   = Prog OutputName OutputBind OutputAnn