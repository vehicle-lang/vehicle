{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Vehicle.Core.AST.Utils where

import Vehicle.Prelude
import Vehicle.Core.AST.Core (Tree(..))
import Vehicle.Core.AST.Recursive (TreeF)
import Vehicle.Core.AST.Info.Core (Info)

-- |Extract the annotation
annotation :: forall sort name ann.
              KnownSort sort =>
              Tree name ann sort ->
              ann sort
annotation = case sortSing :: SSort sort of
  -- Kinds
  SKIND -> \case
    KApp  ann _k1 _k2 -> ann
    KCon  ann _b      -> ann
    KMeta ann _i      -> ann

  -- Types
  STYPE -> \case
    TForall     ann _k _ns _t  -> ann
    TApp        ann _t1 _t2    -> ann
    TVar        ann _n         -> ann
    TCon        ann _b         -> ann
    TLitDim     ann _i         -> ann
    TLitDimList ann _ts        -> ann
    TMeta       ann _i         -> ann

  -- Type arguments
  STARG -> \case
    TArg ann _n -> ann

  -- Expressions
  SEXPR -> \case
    EAnn     ann _e _t       -> ann
    ELet     ann _n _e1 _e2  -> ann
    ELam     ann _ns _e      -> ann
    EApp     ann _e1 _e2     -> ann
    EVar     ann _n          -> ann
    ETyApp   ann _e _t       -> ann
    ETyLam   ann _ns _e      -> ann
    ECon     ann _b          -> ann
    ELitInt  ann _i          -> ann
    ELitReal ann _d          -> ann
    ELitSeq  ann _es         -> ann

  -- Expression arguments
  SEARG -> \case
    EArg ann _n -> ann

  -- Declarations
  SDECL -> \case
    DeclNetw   ann _n _t     -> ann
    DeclData   ann _n _t     -> ann
    DefType    ann _n _ns _t -> ann
    DefFun     ann _n _t _e  -> ann

  -- Programs
  SPROG -> \case
    Main ann _ds -> ann

-- | Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler
type InputAnn = K Provenance :: Sort -> *

type InputTree sort = Tree (K Symbol) InputAnn sort
type InputKind = InputTree 'KIND
type InputType = InputTree 'TYPE
type InputTArg = InputTree 'TARG
type InputExpr = InputTree 'EXPR
type InputEArg = InputTree 'EARG
type InputDecl = InputTree 'DECL
type InputProg = InputTree 'PROG

type InputTreeF (sort :: Sort) (sorted :: Sort -> *)
  = TreeF (K Symbol) InputAnn sort sorted

instance KnownSort sort => HasProvenance (InputAnn sort) where
  prov (K p) = p

instance KnownSort sort => HasProvenance (InputTree sort) where
  prov = prov . annotation

-- | Type of annotations attached to the Core AST that are output by the compiler
type OutputAnn = Info (K Symbol) :*: K Provenance :: Sort -> *

type OutputTree sort = Tree (K Symbol) OutputAnn sort

type OutputKind = OutputTree 'KIND
type OutputType = OutputTree 'TYPE
type OutputTArg = OutputTree 'TARG
type OutputExpr = OutputTree 'EXPR
type OutputEArg = OutputTree 'EARG
type OutputDecl = OutputTree 'DECL
type OutputProg = OutputTree 'PROG
type OutputTreeF (sort :: Sort) (sorted :: Sort -> *)
  = TreeF (K Symbol) OutputAnn sort sorted

instance KnownSort sort => HasProvenance (OutputAnn sort) where
  prov (_ :*: K p) = p

instance KnownSort sort => HasProvenance (OutputTree sort) where
  prov = prov . annotation