{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Vehicle.Frontend.AST.Utils where

import Vehicle.Prelude
import Vehicle.Frontend.AST.Core (Tree(..))
import Vehicle.Frontend.AST.Info (Info)

-- |Extract the top-level annotation from a tree
annotation :: forall sort ann.
              KnownSort sort =>
              Tree ann sort ->
              ann sort
annotation = case sortSing :: SSort sort of
  -- Kinds
  SKIND -> \case
    KApp     ann _k1 _k2 -> ann
    KFun     ann _k1 _k2 -> ann
    KType    ann         -> ann
    KDim     ann         -> ann
    KDimList ann         -> ann

  -- Types
  STYPE -> \case
    TForall     ann _ns _t  -> ann
    TApp        ann _t1 _t2 -> ann
    TVar        ann _n      -> ann
    TFun        ann _t1 _t2 -> ann
    TBool       ann         -> ann
    TProp       ann         -> ann
    TReal       ann         -> ann
    TInt        ann         -> ann
    TList       ann _t      -> ann
    TTensor     ann _t1 _t2 -> ann
    TAdd        ann _t1 _t2 -> ann
    TLitDim     ann _i      -> ann
    TCons       ann _t1 _t2 -> ann
    TLitDimList ann _ts     -> ann

  -- Type arguments
  STARG -> \case
    TArg ann _n -> ann

  -- Expressions
  SEXPR -> \case
    EAnn     ann _e _t       -> ann
    ELet     ann _ds _e      -> ann
    ELam     ann _ns _e      -> ann
    EApp     ann _e1 _e2     -> ann
    EVar     ann _n          -> ann
    ETyApp   ann _e _t       -> ann
    ETyLam   ann _ns _e      -> ann
    EIf      ann _e1 _e2 _e3 -> ann
    EImpl    ann _e1 _e2     -> ann
    EAnd     ann _e1 _e2     -> ann
    EOr      ann _e1 _e2     -> ann
    ENot     ann _e          -> ann
    ETrue    ann             -> ann
    EFalse   ann             -> ann
    EEq      ann _e1 _e2     -> ann
    ENeq     ann _e1 _e2     -> ann
    ELe      ann _e1 _e2     -> ann
    ELt      ann _e1 _e2     -> ann
    EGe      ann _e1 _e2     -> ann
    EGt      ann _e1 _e2     -> ann
    EMul     ann _e1 _e2     -> ann
    EDiv     ann _e1 _e2     -> ann
    EAdd     ann _e1 _e2     -> ann
    ESub     ann _e1 _e2     -> ann
    ENeg     ann _e          -> ann
    ELitInt  ann _i          -> ann
    ELitReal ann _d          -> ann
    ECons    ann _e1 _e2     -> ann
    EAt      ann _e1 _e2     -> ann
    EAll     ann             -> ann
    EAny     ann             -> ann
    ELitSeq  ann _es         -> ann

  -- Expression arguments
  SEARG -> \case
    EArg ann _n -> ann

  -- Declarations
  SDECL -> \case
    DeclNetw ann _n _t        -> ann
    DeclData ann _n _t        -> ann
    DefType  ann _n _ns _t    -> ann
    DefFun   ann _n _t _ns _e -> ann

  -- Programs
  SPROG -> \case
    Main ann _ds -> ann

-- | Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler
type InputAnn = K Provenance :: Sort -> *

type InputTree sort = Tree InputAnn sort
type InputKind = InputTree 'KIND
type InputType = InputTree 'TYPE
type InputTArg = InputTree 'TARG
type InputExpr = InputTree 'EXPR
type InputEArg = InputTree 'EARG
type InputDecl = InputTree 'DECL
type InputProg = InputTree 'PROG

instance KnownSort sort => HasProvenance (InputAnn sort) where
  prov (K p) = p

instance KnownSort sort => HasProvenance (InputTree sort) where
  prov = prov . annotation

-- | Type of annotations attached to the Frontend AST that are output by the compiler
type OutputAnn = Info :*: K Provenance :: Sort -> *

type OutputTree sort = Tree OutputAnn sort
type OutputKind = OutputTree 'KIND
type OutputType = OutputTree 'TYPE
type OutputTArg = OutputTree 'TARG
type OutputExpr = OutputTree 'EXPR
type OutputEArg = OutputTree 'EARG
type OutputDecl = OutputTree 'DECL
type OutputProg = OutputTree 'PROG

instance KnownSort sort => HasProvenance (OutputAnn sort) where
  prov (_ :*: K p) = p

instance KnownSort sort => HasProvenance (OutputTree sort) where
  prov = prov . annotation