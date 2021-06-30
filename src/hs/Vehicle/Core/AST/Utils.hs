{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Vehicle.Core.AST.Utils where

import Vehicle.Prelude
import Vehicle.Core.AST.Core (Tree(..))
import Vehicle.Core.AST.Recursive (TreeF)
import Vehicle.Core.AST.Info.Core (Info)

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

type OutputAnn = Info :*: K Provenance :: Sort -> *
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
    TForall     ann _ns _t  -> ann
    TApp        ann _t1 _t2 -> ann
    TVar        ann _n      -> ann
    TCon        ann _b      -> ann
    TLitDim     ann _i      -> ann
    TLitDimList ann _ts     -> ann
    TMeta       ann _i      -> ann

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

