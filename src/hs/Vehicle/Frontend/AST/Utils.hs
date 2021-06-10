{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE TypeFamilies #-}
module Vehicle.Frontend.AST.Utils where

import Vehicle.Prelude (Symbol)
import Vehicle.Prelude.Sort (KnownSort, SSort(..), sortSing)
import Vehicle.Frontend.AST.Core ( Decl, EArg, TArg, Tree(..) )

-- |Check if a declaration is a network declaration.
isDefFun :: Decl ann -> Bool
isDefFun (DefFunType _ann _name _typ) = True
isDefFun (DefFunExpr _ann _name _args _exp) = True
isDefFun _ = False

-- |Get the name of targ
tArgName :: TArg ann -> Symbol
tArgName (TArg _ann name) = name

-- | Get the name of an expression argument
eArgName :: EArg ann -> Symbol
eArgName (EArg _ann name)= name

-- |Get the name for any declaration.
declName :: Decl ann -> Symbol
declName (DeclNetw   _ann arg _typ)        = eArgName arg
declName (DeclData   _ann arg _typ)        = eArgName arg
declName (DefType    _ann arg _args _type) = tArgName arg
declName (DefFunType _ann arg _typ)        = eArgName arg
declName (DefFunExpr _ann arg _args _exp)  = eArgName arg

-- |Extract the annotation
annotation :: forall sort ann.
              KnownSort sort =>
              Tree ann sort ->
              ann sort
annotation = case sortSing :: SSort sort of
  -- Kinds
  SKIND -> \case
    KApp  ann _k1 _k2 -> ann
    KFun  ann _k1 _k2 -> ann
    KType ann         -> ann
    KDim  ann         -> ann
    KList ann         -> ann

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
    TList       ann         -> ann
    TTensor     ann         -> ann
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
    DeclNetw   ann _n _t     -> ann
    DeclData   ann _n _t     -> ann
    DefType    ann _n _ns _t -> ann
    DefFunType ann _n _t     -> ann
    DefFunExpr ann _n _ns _e -> ann

  -- Programs
  SPROG -> \case
    Main ann _ds -> ann

