{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DataKinds #-} {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provides compatibility between the /more general/ type for Vehicle Core
--   abstract syntax trees defined in 'Vehicle.Core.Type' and the type expected
--   (and generated) by BNFC. The module /must/ be called 'Vehicle.Core.Abs', as
--   this is what BNFC expects (and generates).
module Vehicle.Core.Abs
  ( Kind
  , pattern KApp
  , pattern KCon
  , pattern KMeta
  , Type
  , pattern TForall
  , pattern TApp
  , pattern TVar
  , pattern TCon
  , pattern TLitDim
  , pattern TLitList
  , pattern TMeta
  , Expr
  , pattern EAnn
  , pattern ELet
  , pattern ELam
  , pattern EApp
  , pattern EVar
  , pattern ETyApp
  , pattern ETyLam
  , pattern ECon
  , pattern ELitInt
  , pattern ELitReal
  , pattern ELitSeq
  , Decl
  , pattern DeclNetw
  , pattern DeclData
  , pattern DefType
  , pattern DefFun
  , Prog
  , pattern Main
  , KindBuiltin
  , pattern MkKindBuiltin
  , TypeBuiltin
  , pattern MkTypeBuiltin
  , ExprBuiltin
  , pattern MkExprBuiltin
  , TypeName
  , pattern MkTypeName
  , ExprName
  , pattern MkExprName
  , TypeBinder
  , pattern MkTypeBinder
  , ExprBinder
  , pattern MkExprBinder
  , Builtin
  , pattern Builtin
  , Name
  , pattern Name
  ) where

import           Vehicle.Core.Type (Sort(..), PlainAnn(..))
import qualified Vehicle.Core.Type as Core

type Kind = Core.PlainKind

pattern KApp k1 k2 = Core.KApp (PlainAnn ()) k1 k2
pattern KCon c     = Core.KCon (PlainAnn ()) c
pattern KMeta u    = Core.KMeta (PlainAnn ()) u

type Type = Core.PlainType

pattern TForall n t = Core.TForall (PlainAnn ()) n t
pattern TApp t1 t2  = Core.TApp (PlainAnn ()) t1 t2
pattern TVar n      = Core.TVar (PlainAnn ()) n
pattern TCon c      = Core.TCon (PlainAnn ()) c
pattern TLitDim d   = Core.TLitDim (PlainAnn ()) d
pattern TLitList ts = Core.TLitList (PlainAnn ()) ts
pattern TMeta u     = Core.TMeta (PlainAnn ()) u

type Expr = Core.PlainExpr

pattern EAnn e t     = Core.EAnn (PlainAnn ()) e t
pattern ELet n e1 e2 = Core.ELet (PlainAnn ()) n e1 e2
pattern ELam n e     = Core.ELam (PlainAnn ()) n e
pattern EApp e1 e2   = Core.EApp (PlainAnn ()) e1 e2
pattern EVar n       = Core.EVar (PlainAnn ()) n
pattern ETyApp e t   = Core.ETyApp (PlainAnn ()) e t
pattern ETyLam n e   = Core.ETyLam (PlainAnn ()) n e
pattern ECon c       = Core.ECon (PlainAnn ()) c
pattern ELitInt i    = Core.ELitInt (PlainAnn ()) i
pattern ELitReal r   = Core.ELitReal (PlainAnn ()) r
pattern ELitSeq es   = Core.ELitSeq (PlainAnn ()) es

type Decl = Core.PlainDecl

pattern DeclNetw n t   = Core.DeclNetw (PlainAnn ()) n t
pattern DeclData n t   = Core.DeclData (PlainAnn ()) n t
pattern DefType n ns t = Core.DefType (PlainAnn ()) n ns t
pattern DefFun n t e   = Core.DefFun (PlainAnn ()) n t e

type Prog = Core.PlainProg

pattern Main ds = Core.Main (PlainAnn ()) ds

type KindBuiltin = Core.PlainBuiltin 'KIND

pattern MkKindBuiltin tk = Core.PlainBuiltin tk

type TypeBuiltin = Core.PlainBuiltin 'TYPE

pattern MkTypeBuiltin tk = Core.PlainBuiltin tk

type ExprBuiltin = Core.PlainBuiltin 'EXPR

pattern MkExprBuiltin tk = Core.PlainBuiltin tk

type TypeName = Core.PlainName 'TYPE

pattern MkTypeName tk = Core.PlainName tk

type ExprName = Core.PlainName 'EXPR

pattern MkExprName tk = Core.PlainName tk

type TypeBinder = Core.PlainTArg

pattern MkTypeBinder tk = Core.TArg (PlainAnn ()) (Core.PlainName tk)

type ExprBinder = Core.PlainEArg

pattern MkExprBinder tk = Core.EArg (PlainAnn ()) (Core.PlainName tk)

type Builtin = Core.BuiltinToken

pattern Builtin tk = Core.BuiltinToken tk

type Name = Core.NameToken

pattern Name tk = Core.NameToken tk
