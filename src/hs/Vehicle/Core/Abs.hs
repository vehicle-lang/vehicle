{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Provides compatibility between the /more general/ type for Vehicle Core
--   abstract syntax trees defined in 'Vehicle.Core.Type' and the type expected
--   (and generated) by BNFC. The module /must/ be called 'Vehicle.Core.Abs', as
--   this is what BNFC expects (and generates).
module Vehicle.Core.Abs
  ( Builtin(..)
  , Name(..)
  , Kind
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
  , TArg
  , pattern MkTArg
  , EArg
  , pattern MkEArg
  ) where

import           Vehicle.Core.Type (Builtin(..), Name(..))
import qualified Vehicle.Core.Type as Core

type Kind = Core.PlainKind

pattern KApp k1 k2 = Core.KApp () k1 k2
pattern KCon c     = Core.KCon () c
pattern KMeta u    = Core.KMeta () u

type Type = Core.PlainType

pattern TForall n t = Core.TForall () n t
pattern TApp t1 t2  = Core.TApp () t1 t2
pattern TVar n      = Core.TVar () n
pattern TCon c      = Core.TCon () c
pattern TLitDim d   = Core.TLitDim () d
pattern TLitList ts = Core.TLitList () ts
pattern TMeta u     = Core.TMeta () u

type Expr = Core.PlainExpr

pattern EAnn e t     = Core.EAnn () e t
pattern ELet n e1 e2 = Core.ELet () n e1 e2
pattern ELam n e     = Core.ELam () n e
pattern EApp e1 e2   = Core.EApp () e1 e2
pattern EVar n       = Core.EVar () n
pattern ETyApp e t   = Core.ETyApp () e t
pattern ETyLam n e   = Core.ETyLam () n e
pattern ECon c       = Core.ECon () c
pattern ELitInt i    = Core.ELitInt () i
pattern ELitReal r   = Core.ELitReal () r
pattern ELitSeq es   = Core.ELitSeq () es

type Decl = Core.PlainDecl

pattern DeclNetw n t   = Core.DeclNetw () n t
pattern DeclData n t   = Core.DeclData () n t
pattern DefType n ns t = Core.DefType () n ns t
pattern DefFun n t e   = Core.DefFun () n t e

type Prog = Core.PlainProg

pattern Main ds = Core.Main () ds

type TArg = Core.PlainTArg

pattern MkTArg n k = Core.TArg () n k

type EArg = Core.PlainEArg

pattern MkEArg n t = Core.EArg () n t
