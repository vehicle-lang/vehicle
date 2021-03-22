{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DataKinds #-} {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provides compatibility between the /more general/ type for Vehicle Core
--   abstract syntax trees defined in 'Vehicle.Core.Type' and the type expected
--   (and generated) by BNFC. The module /must/ be called 'Vehicle.Core.Abs', as
--   this is what BNFC expects (and generates).
module Vehicle.Core.Abs
  ( -- * Abstract syntax tree
    Kind
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
  , Builtin(..)
  , Name(..)
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
  ) where


import           Data.Text (Text)
import           Vehicle.Core.Type (Sort(..), K(..))
import qualified Vehicle.Core.Type as Core
import           Vehicle.Prelude (Position)


-- * Abstract syntax tree

type Kind = Core.Kind (K Name) (K Builtin) (K ())

pattern KApp k1 k2 = Core.KApp (K ()) k1 k2
pattern KCon c     = Core.KCon (K ()) c
pattern KMeta i    = Core.KMeta (K ()) i

type Type = Core.Type (K Name) (K Builtin) (K ())

pattern TForall n t = Core.TForall (K ()) n t
pattern TApp t1 t2  = Core.TApp (K ()) t1 t2
pattern TVar n      = Core.TVar (K ()) n
pattern TCon c      = Core.TCon (K ()) c
pattern TLitDim d   = Core.TLitDim (K ()) d
pattern TLitList ts = Core.TLitList (K ()) ts
pattern TMeta i     = Core.TMeta (K ()) i

type Expr = Core.Expr (K Name) (K Builtin) (K ())

pattern EAnn e t     = Core.EAnn (K ()) e t
pattern ELet n e1 e2 = Core.ELet (K ()) n e1 e2
pattern ELam n e     = Core.ELam (K ()) n e
pattern EApp e1 e2   = Core.EApp (K ()) e1 e2
pattern EVar n       = Core.EVar (K ()) n
pattern ETyApp e t   = Core.ETyApp (K ()) e t
pattern ETyLam n e   = Core.ETyLam (K ()) n e
pattern ECon c       = Core.ECon (K ()) c
pattern ELitInt i    = Core.ELitInt (K ()) i
pattern ELitReal r   = Core.ELitReal (K ()) r
pattern ELitSeq es   = Core.ELitSeq (K ()) es

type Decl = Core.Decl (K Name) (K Builtin) (K ())

pattern DeclNetw n t   = Core.DeclNetw (K ()) n t
pattern DeclData n t   = Core.DeclData (K ()) n t
pattern DefType n ns t = Core.DefType (K ()) n ns t
pattern DefFun n t e   = Core.DefFun (K ()) n t e

type Prog = Core.Prog (K Name) (K Builtin) (K ())

pattern Main ds = Core.Main (K ()) ds


-- * Tokens

-- ** Lexer tokens

newtype Builtin = Builtin (Position, Text)

newtype Name = Name (Position, Text)

-- ** K tokens, generic


-- ** K tokens, specialised

type KindBuiltin = K Builtin 'KIND

pattern MkKindBuiltin tk = K tk


type TypeBuiltin = K Builtin 'TYPE

pattern MkTypeBuiltin tk = K tk


type ExprBuiltin = K Builtin 'EXPR

pattern MkExprBuiltin tk = K tk


type TypeName = K Name 'TYPE

pattern MkTypeName tk = K tk


type ExprName = K Name 'EXPR

pattern MkExprName tk = K tk


type TypeBinder = Core.TArg (K Name) (K Builtin) (K ())

pattern MkTypeBinder tk = Core.TArg (K ()) (K tk)


type ExprBinder = Core.EArg (K Name) (K Builtin) (K ())

pattern MkExprBinder tk = Core.EArg (K ()) (K tk)
