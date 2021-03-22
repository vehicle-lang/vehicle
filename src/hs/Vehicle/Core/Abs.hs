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
  , SortedBuiltin(..)
  , SortedName(..)
  , NoAnn(..)
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
import           Vehicle.Core.Type (Sort(..), NoAnn, pattern NA)
import qualified Vehicle.Core.Type as Core
import           Vehicle.Prelude.Token (Position)


-- * Abstract syntax tree

type Kind = Core.Kind SortedName SortedBuiltin NoAnn

pattern KApp k1 k2 = Core.KApp NA k1 k2
pattern KCon c     = Core.KCon NA c
pattern KMeta i    = Core.KMeta NA i

type Type = Core.Type SortedName SortedBuiltin NoAnn

pattern TForall n t = Core.TForall NA n t
pattern TApp t1 t2  = Core.TApp NA t1 t2
pattern TVar n      = Core.TVar NA n
pattern TCon c      = Core.TCon NA c
pattern TLitDim d   = Core.TLitDim NA d
pattern TLitList ts = Core.TLitList NA ts
pattern TMeta i     = Core.TMeta NA i

type Expr = Core.Expr SortedName SortedBuiltin NoAnn

pattern EAnn e t     = Core.EAnn NA e t
pattern ELet n e1 e2 = Core.ELet NA n e1 e2
pattern ELam n e     = Core.ELam NA n e
pattern EApp e1 e2   = Core.EApp NA e1 e2
pattern EVar n       = Core.EVar NA n
pattern ETyApp e t   = Core.ETyApp NA e t
pattern ETyLam n e   = Core.ETyLam NA n e
pattern ECon c       = Core.ECon NA c
pattern ELitInt i    = Core.ELitInt NA i
pattern ELitReal r   = Core.ELitReal NA r
pattern ELitSeq es   = Core.ELitSeq NA es

type Decl = Core.Decl SortedName SortedBuiltin NoAnn

pattern DeclNetw n t   = Core.DeclNetw NA n t
pattern DeclData n t   = Core.DeclData NA n t
pattern DefType n ns t = Core.DefType NA n ns t
pattern DefFun n t e   = Core.DefFun NA n t e

type Prog = Core.Prog SortedName SortedBuiltin NoAnn

pattern Main ds = Core.Main NA ds


-- * Tokens

-- ** Lexer tokens

newtype Builtin = Builtin (Position, Text)
  deriving (Eq, Ord, Show, Read)

newtype Name = Name (Position, Text)
  deriving (Eq, Ord, Show, Read)


-- ** Sorted tokens, generic

newtype SortedBuiltin (sort :: Sort)
  = SortedBuiltin Builtin
  deriving (Eq, Ord, Show, Read)

newtype SortedName (sort :: Sort)
  = SortedName Name
  deriving (Eq, Ord, Show, Read)


-- ** Sorted tokens, specialised

type KindBuiltin = SortedBuiltin 'KIND

pattern MkKindBuiltin tk = SortedBuiltin tk

type TypeBuiltin = SortedBuiltin 'TYPE

pattern MkTypeBuiltin tk = SortedBuiltin tk

type ExprBuiltin = SortedBuiltin 'EXPR

pattern MkExprBuiltin tk = SortedBuiltin tk

type TypeName = SortedName 'TYPE

pattern MkTypeName tk = SortedName tk

type ExprName = SortedName 'EXPR

pattern MkExprName tk = SortedName tk

type TypeBinder = Core.TArg SortedName SortedBuiltin NoAnn

pattern MkTypeBinder tk = Core.TArg NA (SortedName tk)

type ExprBinder = Core.EArg SortedName SortedBuiltin NoAnn

pattern MkExprBinder tk = Core.EArg NA (SortedName tk)
