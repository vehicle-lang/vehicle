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
  , SortedAnn(..)
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
import           Vehicle.Core.Type (Sort(..))
import qualified Vehicle.Core.Type as Core
import           Vehicle.Prelude.Token (Position)


-- * Abstract syntax tree

type Kind = Core.Kind SortedName SortedBuiltin SortedAnn

pattern KApp k1 k2 = Core.KApp (SortedAnn ()) k1 k2
pattern KCon c     = Core.KCon (SortedAnn ()) c
pattern KMeta u    = Core.KMeta (SortedAnn ()) u

type Type = Core.Type SortedName SortedBuiltin SortedAnn

pattern TForall n t = Core.TForall (SortedAnn ()) n t
pattern TApp t1 t2  = Core.TApp (SortedAnn ()) t1 t2
pattern TVar n      = Core.TVar (SortedAnn ()) n
pattern TCon c      = Core.TCon (SortedAnn ()) c
pattern TLitDim d   = Core.TLitDim (SortedAnn ()) d
pattern TLitList ts = Core.TLitList (SortedAnn ()) ts
pattern TMeta u     = Core.TMeta (SortedAnn ()) u

type Expr = Core.Expr SortedName SortedBuiltin SortedAnn

pattern EAnn e t     = Core.EAnn (SortedAnn ()) e t
pattern ELet n e1 e2 = Core.ELet (SortedAnn ()) n e1 e2
pattern ELam n e     = Core.ELam (SortedAnn ()) n e
pattern EApp e1 e2   = Core.EApp (SortedAnn ()) e1 e2
pattern EVar n       = Core.EVar (SortedAnn ()) n
pattern ETyApp e t   = Core.ETyApp (SortedAnn ()) e t
pattern ETyLam n e   = Core.ETyLam (SortedAnn ()) n e
pattern ECon c       = Core.ECon (SortedAnn ()) c
pattern ELitInt i    = Core.ELitInt (SortedAnn ()) i
pattern ELitReal r   = Core.ELitReal (SortedAnn ()) r
pattern ELitSeq es   = Core.ELitSeq (SortedAnn ()) es

type Decl = Core.Decl SortedName SortedBuiltin SortedAnn

pattern DeclNetw n t   = Core.DeclNetw (SortedAnn ()) n t
pattern DeclData n t   = Core.DeclData (SortedAnn ()) n t
pattern DefType n ns t = Core.DefType (SortedAnn ()) n ns t
pattern DefFun n t e   = Core.DefFun (SortedAnn ()) n t e

type Prog = Core.Prog SortedName SortedBuiltin SortedAnn

pattern Main ds = Core.Main (SortedAnn ()) ds


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

newtype SortedAnn (sort :: Sort)
  = SortedAnn ()
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

type TypeBinder = Core.TArg SortedName SortedBuiltin SortedAnn

pattern MkTypeBinder tk = Core.TArg (SortedAnn ()) (SortedName tk)

type ExprBinder = Core.EArg SortedName SortedBuiltin SortedAnn

pattern MkExprBinder tk = Core.EArg (SortedAnn ()) (SortedName tk)
