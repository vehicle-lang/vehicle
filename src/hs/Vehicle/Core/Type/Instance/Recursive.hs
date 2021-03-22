{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Vehicle.Core.Type.Instance.Recursive where

import Data.Functor.Foldable (Base, Recursive(..), Corecursive(..))
import Vehicle.Core.Type.Core


-- * Base functor for kinds

data family TreeF (sort :: Sort) (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) (x :: *)

type KindF name builtin ann x = TreeF 'KIND name builtin ann x

data instance TreeF 'KIND name builtin ann x
  = KAppF (ann 'KIND) x x
  | KConF (ann 'KIND) (builtin 'KIND)
  | KMetaF (ann 'KIND) Integer
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'KIND name builtin ann) = TreeF 'KIND name builtin ann

instance Recursive (Kind name builtin ann) where
  project (KApp  ann k1 k2) = KAppF  ann k1 k2
  project (KCon  ann op)    = KConF  ann op
  project (KMeta ann i)     = KMetaF ann i

instance Corecursive (Kind name builtin ann) where
  embed (KAppF  ann k1 k2) = KApp  ann k1 k2
  embed (KConF  ann op)    = KCon  ann op
  embed (KMetaF ann i)     = KMeta ann i


-- * Base functor for types

type TypeF name builtin ann x = TreeF 'TYPE name builtin ann x

data instance TreeF 'TYPE name builtin ann x
  = TForallF  (ann 'TYPE) (TArg name builtin ann) x
  | TAppF     (ann 'TYPE) x x
  | TVarF     (ann 'TYPE) (name 'TYPE)
  | TConF     (ann 'TYPE) (builtin 'TYPE)
  | TLitDimF  (ann 'TYPE) Integer
  | TLitListF (ann 'TYPE) [x]
  | TMetaF    (ann 'TYPE) Integer
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'TYPE name builtin ann) = TreeF 'TYPE name builtin ann

instance Recursive (Type name builtin ann) where
  project (TForall  ann n t)   = TForallF  ann n t
  project (TApp     ann t1 t2) = TAppF     ann t1 t2
  project (TVar     ann n)     = TVarF     ann n
  project (TCon     ann op)    = TConF     ann op
  project (TLitDim  ann d)     = TLitDimF  ann d
  project (TLitList ann ts)    = TLitListF ann ts
  project (TMeta    ann i)     = TMetaF    ann i

instance Corecursive (Type name builtin ann) where
  embed (TForallF  ann n t)   = TForall  ann n t
  embed (TAppF     ann t1 t2) = TApp     ann t1 t2
  embed (TVarF     ann n)     = TVar     ann n
  embed (TConF     ann op)    = TCon     ann op
  embed (TLitDimF  ann d)     = TLitDim  ann d
  embed (TLitListF ann ts)    = TLitList ann ts
  embed (TMetaF    ann i)     = TMeta    ann i


-- * Base functor for expressions

type ExprF name builtin ann x = TreeF 'EXPR name builtin ann x

data instance TreeF 'EXPR name builtin ann x
  = EAnnF     (ann 'EXPR) x (Type name builtin ann)
  | ELetF     (ann 'EXPR) (EArg name builtin ann) x x
  | ELamF     (ann 'EXPR) (EArg name builtin ann) x
  | EAppF     (ann 'EXPR) x x
  | EVarF     (ann 'EXPR) (name 'EXPR)
  | ETyAppF   (ann 'EXPR) x (Type name builtin ann)
  | ETyLamF   (ann 'EXPR) (TArg name builtin ann) x
  | EConF     (ann 'EXPR) (builtin 'EXPR)
  | ELitIntF  (ann 'EXPR) Integer
  | ELitRealF (ann 'EXPR) Double
  | ELitSeqF  (ann 'EXPR) [x]
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'EXPR name builtin ann) = TreeF 'EXPR name builtin ann

instance Recursive (Expr name builtin ann) where
  project (EAnn     ann e t)     = EAnnF     ann e t
  project (ELet     ann n e1 e2) = ELetF     ann n e1 e2
  project (ELam     ann n e)     = ELamF     ann n e
  project (EApp     ann e1 e2)   = EAppF     ann e1 e2
  project (EVar     ann n)       = EVarF     ann n
  project (ETyApp   ann e t)     = ETyAppF   ann e t
  project (ETyLam   ann n e)     = ETyLamF   ann n e
  project (ECon     ann op)      = EConF     ann op
  project (ELitInt  ann z)       = ELitIntF  ann z
  project (ELitReal ann r)       = ELitRealF ann r
  project (ELitSeq  ann es)      = ELitSeqF  ann es

instance Corecursive (Expr name builtin ann) where
  embed (EAnnF     ann e t)     = EAnn     ann e t
  embed (ELetF     ann n e1 e2) = ELet     ann n e1 e2
  embed (ELamF     ann n e)     = ELam     ann n e
  embed (EAppF     ann e1 e2)   = EApp     ann e1 e2
  embed (EVarF     ann n)       = EVar     ann n
  embed (ETyAppF   ann e t)     = ETyApp   ann e t
  embed (ETyLamF   ann n e)     = ETyLam   ann n e
  embed (EConF     ann op)      = ECon     ann op
  embed (ELitIntF  ann z)       = ELitInt  ann z
  embed (ELitRealF ann r)       = ELitReal ann r
  embed (ELitSeqF  ann es)      = ELitSeq  ann es


-- * Base functor for declarations

type DeclF name builtin ann x = TreeF 'DECL name builtin ann x

data instance TreeF 'DECL name builtin ann x
  = DeclNetwF (ann 'DECL) (EArg name builtin ann) (Type name builtin ann)
  | DeclDataF (ann 'DECL) (EArg name builtin ann) (Type name builtin ann)
  | DefTypeF  (ann 'DECL) (TArg name builtin ann) [TArg name builtin ann] (Type name builtin ann)
  | DefFunF   (ann 'DECL) (EArg name builtin ann) (Type name builtin ann) (Expr name builtin ann)
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'DECL name builtin ann) = TreeF 'DECL name builtin ann

instance Recursive (Decl name builtin ann) where
  project (DeclNetw ann n t)    = DeclNetwF ann n t
  project (DeclData ann n t)    = DeclDataF ann n t
  project (DefType  ann n ns t) = DefTypeF  ann n ns t
  project (DefFun   ann n t e)  = DefFunF   ann n t e

instance Corecursive (Decl name builtin ann) where
  embed (DeclNetwF ann n t)    = DeclNetw ann n t
  embed (DeclDataF ann n t)    = DeclData ann n t
  embed (DefTypeF  ann n ns t) = DefType  ann n ns t
  embed (DefFunF   ann n t e)  = DefFun   ann n t e


-- * Base functor for programs

type ProgF name builtin ann x = TreeF 'PROG name builtin ann x

data instance TreeF 'PROG name builtin ann x
  = MainF (ann 'PROG) [Decl name builtin ann]
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'PROG name builtin ann) = TreeF 'PROG name builtin ann

instance Recursive (Prog name builtin ann) where
  project (Main ann ds) = MainF ann ds

instance Corecursive (Prog name builtin ann) where
  embed (MainF ann ds) = Main ann ds


-- * Base functor for expression arguments

type TArgF name builtin ann x = TreeF 'PROG name builtin ann x

data instance TreeF 'TARG name builtin ann x
  = TArgF (ann 'TARG) (name 'TARG)
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'TARG name builtin ann) = TreeF 'TARG name builtin ann

instance Recursive (TArg name builtin ann) where
  project (TArg ann ds) = TArgF ann ds

instance Corecursive (TArg name builtin ann) where
  embed (TArgF ann ds) = TArg ann ds


-- * Base functor for expression arguments

type EArgF name builtin ann x = TreeF 'PROG name builtin ann x

data instance TreeF 'EARG name builtin ann x
  = EArgF (ann 'EARG) (name 'EARG)
  deriving (Functor, Foldable, Traversable)

type instance Base (Tree 'EARG name builtin ann) = TreeF 'EARG name builtin ann

instance Recursive (EArg name builtin ann) where
  project (EArg ann ds) = EArgF ann ds

instance Corecursive (EArg name builtin ann) where
  embed (EArgF ann ds) = EArg ann ds


-- -}
-- -}
-- -}
-- -}
-- -}
