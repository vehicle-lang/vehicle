{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Vehicle.Core.Type.Recursive where

import Control.Monad.Identity
import Vehicle.Core.Type.Core


-- * Base functor for kinds

data family TreeF (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) (sort :: Sort) (tree :: Sort -> *)

type KindF name builtin ann tree = TreeF name builtin ann 'KIND tree

data instance TreeF name builtin ann 'KIND tree
  = KAppF  (ann 'KIND) (tree 'KIND) (tree 'KIND)
  | KConF  (ann 'KIND) (builtin 'KIND)
  | KMetaF (ann 'KIND) Integer


-- * Base functor for types

type TypeF name builtin ann tree = TreeF name builtin ann 'TYPE tree

data instance TreeF name builtin ann 'TYPE tree
  = TForallF  (ann 'TYPE) (tree 'TARG) (tree 'TYPE)
  | TAppF     (ann 'TYPE) (tree 'TYPE) (tree 'TYPE)
  | TVarF     (ann 'TYPE) (name 'TYPE)
  | TConF     (ann 'TYPE) (builtin 'TYPE)
  | TLitDimF  (ann 'TYPE) Integer
  | TLitListF (ann 'TYPE) [tree 'TYPE]
  | TMetaF    (ann 'TYPE) Integer


-- * Base functor for expressions

type ExprF name builtin ann tree = TreeF name builtin ann 'EXPR tree

data instance TreeF name builtin ann 'EXPR tree
  = EAnnF     (ann 'EXPR) (tree 'EXPR) (tree 'TYPE)
  | ELetF     (ann 'EXPR) (tree 'EARG) (tree 'EXPR) (tree 'EXPR)
  | ELamF     (ann 'EXPR) (tree 'EARG) (tree 'EXPR)
  | EAppF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EVarF     (ann 'EXPR) (name 'EXPR)
  | ETyAppF   (ann 'EXPR) (tree 'EXPR) (tree 'TYPE)
  | ETyLamF   (ann 'EXPR) (tree 'TARG) (tree 'EXPR)
  | EConF     (ann 'EXPR) (builtin 'EXPR)
  | ELitIntF  (ann 'EXPR) Integer
  | ELitRealF (ann 'EXPR) Double
  | ELitSeqF  (ann 'EXPR) [tree 'EXPR]


-- * Base functor for declarations

type DeclF name builtin ann tree = TreeF name builtin ann 'DECL tree

data instance TreeF name builtin ann 'DECL tree
  = DeclNetwF (ann 'DECL) (tree 'EARG) (tree 'TYPE)
  | DeclDataF (ann 'DECL) (tree 'EARG) (tree 'TYPE)
  | DefTypeF  (ann 'DECL) (tree 'TARG) [tree 'TARG] (tree 'TYPE)
  | DefFunF   (ann 'DECL) (tree 'EARG) (tree 'TYPE) (tree 'EXPR)


-- * Base functor for programs

type ProgF name builtin ann tree = TreeF name builtin ann 'PROG tree

data instance TreeF name builtin ann 'PROG tree
  = MainF (ann 'PROG) [tree 'DECL]


-- * Base functor for expression arguments

type TArgF name builtin ann tree = TreeF name builtin ann 'PROG tree

data instance TreeF name builtin ann 'TARG tree
  = TArgF (ann 'TARG) (name 'TARG)


-- * Base functor for expression arguments

type EArgF name builtin ann tree = TreeF name builtin ann 'PROG tree

data instance TreeF name builtin ann 'EARG tree
  = EArgF (ann 'EARG) (name 'EARG)


-- |Unroll a single recursion layer.
project ::
  forall sort name builtin ann.
  KnownSort sort =>
  Tree name builtin ann sort ->
  TreeF name builtin ann sort (Tree name builtin ann)
project = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> \case
    KApp  ann k1 k2 -> KAppF  ann k1 k2
    KCon  ann op    -> KConF  ann op
    KMeta ann i     -> KMetaF ann i

  -- Types
  STYPE -> \case
    TForall  ann n t   -> TForallF  ann n t
    TApp     ann t1 t2 -> TAppF     ann t1 t2
    TVar     ann n     -> TVarF     ann n
    TCon     ann op    -> TConF     ann op
    TLitDim  ann d     -> TLitDimF  ann d
    TLitList ann ts    -> TLitListF ann ts
    TMeta    ann i     -> TMetaF    ann i

  -- Type arguments
  STARG -> \case
    TArg ann n -> TArgF ann n

  -- Expressions
  SEXPR -> \case
    EAnn     ann e t     -> EAnnF     ann e t
    ELet     ann n e1 e2 -> ELetF     ann n e1 e2
    ELam     ann n e     -> ELamF     ann n e
    EApp     ann e1 e2   -> EAppF     ann e1 e2
    EVar     ann n       -> EVarF     ann n
    ETyApp   ann e t     -> ETyAppF   ann e t
    ETyLam   ann n e     -> ETyLamF   ann n e
    ECon     ann op      -> EConF     ann op
    ELitInt  ann z       -> ELitIntF  ann z
    ELitReal ann r       -> ELitRealF ann r
    ELitSeq  ann es      -> ELitSeqF  ann es

  -- Expression arguments
  SEARG -> \case
    EArg ann n -> EArgF ann n

  -- Declarations
  SDECL -> \case
    DeclNetw ann n t    -> DeclNetwF ann n t
    DeclData ann n t    -> DeclDataF ann n t
    DefType  ann n ns t -> DefTypeF  ann n ns t
    DefFun   ann n t e  -> DefFunF   ann n t e

  -- Programs
  SPROG -> \case
    Main ann ds -> MainF ann ds


-- |Roll up a single recursion layer.
embed ::
  forall sort name builtin ann.
  KnownSort sort =>
  TreeF name builtin ann sort (Tree name builtin ann) ->
  Tree name builtin ann sort
embed = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> \case
    KAppF  ann k1 k2 -> KApp  ann k1 k2
    KConF  ann op    -> KCon  ann op
    KMetaF ann i     -> KMeta ann i

  -- Types
  STYPE -> \case
    TForallF  ann n t   -> TForall  ann n t
    TAppF     ann t1 t2 -> TApp     ann t1 t2
    TVarF     ann n     -> TVar     ann n
    TConF     ann op    -> TCon     ann op
    TLitDimF  ann d     -> TLitDim  ann d
    TLitListF ann ts    -> TLitList ann ts
    TMetaF    ann i     -> TMeta    ann i

  -- Type arguments
  STARG -> \case
    TArgF ann n -> TArg ann n

  -- Expressions
  SEXPR -> \case
    EAnnF     ann e t     -> EAnn     ann e t
    ELetF     ann n e1 e2 -> ELet     ann n e1 e2
    ELamF     ann n e     -> ELam     ann n e
    EAppF     ann e1 e2   -> EApp     ann e1 e2
    EVarF     ann n       -> EVar     ann n
    ETyAppF   ann e t     -> ETyApp   ann e t
    ETyLamF   ann n e     -> ETyLam   ann n e
    EConF     ann op      -> ECon     ann op
    ELitIntF  ann z       -> ELitInt  ann z
    ELitRealF ann r       -> ELitReal ann r
    ELitSeqF  ann es      -> ELitSeq  ann es

  -- Expression arguments
  SEARG -> \case
    EArgF ann n -> EArg ann n

  -- Declarations
  SDECL -> \case
    DeclNetwF ann n t    -> DeclNetw ann n t
    DeclDataF ann n t    -> DeclData ann n t
    DefTypeF  ann n ns t -> DefType  ann n ns t
    DefFunF   ann n t e  -> DefFun   ann n t e

  -- Programs
  SPROG -> \case
    MainF ann ds -> Main ann ds

-- |Apply a sorted function to each recursive position.
mapTreeF ::
  (forall sort. KnownSort sort => sorted1 sort -> sorted2 sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort sorted1 -> TreeF name builtin ann sort sorted2)
mapTreeF f tree = runIdentity $ mapTreeFM (pure . f) tree


-- |Effectful version of |mapTreeF|.
mapTreeFM ::
  (Applicative f) =>
  (forall sort. KnownSort sort => sorted1 sort -> f (sorted2 sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort sorted1 -> f (TreeF name builtin ann sort sorted2))

-- NOTE: The effectful version can be implemented in terms of the pure version,
--       similar to how it's done in the recursion-schemes package. However,
--       it's a bit tricky, as we'd have to compute the applicative functor |f|
--       with the sorted type |sorted2|. Therefore, it's easier to define the
--       effectful version, and derive the pure version.

mapTreeFM f (tree :: TreeF _name _builtin _ann sort _sorted1) = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> case tree of
    KAppF  ann k1 k2 -> KAppF ann <$> f k1 <*> f k2
    KConF  ann op    -> pure $ KConF  ann op
    KMetaF ann i     -> pure $ KMetaF ann i

  -- Types
  STYPE -> case tree of
    TForallF  ann n t   -> TForallF ann <$> f n <*> f t
    TAppF     ann t1 t2 -> TAppF ann <$> f t1 <*> f t2
    TVarF     ann n     -> pure $ TVarF ann n
    TConF     ann op    -> pure $ TConF ann op
    TLitDimF  ann d     -> pure $ TLitDimF ann d
    TLitListF ann ts    -> TLitListF ann <$> traverse f ts
    TMetaF    ann i     -> pure $ TMetaF ann i

  -- Type arguments
  STARG -> case tree of
    TArgF ann n -> pure $ TArgF ann n

  -- Expressions
  SEXPR -> case tree of
    EAnnF     ann e t     -> EAnnF ann <$> f e <*> f t
    ELetF     ann n e1 e2 -> ELetF ann <$> f n <*> f e1 <*> f e2
    ELamF     ann n e     -> ELamF ann <$> f n <*> f e
    EAppF     ann e1 e2   -> EAppF ann <$> f e1 <*> f e2
    EVarF     ann n       -> pure $ EVarF ann n
    ETyAppF   ann e t     -> ETyAppF ann <$> f e <*> f t
    ETyLamF   ann n e     -> ETyLamF ann <$> f n <*> f e
    EConF     ann op      -> pure $ EConF ann op
    ELitIntF  ann z       -> pure $ ELitIntF ann z
    ELitRealF ann r       -> pure $ ELitRealF ann r
    ELitSeqF  ann es      -> ELitSeqF ann <$> traverse f es

  -- Expression arguments
  SEARG -> case tree of
    EArgF ann n -> pure $ EArgF ann n

  -- Declarations
  SDECL -> case tree of
    DeclNetwF ann n t    -> DeclNetwF ann <$> f n <*> f t
    DeclDataF ann n t    -> DeclDataF ann <$> f n <*> f t
    DefTypeF  ann n ns t -> DefTypeF ann <$> f n <*> traverse f ns <*> f t
    DefFunF   ann n t e  -> DefFunF ann <$> f n <*> f t <*> f e

  -- Programs
  SPROG -> case tree of
    MainF ann ds -> MainF ann <$> traverse f ds

-- |Folds a tree down to a value, one layer at a time.
foldTree ::
  (forall sort. KnownSort sort => TreeF name builtin ann sort sorted -> sorted sort) ->
  (forall sort. KnownSort sort => Tree  name builtin ann sort        -> sorted sort)
foldTree f tree = runIdentity $ foldTreeM (pure . f) tree

-- |Effectful version of |foldTree|.
foldTreeM ::
  (Monad m) =>
  (forall sort. KnownSort sort => TreeF name builtin ann sort sorted -> m (sorted sort)) ->
  (forall sort. KnownSort sort => Tree  name builtin ann sort        -> m (sorted sort))
foldTreeM f tree = f =<< mapTreeFM (foldTreeM f) (project tree)

-- -}
-- -}
-- -}
-- -}
-- -}
