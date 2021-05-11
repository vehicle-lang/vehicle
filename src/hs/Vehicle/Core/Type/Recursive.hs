{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Vehicle.Core.Type.Recursive where

import           Control.Monad.Identity
import           Vehicle.Core.Type.Core
import           Vehicle.Prelude


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

-- |Map each element of the layer to an action, evaluate these actions from left to right, and collect the results.
traverseTreeF ::
  (Applicative f) =>
  (forall sort. KnownSort sort => name1    sort -> f (name2    sort)) ->
  (forall sort. KnownSort sort => builtin1 sort -> f (builtin2 sort)) ->
  (forall sort. KnownSort sort => ann1     sort -> f (ann2     sort)) ->
  (forall sort. KnownSort sort => sorted1  sort -> f (sorted2  sort)) ->
  (forall sort. KnownSort sort => TreeF name1 builtin1 ann1 sort sorted1 -> f (TreeF name2 builtin2 ann2 sort sorted2))

traverseTreeF fName fBuiltin fAnn fRec (tree :: TreeF name1 builtin1 ann1 sort sorted1) = case sortSing :: SSort sort of

 -- Kinds
  SKIND -> case tree of
    KAppF  ann k1 k2 -> KAppF  <$> fAnn ann <*> fRec k1 <*> fRec k2
    KConF  ann op    -> KConF  <$> fAnn ann <*> fBuiltin op
    KMetaF ann i     -> KMetaF <$> fAnn ann <*> pure i

  -- Types
  STYPE -> case tree of
    TForallF  ann n t   -> TForallF  <$> fAnn ann <*> fRec n <*> fRec t
    TAppF     ann t1 t2 -> TAppF     <$> fAnn ann <*> fRec t1 <*> fRec t2
    TVarF     ann n     -> TVarF     <$> fAnn ann <*> fName n
    TConF     ann op    -> TConF     <$> fAnn ann <*> fBuiltin op
    TLitDimF  ann d     -> TLitDimF  <$> fAnn ann <*> pure d
    TLitListF ann ts    -> TLitListF <$> fAnn ann <*> traverse fRec ts
    TMetaF    ann i     -> TMetaF    <$> fAnn ann <*> pure i

  -- Type arguments
  STARG -> case tree of
    TArgF ann n -> TArgF <$> fAnn ann <*> fName n

  -- Expressions
  SEXPR -> case tree of
    EAnnF     ann e t     -> EAnnF     <$> fAnn ann <*> fRec e <*> fRec t
    ELetF     ann n e1 e2 -> ELetF     <$> fAnn ann <*> fRec n <*> fRec e1 <*> fRec e2
    ELamF     ann n e     -> ELamF     <$> fAnn ann <*> fRec n <*> fRec e
    EAppF     ann e1 e2   -> EAppF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    EVarF     ann n       -> EVarF     <$> fAnn ann <*> fName n
    ETyAppF   ann e t     -> ETyAppF   <$> fAnn ann <*> fRec e <*> fRec t
    ETyLamF   ann n e     -> ETyLamF   <$> fAnn ann <*> fRec n <*> fRec e
    EConF     ann op      -> EConF     <$> fAnn ann <*> fBuiltin op
    ELitIntF  ann z       -> ELitIntF  <$> fAnn ann <*> pure z
    ELitRealF ann r       -> ELitRealF <$> fAnn ann <*> pure r
    ELitSeqF  ann es      -> ELitSeqF  <$> fAnn ann <*> traverse fRec es

  -- Expression arguments
  SEARG -> case tree of
    EArgF ann n -> EArgF <$> fAnn ann <*> fName n

  -- Declarations
  SDECL -> case tree of
    DeclNetwF ann n t    -> DeclNetwF <$> fAnn ann <*> fRec n <*> fRec t
    DeclDataF ann n t    -> DeclDataF <$> fAnn ann <*> fRec n <*> fRec t
    DefTypeF  ann n ns t -> DefTypeF  <$> fAnn ann <*> fRec n <*> traverse fRec ns <*> fRec t
    DefFunF   ann n t e  -> DefFunF   <$> fAnn ann <*> fRec n <*> fRec t <*> fRec e

  -- Programs
  SPROG -> case tree of
    MainF ann ds -> MainF <$> fAnn ann <*> traverse fRec ds

-- |Evaluate each action in the layer from left to right, and collect the results.
sequenceTreeF ::
  (Applicative f, KnownSort sort) =>
  TreeF (f `O` name) (f `O` builtin) (f `O` ann) sort (f `O` sorted) ->
  f (TreeF name builtin ann sort sorted)

sequenceTreeF = traverseTreeF unO unO unO unO

-- |Apply a function to each element of the layer.
mapTreeF ::
  (forall sort. KnownSort sort => name1    sort -> name2    sort) ->
  (forall sort. KnownSort sort => builtin1 sort -> builtin2 sort) ->
  (forall sort. KnownSort sort => ann1     sort -> ann2     sort) ->
  (forall sort. KnownSort sort => sorted1  sort -> sorted2  sort) ->
  (forall sort. KnownSort sort => TreeF name1 builtin1 ann1 sort sorted1 -> TreeF name2 builtin2 ann2 sort sorted2)

mapTreeF fName fBuiltin fAnn fRec =
  runIdentity . traverseTreeF (pure . fName) (pure . fBuiltin) (pure . fAnn) (pure . fRec)

-- |Folds a tree down to a sorted value, one layer at a time.
foldTree ::
  (forall sort. KnownSort sort => TreeF name builtin ann sort sorted -> sorted sort) ->
  (forall sort. KnownSort sort => Tree  name builtin ann sort        -> sorted sort)
foldTree f = runIdentity . foldTreeM (pure . f)

-- |Effectful version of |foldTree|.
foldTreeM ::
  (Monad m) =>
  (forall sort. KnownSort sort => TreeF name builtin ann sort sorted -> m (sorted sort)) ->
  (forall sort. KnownSort sort => Tree  name builtin ann sort        -> m (sorted sort))
foldTreeM f tree = f =<< traverseTreeF pure pure pure (foldTreeM f) (project tree)

-- |Folds a tree down to a sorted value, one layer at a time.
foldTreeO ::
  (Applicative f) =>
  (forall sort. KnownSort sort => TreeF name builtin ann sort (f `O` sorted) -> f (sorted sort)) ->
  (forall sort. KnownSort sort => Tree  name builtin ann sort                -> f (sorted sort))
foldTreeO f = unO . foldTree (O . f)


-- * Update fields in one layer

-- |Apply sorted function to the names in one layer.
mapTreeFName ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name' builtin ann sort tree)

mapTreeFName f = mapTreeF f id id id

-- |Effectful version |mapTreeFName|.
traverseTreeFName ::
  (Applicative f) =>
  (forall sort. KnownSort sort => name sort -> f (name' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name' builtin ann sort tree))

traverseTreeFName f = traverseTreeF f pure pure pure

-- |Apply sorted function to the builtins in one layer.
mapTreeFBuiltin ::
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name builtin' ann sort tree)

mapTreeFBuiltin g = mapTreeF id g id id

-- |Effectful version |mapTreeFBuiltin|.
traverseTreeFBuiltin ::
  (Applicative f) =>
  (forall sort. KnownSort sort => builtin sort -> f (builtin' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name builtin' ann sort tree))

traverseTreeFBuiltin g = traverseTreeF pure g pure pure

-- |Apply sorted function to the annotation in one layer.
mapTreeFAnn ::
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name builtin ann' sort tree)

mapTreeFAnn h = mapTreeF id id h id

-- |Effectful version |mapTreeFAnn|.
traverseTreeFAnn ::
  (Applicative f) =>
  (forall sort. KnownSort sort => ann sort -> f (ann' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name builtin ann' sort tree))

traverseTreeFAnn h = traverseTreeF pure pure h pure


-- * Update fields in a tree

-- |Apply sorted functions to all names, builtins, and annotations in a tree.
mapTreeFields ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name' builtin' ann' sort)

mapTreeFields f g h = runIdentity . traverseTreeFields (pure . f) (pure . g) (pure . h)

-- |Effectful version of |mapFields|.
traverseTreeFields ::
  (Monad m) =>
  (forall sort. KnownSort sort => name1    sort -> m (name2    sort)) ->
  (forall sort. KnownSort sort => builtin1 sort -> m (builtin2 sort)) ->
  (forall sort. KnownSort sort => ann1     sort -> m (ann2     sort)) ->
  (forall sort. KnownSort sort => Tree name1 builtin1 ann1 sort -> m (Tree name2 builtin2 ann2 sort))

traverseTreeFields f g h = foldTreeM (fmap embed . traverseTreeF f g h pure)

-- |Apply sorted functions to all names in a tree.
mapTreeName ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name' builtin ann sort)
mapTreeName f = mapTreeFields f id id

-- |Effectful version of |mapTreeName|.
traverseTreeName ::
  (Monad m) =>
  (forall sort. KnownSort sort => name sort -> m (name' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name' builtin ann sort))
traverseTreeName f = traverseTreeFields f pure pure

-- |Apply sorted functions to all builtins in a tree.
mapTreeBuiltin ::
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name builtin' ann sort)
mapTreeBuiltin g = mapTreeFields id g id

-- |Effectful version of |mapTreeBuiltin|.
traverseTreeBuiltin ::
  (Monad m) =>
  (forall sort. KnownSort sort => builtin sort -> m (builtin' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name builtin' ann sort))
traverseTreeBuiltin g = traverseTreeFields pure g pure

-- |Apply sorted functions to all annotations in a tree.
mapTreeAnn ::
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name builtin ann' sort)
mapTreeAnn h = mapTreeFields id id h

-- |Effectful version of |mapTreeAnn|.
traverseTreeAnn ::
  (Monad m) =>
  (forall sort. KnownSort sort => ann sort -> m (ann' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name builtin ann' sort))
traverseTreeAnn h = traverseTreeFields pure pure h

-- -}
-- -}
-- -}
-- -}
-- -}
