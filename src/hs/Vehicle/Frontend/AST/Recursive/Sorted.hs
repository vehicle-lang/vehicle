{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Vehicle.Frontend.AST.Recursive.Sorted where

import Data.Functor.Identity (Identity(..))
import Vehicle.Frontend.AST.Core ( Tree(..) )
import Vehicle.Prelude (KnownSort(..), SSort(..), Sort(..), Symbol, O(..))


data family TreeF (ann :: Sort -> *) (sort :: Sort) (tree :: Sort -> *)


-- * Base functor for kinds

type KindF ann tree = TreeF ann 'KIND tree

data instance TreeF ann 'KIND tree
  = KAppF  (ann 'KIND) (tree 'KIND) (tree 'KIND)
  | KFunF  (ann 'KIND) (tree 'KIND) (tree 'KIND)
  | KTypeF (ann 'KIND)
  | KDimF  (ann 'KIND)
  | KListF (ann 'KIND)

-- * Base functor for types

type TypeF ann tree = TreeF ann 'TYPE tree

data instance TreeF ann 'TYPE tree
  = TForallF     (ann 'TYPE) [tree 'TARG] (tree 'TYPE)
  | TAppF        (ann 'TYPE) (tree 'TYPE) (tree 'TYPE)
  | TVarF        (ann 'TYPE) Symbol
  | TFunF        (ann 'TYPE) (tree 'TYPE) (tree 'TYPE)
  | TBoolF       (ann 'TYPE)
  | TPropF       (ann 'TYPE)
  | TRealF       (ann 'TYPE)
  | TIntF        (ann 'TYPE)
  | TListF       (ann 'TYPE)
  | TTensorF     (ann 'TYPE)
  | TAddF        (ann 'TYPE) (tree 'TYPE) (tree 'TYPE)
  | TLitDimF     (ann 'TYPE) Integer
  | TConsF       (ann 'TYPE) (tree 'TYPE) (tree 'TYPE)
  | TLitDimListF (ann 'TYPE) [tree 'TYPE]

-- * Base functor for expression arguments

type TArgF ann tree = TreeF ann 'PROG tree

data instance TreeF ann 'TARG tree
  = TArgF (ann 'TARG) Symbol


-- * Base functor for expressions

type ExprF ann tree = TreeF ann 'EXPR tree

data instance TreeF ann 'EXPR tree
  = EAnnF     (ann 'EXPR) (tree 'EXPR) (tree 'TYPE)
  | ELetF     (ann 'EXPR) [tree 'DECL] (tree 'EXPR)
  | ELamF     (ann 'EXPR) [tree 'EARG] (tree 'EXPR)
  | EAppF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EVarF     (ann 'EXPR) Symbol
  | ETyAppF   (ann 'EXPR) (tree 'EXPR) (tree 'TYPE)
  | ETyLamF   (ann 'EXPR) [tree 'TARG] (tree 'EXPR)
  | EIfF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EImplF    (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EAndF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EOrF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | ENotF     (ann 'EXPR) (tree 'EXPR)
  | ETrueF    (ann 'EXPR)
  | EFalseF   (ann 'EXPR)
  | EEqF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | ENeqF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | ELeF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | ELtF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EGeF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EGtF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EMulF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EDivF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EAddF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | ESubF     (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | ENegF     (ann 'EXPR) (tree 'EXPR)
  | ELitIntF  (ann 'EXPR) Integer
  | ELitRealF (ann 'EXPR) Double
  | EConsF    (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EAtF      (ann 'EXPR) (tree 'EXPR) (tree 'EXPR)
  | EAllF     (ann 'EXPR)
  | EAnyF     (ann 'EXPR)
  | ELitSeqF  (ann 'EXPR) [tree 'EXPR]


-- * Base functor for expression arguments

type EArgF ann tree = TreeF ann 'PROG tree

data instance TreeF ann 'EARG tree
  = EArgF (ann 'EARG) Symbol


-- * Base functor for declarations

type DeclF ann tree = TreeF ann 'DECL tree

data instance TreeF ann 'DECL tree
  = DeclNetwF   (ann 'DECL) (tree 'EARG) (tree 'TYPE)
  | DeclDataF   (ann 'DECL) (tree 'EARG) (tree 'TYPE)
  | DefTypeF    (ann 'DECL) (tree 'TARG) [tree 'TARG] (tree 'TYPE)
  | DefFunTypeF (ann 'DECL) (tree 'EARG) (tree 'TYPE)
  | DefFunExprF (ann 'DECL) (tree 'EARG) [tree 'EARG] (tree 'EXPR)


-- * Base functor for programs

type ProgF ann tree = TreeF ann 'PROG tree

data instance TreeF ann 'PROG tree
  = MainF (ann 'PROG) [tree 'DECL]


-- |Unroll a single recursion layer.
project ::
  forall sort ann.
  KnownSort sort =>
  Tree ann sort ->
  TreeF ann sort (Tree ann)
project = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> \case
    KApp  ann k1 k2 -> KAppF  ann k1 k2
    KFun  ann k1 k2 -> KFunF  ann k1 k2
    KType ann       -> KTypeF ann
    KDim  ann       -> KDimF  ann
    KList ann       -> KListF ann

  -- Types
  STYPE -> \case
    TForall     ann ns t  -> TForallF     ann ns t
    TApp        ann t1 t2 -> TAppF        ann t1 t2
    TVar        ann n     -> TVarF        ann n
    TFun        ann t1 t2 -> TFunF        ann t1 t2
    TBool       ann       -> TBoolF       ann
    TProp       ann       -> TPropF       ann
    TReal       ann       -> TRealF       ann
    TInt        ann       -> TIntF        ann
    TList       ann       -> TListF       ann
    TTensor     ann       -> TTensorF     ann
    TAdd        ann t1 t2 -> TAddF        ann t1 t2
    TLitDim     ann i     -> TLitDimF     ann i
    TCons       ann t1 t2 -> TConsF       ann t1 t2
    TLitDimList ann ts    -> TLitDimListF ann ts

  -- Type arguments
  STARG -> \case
    TArg ann n -> TArgF ann n

  -- Expressions
  SEXPR -> \case
    EAnn     ann e t      -> EAnnF     ann e t
    ELet     ann ds e     -> ELetF     ann ds e
    ELam     ann ns e     -> ELamF     ann ns e
    EApp     ann e1 e2    -> EAppF     ann e1 e2
    EVar     ann n        -> EVarF     ann n
    ETyApp   ann e t      -> ETyAppF   ann e t
    ETyLam   ann ns e     -> ETyLamF   ann ns e
    EIf      ann e1 e2 e3 -> EIfF      ann e1 e2 e3
    EImpl    ann e1 e2    -> EImplF    ann e1 e2
    EAnd     ann e1 e2    -> EAndF     ann e1 e2
    EOr      ann e1 e2    -> EOrF      ann e1 e2
    ENot     ann e        -> ENotF     ann e
    ETrue    ann          -> ETrueF    ann
    EFalse   ann          -> EFalseF   ann
    EEq      ann e1 e2    -> EEqF      ann e1 e2
    ENeq     ann e1 e2    -> ENeqF     ann e1 e2
    ELe      ann e1 e2    -> ELeF      ann e1 e2
    ELt      ann e1 e2    -> ELtF      ann e1 e2
    EGe      ann e1 e2    -> EGeF      ann e1 e2
    EGt      ann e1 e2    -> EGtF      ann e1 e2
    EMul     ann e1 e2    -> EMulF     ann e1 e2
    EDiv     ann e1 e2    -> EDivF     ann e1 e2
    EAdd     ann e1 e2    -> EAddF     ann e1 e2
    ESub     ann e1 e2    -> ESubF     ann e1 e2
    ENeg     ann e        -> ENegF     ann e
    ELitInt  ann i        -> ELitIntF  ann i
    ELitReal ann d        -> ELitRealF ann d
    ECons    ann e1 e2    -> EConsF    ann e1 e2
    EAt      ann e1 e2    -> EAtF      ann e1 e2
    EAll     ann          -> EAllF     ann
    EAny     ann          -> EAnyF     ann
    ELitSeq  ann es       -> ELitSeqF  ann es

  -- Expression arguments
  SEARG -> \case
    EArg ann n -> EArgF ann n

  -- Declarations
  SDECL -> \case
    DeclNetw   ann n t    -> DeclNetwF   ann n t
    DeclData   ann n t    -> DeclDataF   ann n t
    DefType    ann n ns t -> DefTypeF    ann n ns t
    DefFunType ann n t    -> DefFunTypeF ann n t
    DefFunExpr ann n ns e -> DefFunExprF ann n ns e

  -- Programs
  SPROG -> \case
    Main ann ds -> MainF ann ds

-- |Roll up a single recursion layer.
embed ::
  forall sort ann.
  KnownSort sort =>
  TreeF ann sort (Tree ann) ->
  Tree ann sort
embed = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> \case
    KAppF  ann k1 k2 -> KApp  ann k1 k2
    KFunF  ann k1 k2 -> KFun  ann k1 k2
    KTypeF ann       -> KType ann
    KDimF  ann       -> KDim  ann
    KListF ann       -> KList ann

  -- Types
  STYPE -> \case
    TForallF     ann ns t  -> TForall     ann ns t
    TAppF        ann t1 t2 -> TApp        ann t1 t2
    TVarF        ann n     -> TVar        ann n
    TFunF        ann t1 t2 -> TFun        ann t1 t2
    TBoolF       ann       -> TBool       ann
    TPropF       ann       -> TProp       ann
    TRealF       ann       -> TReal       ann
    TIntF        ann       -> TInt        ann
    TListF       ann       -> TList       ann
    TTensorF     ann       -> TTensor     ann
    TAddF        ann t1 t2 -> TAdd        ann t1 t2
    TLitDimF     ann i     -> TLitDim     ann i
    TConsF       ann t1 t2 -> TCons       ann t1 t2
    TLitDimListF ann ts    -> TLitDimList ann ts

  -- Type arguments
  STARG -> \case
    TArgF ann n -> TArg ann n

  -- Expressions
  SEXPR -> \case
    EAnnF     ann e t      -> EAnn     ann e t
    ELetF     ann ds e     -> ELet     ann ds e
    ELamF     ann ns e     -> ELam     ann ns e
    EAppF     ann e1 e2    -> EApp     ann e1 e2
    EVarF     ann n        -> EVar     ann n
    ETyAppF   ann e t      -> ETyApp   ann e t
    ETyLamF   ann ns e     -> ETyLam   ann ns e
    EIfF      ann e1 e2 e3 -> EIf      ann e1 e2 e3
    EImplF    ann e1 e2    -> EImpl    ann e1 e2
    EAndF     ann e1 e2    -> EAnd     ann e1 e2
    EOrF      ann e1 e2    -> EOr      ann e1 e2
    ENotF     ann e        -> ENot     ann e
    ETrueF    ann          -> ETrue    ann
    EFalseF   ann          -> EFalse   ann
    EEqF      ann e1 e2    -> EEq      ann e1 e2
    ENeqF     ann e1 e2    -> ENeq     ann e1 e2
    ELeF      ann e1 e2    -> ELe      ann e1 e2
    ELtF      ann e1 e2    -> ELt      ann e1 e2
    EGeF      ann e1 e2    -> EGe      ann e1 e2
    EGtF      ann e1 e2    -> EGt      ann e1 e2
    EMulF     ann e1 e2    -> EMul     ann e1 e2
    EDivF     ann e1 e2    -> EDiv     ann e1 e2
    EAddF     ann e1 e2    -> EAdd     ann e1 e2
    ESubF     ann e1 e2    -> ESub     ann e1 e2
    ENegF     ann e        -> ENeg     ann e
    ELitIntF  ann i        -> ELitInt  ann i
    ELitRealF ann d        -> ELitReal ann d
    EConsF    ann e1 e2    -> ECons    ann e1 e2
    EAtF      ann e1 e2    -> EAt      ann e1 e2
    EAllF     ann          -> EAll     ann
    EAnyF     ann          -> EAny     ann
    ELitSeqF  ann es       -> ELitSeq  ann es

  -- Expression arguments
  SEARG -> \case
    EArgF ann n -> EArg ann n

  -- Declarations
  SDECL -> \case
    DeclNetwF   ann n t    -> DeclNetw   ann n t
    DeclDataF   ann n t    -> DeclData   ann n t
    DefTypeF    ann n ns t -> DefType    ann n ns t
    DefFunTypeF ann n t    -> DefFunType ann n t
    DefFunExprF ann n ns e -> DefFunExpr ann n ns e

  -- Programs
  SPROG -> \case
    MainF ann ds -> Main ann ds

-- |Map each element of the layer to an action, evaluate these actions
-- from left to right, and collect the results.
traverseTreeF ::
  (Applicative f) =>
  (forall sort. KnownSort sort => ann1     sort -> f (ann2     sort)) ->
  (forall sort. KnownSort sort => sorted1  sort -> f (sorted2  sort)) ->
  forall sort. KnownSort sort => TreeF ann1 sort sorted1 ->
  f (TreeF ann2 sort sorted2)

traverseTreeF fAnn fRec (tree :: TreeF ann1 sort sorted1) = case sortSing :: SSort sort of
 -- Kinds
  SKIND -> case tree of
    KAppF  ann k1 k2 -> KAppF  <$> fAnn ann <*> fRec k1 <*> fRec k2
    KFunF  ann k1 k2 -> KFunF  <$> fAnn ann <*> fRec k1 <*> fRec k2
    KTypeF ann       -> KTypeF <$> fAnn ann
    KDimF  ann       -> KDimF  <$> fAnn ann
    KListF ann       -> KListF <$> fAnn ann

  -- Types
  STYPE -> case tree of
    TForallF     ann ns t  -> TForallF     <$> fAnn ann <*> traverse fRec ns <*> fRec t
    TAppF        ann t1 t2 -> TAppF        <$> fAnn ann <*> fRec t1 <*> fRec t2
    TVarF        ann n     -> TVarF        <$> fAnn ann <*> pure n
    TFunF        ann t1 t2 -> TFunF        <$> fAnn ann <*> fRec t1 <*> fRec t2
    TBoolF       ann       -> TBoolF       <$> fAnn ann
    TPropF       ann       -> TPropF       <$> fAnn ann
    TRealF       ann       -> TRealF       <$> fAnn ann
    TIntF        ann       -> TIntF        <$> fAnn ann
    TListF       ann       -> TListF       <$> fAnn ann
    TTensorF     ann       -> TTensorF     <$> fAnn ann
    TAddF        ann t1 t2 -> TAddF        <$> fAnn ann <*> fRec t1 <*> fRec t2
    TLitDimF     ann i     -> TLitDimF     <$> fAnn ann <*> pure i
    TConsF       ann t1 t2 -> TConsF       <$> fAnn ann <*> fRec t1 <*> fRec t2
    TLitDimListF ann ts    -> TLitDimListF <$> fAnn ann <*> traverse fRec ts

  -- Type arguments
  STARG -> case tree of
    TArgF ann n -> TArgF <$> fAnn ann <*> pure n

  -- Expressions
  SEXPR -> case tree of
    EAnnF     ann e t      -> EAnnF     <$> fAnn ann <*> fRec e <*> fRec t
    ELetF     ann ds e     -> ELetF     <$> fAnn ann <*> traverse fRec ds <*> fRec e
    ELamF     ann ns e     -> ELamF     <$> fAnn ann <*> traverse fRec ns <*> fRec e
    EAppF     ann e1 e2    -> EAppF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    EVarF     ann n        -> EVarF     <$> fAnn ann <*> pure n
    ETyAppF   ann e t      -> ETyAppF   <$> fAnn ann <*> fRec e <*> fRec t
    ETyLamF   ann ns e     -> ETyLamF   <$> fAnn ann <*> traverse fRec ns <*> fRec e
    EIfF      ann e1 e2 e3 -> EIfF      <$> fAnn ann <*> fRec e1 <*> fRec e2 <*> fRec e3
    EImplF    ann e1 e2    -> EImplF    <$> fAnn ann <*> fRec e1 <*> fRec e2
    EAndF     ann e1 e2    -> EAndF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    EOrF      ann e1 e2    -> EOrF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    ENotF     ann e        -> ENotF     <$> fAnn ann <*> fRec e
    ETrueF    ann          -> ETrueF    <$> fAnn ann
    EFalseF   ann          -> EFalseF   <$> fAnn ann
    EEqF      ann e1 e2    -> EEqF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    ENeqF     ann e1 e2    -> ENeqF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    ELeF      ann e1 e2    -> ELeF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    ELtF      ann e1 e2    -> ELtF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    EGeF      ann e1 e2    -> EGeF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    EGtF      ann e1 e2    -> EGtF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    EMulF     ann e1 e2    -> EMulF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    EDivF     ann e1 e2    -> EDivF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    EAddF     ann e1 e2    -> EAddF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    ESubF     ann e1 e2    -> ESubF     <$> fAnn ann <*> fRec e1 <*> fRec e2
    ENegF     ann e        -> ENegF     <$> fAnn ann <*> fRec e
    ELitIntF  ann i        -> ELitIntF  <$> fAnn ann <*> pure i
    ELitRealF ann d        -> ELitRealF <$> fAnn ann <*> pure d
    EConsF    ann e1 e2    -> EConsF    <$> fAnn ann <*> fRec e1 <*> fRec e2
    EAtF      ann e1 e2    -> EAtF      <$> fAnn ann <*> fRec e1 <*> fRec e2
    EAllF     ann          -> EAllF     <$> fAnn ann
    EAnyF     ann          -> EAnyF     <$> fAnn ann
    ELitSeqF  ann es       -> ELitSeqF  <$> fAnn ann <*> traverse fRec es

  -- Expression arguments
  SEARG -> case tree of
    EArgF ann n -> EArgF <$> fAnn ann <*> pure n

  -- Declarations
  SDECL -> case tree of
    DeclNetwF   ann n t    -> DeclNetwF   <$> fAnn ann <*> fRec n <*> fRec t
    DeclDataF   ann n t    -> DeclDataF   <$> fAnn ann <*> fRec n <*> fRec t
    DefTypeF    ann n ns t -> DefTypeF    <$> fAnn ann <*> fRec n <*> traverse fRec ns <*> fRec t
    DefFunTypeF ann n t    -> DefFunTypeF <$> fAnn ann <*> fRec n <*> fRec t
    DefFunExprF ann n ns e -> DefFunExprF <$> fAnn ann <*> fRec n <*> traverse fRec ns <*> fRec e

  -- Programs
  SPROG -> case tree of
    MainF ann ds -> MainF <$> fAnn ann <*> traverse fRec ds



-- |Evaluate each action in the layer from left to right, and collect the results.
sequenceTreeF ::
  (Applicative f, KnownSort sort) =>
  TreeF (f `O` ann) sort (f `O` sorted) ->
  f (TreeF ann sort sorted)
sequenceTreeF = traverseTreeF unO unO

-- |Apply a function to each element of the layer.
mapTreeF ::
  (forall sort. KnownSort sort => ann1     sort -> ann2     sort) ->
  (forall sort. KnownSort sort => sorted1  sort -> sorted2  sort) ->
  (forall sort. KnownSort sort => TreeF ann1 sort sorted1 -> TreeF ann2 sort sorted2)
mapTreeF fAnn fRec = runIdentity . traverseTreeF(pure . fAnn) (pure . fRec)

-- |Folds a tree down to a sorted value, one layer at a time.
foldTree ::
  (forall sort. KnownSort sort => TreeF ann sort sorted -> sorted sort) ->
  (forall sort. KnownSort sort => Tree  ann sort        -> sorted sort)
foldTree f = runIdentity . foldTreeM (pure . f)

-- |Effectful version of |foldTree|.
foldTreeM ::
  (Monad m) =>
  (forall sort. KnownSort sort => TreeF ann sort sorted -> m (sorted sort)) ->
  (forall sort. KnownSort sort => Tree  ann sort        -> m (sorted sort))
foldTreeM f tree = f =<< traverseTreeF pure (foldTreeM f) (project tree)

-- * Update fields in one layer

-- |Apply sorted function to the annotation in one layer.
mapTreeFAnn ::
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => TreeF ann sort tree -> TreeF ann' sort tree)
mapTreeFAnn h = mapTreeF h id

-- |Effectful version |mapTreeFAnn|.
traverseTreeFAnn ::
  (Applicative f) =>
  (forall sort. KnownSort sort => ann sort -> f (ann' sort)) ->
  (forall sort. KnownSort sort => TreeF ann sort tree -> f (TreeF ann' sort tree))
traverseTreeFAnn h = traverseTreeF h pure


-- * Update fields in a tree

-- |Apply sorted functions to all annotations in a tree.
mapTreeAnn ::
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => Tree ann sort -> Tree ann' sort)
mapTreeAnn f = runIdentity . traverseTreeAnn (pure . f)

-- |Effectful version of |mapTreeAnn|.
traverseTreeAnn ::
  (Monad m) =>
  (forall sort. KnownSort sort => ann sort -> m (ann' sort)) ->
  (forall sort. KnownSort sort => Tree ann sort -> m (Tree ann' sort))
traverseTreeAnn f = foldTreeM (fmap embed . traverseTreeF f pure)