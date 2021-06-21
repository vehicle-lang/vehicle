{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Frontend.AST.Recursive.Unsorted where

import Data.Functor.Identity (Identity(..))
import Vehicle.Frontend.AST.Core ( Tree(..) )
import Vehicle.Prelude (KnownSort(..), SSort(..), Sort(..), Symbol, K(..))
import qualified Vehicle.Frontend.AST.Recursive.Sorted as S (TreeF(..), foldTreeM)

--------------------------------------------------------------------------------
-- An unsorted version of the recursion principle over trees. It would be really
-- nice if there was a way we could derive this from the sorted version
-- automatically but I can't work out how...

data family TreeF (ann :: Sort -> *) (sort :: Sort) (tree :: *)


-- * Base functor for kinds

type KindF ann tree = TreeF ann 'KIND tree

data instance TreeF ann 'KIND tree
  = KAppF  (ann 'KIND) tree tree
  | KFunF  (ann 'KIND) tree tree
  | KTypeF (ann 'KIND)
  | KDimF  (ann 'KIND)
  | KListF (ann 'KIND)

-- * Base functor for types

type TypeF ann tree = TreeF ann 'TYPE tree

data instance TreeF ann 'TYPE tree
  = TForallF     (ann 'TYPE) [tree] tree
  | TAppF        (ann 'TYPE) tree tree
  | TVarF        (ann 'TYPE) Symbol
  | TFunF        (ann 'TYPE) tree tree
  | TBoolF       (ann 'TYPE)
  | TPropF       (ann 'TYPE)
  | TRealF       (ann 'TYPE)
  | TIntF        (ann 'TYPE)
  | TListF       (ann 'TYPE)
  | TTensorF     (ann 'TYPE)
  | TAddF        (ann 'TYPE) tree tree
  | TLitDimF     (ann 'TYPE) Integer
  | TConsF       (ann 'TYPE) tree tree
  | TLitDimListF (ann 'TYPE) [tree]

-- * Base functor for expression arguments

type TArgF ann tree = TreeF ann 'PROG tree

data instance TreeF ann 'TARG tree
  = TArgF (ann 'TARG) Symbol


-- * Base functor for expressions

type ExprF ann tree = TreeF ann 'EXPR tree

data instance TreeF ann 'EXPR tree
  = EAnnF     (ann 'EXPR) tree tree
  | ELetF     (ann 'EXPR) [tree] tree
  | ELamF     (ann 'EXPR) [tree] tree
  | EAppF     (ann 'EXPR) tree tree
  | EVarF     (ann 'EXPR) Symbol
  | ETyAppF   (ann 'EXPR) tree tree
  | ETyLamF   (ann 'EXPR) [tree] tree
  | EIfF      (ann 'EXPR) tree tree tree
  | EImplF    (ann 'EXPR) tree tree
  | EAndF     (ann 'EXPR) tree tree
  | EOrF      (ann 'EXPR) tree tree
  | ENotF     (ann 'EXPR) tree
  | ETrueF    (ann 'EXPR)
  | EFalseF   (ann 'EXPR)
  | EEqF      (ann 'EXPR) tree tree
  | ENeqF     (ann 'EXPR) tree tree
  | ELeF      (ann 'EXPR) tree tree
  | ELtF      (ann 'EXPR) tree tree
  | EGeF      (ann 'EXPR) tree tree
  | EGtF      (ann 'EXPR) tree tree
  | EMulF     (ann 'EXPR) tree tree
  | EDivF     (ann 'EXPR) tree tree
  | EAddF     (ann 'EXPR) tree tree
  | ESubF     (ann 'EXPR) tree tree
  | ENegF     (ann 'EXPR) tree
  | ELitIntF  (ann 'EXPR) Integer
  | ELitRealF (ann 'EXPR) Double
  | EConsF    (ann 'EXPR) tree tree
  | EAtF      (ann 'EXPR) tree tree
  | EAllF     (ann 'EXPR)
  | EAnyF     (ann 'EXPR)
  | ELitSeqF  (ann 'EXPR) [tree]


-- * Base functor for expression arguments

type EArgF ann tree = TreeF ann 'PROG tree

data instance TreeF ann 'EARG tree
  = EArgF (ann 'EARG) Symbol


-- * Base functor for declarations

type DeclF ann tree = TreeF ann 'DECL tree

data instance TreeF ann 'DECL tree
  = DeclNetwF   (ann 'DECL) tree tree
  | DeclDataF   (ann 'DECL) tree tree
  | DefTypeF    (ann 'DECL) tree [tree] tree
  | DefFunTypeF (ann 'DECL) tree tree
  | DefFunExprF (ann 'DECL) tree [tree] tree


-- * Base functor for programs

type ProgF ann tree = TreeF ann 'PROG tree

data instance TreeF ann 'PROG tree
  = MainF (ann 'PROG) [tree]

-- |Map from a sorted functor to an unsorted functor.
mapSorted ::
  (forall sort . KnownSort sort => sorted sort -> value) ->
  forall sort ann.
  KnownSort sort =>
  S.TreeF ann sort sorted ->
  TreeF ann sort value
mapSorted f (tree :: S.TreeF ann1 sort sorted1) = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> case tree of
    S.KAppF  ann k1 k2 -> KAppF  ann (f k1) (f k2)
    S.KFunF  ann k1 k2 -> KFunF  ann (f k1) (f k2)
    S.KTypeF ann       -> KTypeF ann
    S.KDimF  ann       -> KDimF  ann
    S.KListF ann       -> KListF ann

  -- Types
  STYPE -> case tree of
    S.TForallF     ann ns t  -> TForallF     ann (map f ns) (f t)
    S.TAppF        ann t1 t2 -> TAppF        ann (f t1) (f t2)
    S.TVarF        ann n     -> TVarF        ann n
    S.TFunF        ann t1 t2 -> TFunF        ann (f t1) (f t2)
    S.TBoolF       ann       -> TBoolF       ann
    S.TPropF       ann       -> TPropF       ann
    S.TRealF       ann       -> TRealF       ann
    S.TIntF        ann       -> TIntF        ann
    S.TListF       ann       -> TListF       ann
    S.TTensorF     ann       -> TTensorF     ann
    S.TAddF        ann t1 t2 -> TAddF        ann (f t1) (f t2)
    S.TLitDimF     ann i     -> TLitDimF     ann i
    S.TConsF       ann t1 t2 -> TConsF       ann (f t1) (f t2)
    S.TLitDimListF ann ts    -> TLitDimListF ann (map f ts)

  -- Type arguments
  STARG -> case tree of
    S.TArgF ann n -> TArgF ann n

  -- Expressions
  SEXPR -> case tree of
    S.EAnnF     ann e t      -> EAnnF     ann (f e) (f t)
    S.ELetF     ann ds e     -> ELetF     ann (map f ds) (f e)
    S.ELamF     ann ns e     -> ELamF     ann (map f ns) (f e)
    S.EAppF     ann e1 e2    -> EAppF     ann (f e1) (f e2)
    S.EVarF     ann n        -> EVarF     ann n
    S.ETyAppF   ann e t      -> ETyAppF   ann (f e) (f t)
    S.ETyLamF   ann ns e     -> ETyLamF   ann (map f ns) (f e)
    S.EIfF      ann e1 e2 e3 -> EIfF      ann (f e1) (f e2) (f e3)
    S.EImplF    ann e1 e2    -> EImplF    ann (f e1) (f e2)
    S.EAndF     ann e1 e2    -> EAndF     ann (f e1) (f e2)
    S.EOrF      ann e1 e2    -> EOrF      ann (f e1) (f e2)
    S.ENotF     ann e        -> ENotF     ann (f e)
    S.ETrueF    ann          -> ETrueF    ann
    S.EFalseF   ann          -> EFalseF   ann
    S.EEqF      ann e1 e2    -> EEqF      ann (f e1) (f e2)
    S.ENeqF     ann e1 e2    -> ENeqF     ann (f e1) (f e2)
    S.ELeF      ann e1 e2    -> ELeF      ann (f e1) (f e2)
    S.ELtF      ann e1 e2    -> ELtF      ann (f e1) (f e2)
    S.EGeF      ann e1 e2    -> EGeF      ann (f e1) (f e2)
    S.EGtF      ann e1 e2    -> EGtF      ann (f e1) (f e2)
    S.EMulF     ann e1 e2    -> EMulF     ann (f e1) (f e2)
    S.EDivF     ann e1 e2    -> EDivF     ann (f e1) (f e2)
    S.EAddF     ann e1 e2    -> EAddF     ann (f e1) (f e2)
    S.ESubF     ann e1 e2    -> ESubF     ann (f e1) (f e2)
    S.ENegF     ann e        -> ENegF     ann (f e)
    S.ELitIntF  ann i        -> ELitIntF  ann i
    S.ELitRealF ann d        -> ELitRealF ann d
    S.EConsF    ann e1 e2    -> EConsF    ann (f e1) (f e2)
    S.EAtF      ann e1 e2    -> EAtF      ann (f e1) (f e2)
    S.EAllF     ann          -> EAllF     ann
    S.EAnyF     ann          -> EAnyF     ann
    S.ELitSeqF  ann es       -> ELitSeqF  ann (map f es)

  -- Expression arguments
  SEARG -> case tree of
    S.EArgF ann n -> EArgF ann n

  -- Declarations
  SDECL -> case tree of
    S.DeclNetwF   ann n t    -> DeclNetwF   ann (f n) (f t)
    S.DeclDataF   ann n t    -> DeclDataF   ann (f n) (f t)
    S.DefTypeF    ann n ns t -> DefTypeF    ann (f n) (map f ns) (f t)
    S.DefFunTypeF ann n t    -> DefFunTypeF ann (f n) (f t)
    S.DefFunExprF ann n ns e -> DefFunExprF ann (f n) (map f ns) (f e)

  -- Programs
  SPROG -> case tree of
    S.MainF ann ds -> MainF ann (map f ds)

-- |Folds a tree down to a sorted value, one layer at a time.
foldTree ::
  (forall sort. KnownSort sort => TreeF ann sort value -> value) ->
  (forall sort. KnownSort sort => Tree   ann sort       -> value)
foldTree f = runIdentity . foldTreeM (pure . f)

-- |Effectful version of |foldTree|.
foldTreeM ::
  (Monad m) =>
  (forall sort. KnownSort sort => TreeF ann sort value -> m value) ->
  (forall sort. KnownSort sort => Tree   ann sort       -> m value)
foldTreeM f tree = unK <$> S.foldTreeM (\v -> K <$> f (mapSorted unK v)) tree