{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Vehicle.Core.Type.Instance.SortedTrifunctor where

import Control.Monad.Identity
import Data.Functor.Foldable (fold)
import Vehicle.Core.Type.Core
import Vehicle.Core.Type.Instance.Recursive

-- | Listen, this is the sensible name to give to this class, I guess, even
--   though it's a mouthful and it's not really informative. If you've got
--   suggestions, please feel free to change it.
class SortedTrifunctor (tree :: (Sort -> *) -> (Sort -> *) -> (Sort -> *) -> *) where
  sortedMapM :: (Applicative f) =>
                (forall sort. SSort sort -> name sort -> f (name' sort)) ->
                (forall sort. SSort sort -> builtin sort -> f (builtin' sort)) ->
                (forall sort. SSort sort -> ann sort -> f (ann' sort)) ->
                tree name builtin ann -> f (tree name' builtin' ann')

  -- | Pure variant of 'sortedMapM'.
  --   Defaults to running 'sortedMapM' with the 'Identity' functor.
  sortedMap :: (forall sort. SSort sort -> name sort -> name' sort) ->
               (forall sort. SSort sort -> builtin sort -> builtin' sort) ->
               (forall sort. SSort sort -> ann sort -> ann' sort) ->
               tree name builtin ann -> tree name' builtin' ann'
  sortedMap fName fBuiltin fAnn tree =
    runIdentity (sortedMapM fNameM fBuiltinM fAnnM tree)
    where
      fNameM    ssort name    = pure (fName    ssort name)
      fBuiltinM ssort builtin = pure (fBuiltin ssort builtin)
      fAnnM     ssort ann     = pure (fAnn     ssort ann)

instance SortedTrifunctor Kind where
  sortedMapM _fName fBuiltin fAnn = fold $ \case
    KAppF  ann k1 k2 -> KApp  <$> fAnn SKIND ann <*> k1 <*> k2
    KConF  ann op    -> KCon  <$> fAnn SKIND ann <*> fBuiltin SKIND op
    KMetaF ann i     -> KMeta <$> fAnn SKIND ann <*> pure i

instance SortedTrifunctor Type where
  sortedMapM fName fBuiltin fAnn = fold $ \case
    TForallF  ann n t   -> TForall  <$> fAnn STYPE ann <*> sortedMapM fName fBuiltin fAnn n <*> t
    TAppF     ann t1 t2 -> TApp     <$> fAnn STYPE ann <*> t1 <*> t2
    TVarF     ann n     -> TVar     <$> fAnn STYPE ann <*> fName STYPE n
    TConF     ann op    -> TCon     <$> fAnn STYPE ann <*> fBuiltin STYPE op
    TLitDimF  ann d     -> TLitDim  <$> fAnn STYPE ann <*> pure d
    TLitListF ann ts    -> TLitList <$> fAnn STYPE ann <*> sequenceA ts
    TMetaF    ann i     -> TMeta    <$> fAnn STYPE ann <*> pure i

instance SortedTrifunctor Expr where
  sortedMapM fName fBuiltin fAnn = fold $ \case
    EAnnF     ann e t     -> EAnn     <$> fAnn SEXPR ann <*> e <*> sortedMapM fName fBuiltin fAnn t
    ELetF     ann n e1 e2 -> ELet     <$> fAnn SEXPR ann <*> sortedMapM fName fBuiltin fAnn n <*> e1 <*> e2
    ELamF     ann n e     -> ELam     <$> fAnn SEXPR ann <*> sortedMapM fName fBuiltin fAnn n <*> e
    EAppF     ann e1 e2   -> EApp     <$> fAnn SEXPR ann <*> e1 <*> e2
    EVarF     ann n       -> EVar     <$> fAnn SEXPR ann <*> fName SEXPR n
    ETyAppF   ann e t     -> ETyApp   <$> fAnn SEXPR ann <*> e <*> sortedMapM fName fBuiltin fAnn t
    ETyLamF   ann n e     -> ETyLam   <$> fAnn SEXPR ann <*> sortedMapM fName fBuiltin fAnn n <*> e
    EConF     ann op      -> ECon     <$> fAnn SEXPR ann <*> fBuiltin SEXPR op
    ELitIntF  ann z       -> ELitInt  <$> fAnn SEXPR ann <*> pure z
    ELitRealF ann r       -> ELitReal <$> fAnn SEXPR ann <*> pure r
    ELitSeqF  ann es      -> ELitSeq  <$> fAnn SEXPR ann <*> sequenceA es

instance SortedTrifunctor Decl where
  sortedMapM fName fBuiltin fAnn = \case
    DeclNetw ann n t    -> DeclNetw <$> fAnn SDECL ann <*>
                           sortedMapM fName fBuiltin fAnn n <*>
                           sortedMapM fName fBuiltin fAnn t
    DeclData ann n t    -> DeclData <$> fAnn SDECL ann <*>
                           sortedMapM fName fBuiltin fAnn n <*>
                           sortedMapM fName fBuiltin fAnn t
    DefType  ann n ns t -> DefType <$> fAnn SDECL ann <*>
                           sortedMapM fName fBuiltin fAnn n <*>
                           traverse (sortedMapM fName fBuiltin fAnn) ns <*>
                           sortedMapM fName fBuiltin fAnn t
    DefFun   ann n t e  -> DefFun <$> fAnn SDECL ann <*>
                           sortedMapM fName fBuiltin fAnn n <*>
                           sortedMapM fName fBuiltin fAnn t <*>
                           sortedMapM fName fBuiltin fAnn e

instance SortedTrifunctor Prog where
  sortedMapM fName fBuiltin fAnn = \case
    Main ann ds -> Main <$> fAnn SPROG ann <*> traverse (sortedMapM fName fBuiltin fAnn) ds

instance SortedTrifunctor TArg where
  sortedMapM fName _fBuiltin fAnn = \case
    TArg ann n -> TArg <$> fAnn STARG ann <*> fName STARG n

instance SortedTrifunctor EArg where
  sortedMapM fName _fBuiltin fAnn = \case
    EArg ann n -> EArg <$> fAnn SEARG ann <*> fName SEARG n


mapNameM :: (SortedTrifunctor tree, Applicative f) =>
            (forall sort. SSort sort -> name sort -> f (name' sort)) ->
            tree name builtin ann -> f (tree name' builtin ann)
mapNameM fName = sortedMapM fName (const pure) (const pure)

mapName :: (SortedTrifunctor tree) =>
           (forall sort. SSort sort -> name sort -> name' sort) ->
           tree name builtin ann -> tree name' builtin ann
mapName fName = sortedMap fName (const id) (const id)

mapBuiltinM :: (SortedTrifunctor tree, Applicative f) =>
               (forall sort. SSort sort -> builtin sort -> f (builtin' sort)) ->
               tree name builtin ann -> f (tree name builtin' ann)
mapBuiltinM fBuiltin = sortedMapM (const pure) fBuiltin (const pure)

mapBuiltin :: (SortedTrifunctor tree) =>
              (forall sort. SSort sort -> builtin sort -> builtin' sort) ->
              tree name builtin ann -> tree name builtin' ann
mapBuiltin fBuiltin = sortedMap (const id) fBuiltin (const id)

mapAnnM :: (SortedTrifunctor tree, Applicative f) =>
            (forall sort. SSort sort -> ann sort -> f (ann' sort)) ->
            tree name builtin ann -> f (tree name builtin ann')
mapAnnM fAnn = sortedMapM (const pure) (const pure) fAnn

mapAnn :: (SortedTrifunctor tree) =>
          (forall sort. SSort sort -> ann sort -> ann' sort) ->
          tree name builtin ann -> tree name builtin ann'
mapAnn fAnn = sortedMap (const id) (const id) fAnn
