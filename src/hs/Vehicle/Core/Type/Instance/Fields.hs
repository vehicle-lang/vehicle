{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.Type.Instance.Fields where

import Control.Monad.Identity
import Data.Functor.Foldable (fold)
import Vehicle.Core.Type.Core
import Vehicle.Core.Type.Instance.Recursive

mapFieldsM ::
  forall sort name builtin ann name' builtin' ann' f.
  (Applicative f, KnownSort sort) =>
  (forall sort. KnownSort sort => name sort -> f (name' sort)) ->
  (forall sort. KnownSort sort => builtin sort -> f (builtin' sort)) ->
  (forall sort. KnownSort sort => ann sort -> f (ann' sort)) ->
  Tree sort name builtin ann -> f (Tree sort name' builtin' ann')
mapFieldsM fName fBuiltin fAnn = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> fold $ \case
    KAppF  ann k1 k2 -> KApp  <$> fAnn ann <*> k1 <*> k2
    KConF  ann op    -> KCon  <$> fAnn ann <*> fBuiltin op
    KMetaF ann i     -> KMeta <$> fAnn ann <*> pure i

  -- Types
  STYPE -> fold $ \case
    TForallF  ann n t   -> TForall  <$> fAnn ann <*> mapFieldsM fName fBuiltin fAnn n <*> t
    TAppF     ann t1 t2 -> TApp     <$> fAnn ann <*> t1 <*> t2
    TVarF     ann n     -> TVar     <$> fAnn ann <*> fName n
    TConF     ann op    -> TCon     <$> fAnn ann <*> fBuiltin op
    TLitDimF  ann d     -> TLitDim  <$> fAnn ann <*> pure d
    TLitListF ann ts    -> TLitList <$> fAnn ann <*> sequenceA ts
    TMetaF    ann i     -> TMeta    <$> fAnn ann <*> pure i

  -- Type arguments
  STARG -> \case
    TArg ann n -> TArg <$> fAnn ann <*> fName n

  -- Expressions
  SEXPR -> fold $ \case
    EAnnF     ann e t     -> EAnn     <$> fAnn ann <*> e <*> mapFieldsM fName fBuiltin fAnn t
    ELetF     ann n e1 e2 -> ELet     <$> fAnn ann <*> mapFieldsM fName fBuiltin fAnn n <*> e1 <*> e2
    ELamF     ann n e     -> ELam     <$> fAnn ann <*> mapFieldsM fName fBuiltin fAnn n <*> e
    EAppF     ann e1 e2   -> EApp     <$> fAnn ann <*> e1 <*> e2
    EVarF     ann n       -> EVar     <$> fAnn ann <*> fName n
    ETyAppF   ann e t     -> ETyApp   <$> fAnn ann <*> e <*> mapFieldsM fName fBuiltin fAnn t
    ETyLamF   ann n e     -> ETyLam   <$> fAnn ann <*> mapFieldsM fName fBuiltin fAnn n <*> e
    EConF     ann op      -> ECon     <$> fAnn ann <*> fBuiltin op
    ELitIntF  ann z       -> ELitInt  <$> fAnn ann <*> pure z
    ELitRealF ann r       -> ELitReal <$> fAnn ann <*> pure r
    ELitSeqF  ann es      -> ELitSeq  <$> fAnn ann <*> sequenceA es

  -- Expression arguments
  SEARG -> \case
    EArg ann n -> EArg <$> fAnn ann <*> fName n

  -- Declarations
  SDECL -> \case
    DeclNetw ann n t    -> DeclNetw <$> fAnn ann <*>
                           mapFieldsM fName fBuiltin fAnn n <*>
                           mapFieldsM fName fBuiltin fAnn t
    DeclData ann n t    -> DeclData <$> fAnn ann <*>
                           mapFieldsM fName fBuiltin fAnn n <*>
                           mapFieldsM fName fBuiltin fAnn t
    DefType  ann n ns t -> DefType <$> fAnn ann <*>
                           mapFieldsM fName fBuiltin fAnn n <*>
                           traverse (mapFieldsM fName fBuiltin fAnn) ns <*>
                           mapFieldsM fName fBuiltin fAnn t
    DefFun   ann n t e  -> DefFun <$> fAnn ann <*>
                           mapFieldsM fName fBuiltin fAnn n <*>
                           mapFieldsM fName fBuiltin fAnn t <*>
                           mapFieldsM fName fBuiltin fAnn e

  -- Programs
  SPROG -> \case
    Main ann ds -> Main <$> fAnn ann <*> traverse (mapFieldsM fName fBuiltin fAnn) ds


mapFields ::
  (KnownSort sort) =>
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  Tree sort name builtin ann -> Tree sort name' builtin' ann'
mapFields fName fBuiltin fAnn tree =
  runIdentity $ mapFieldsM (pure . fName) (pure . fBuiltin) (pure . fAnn) tree

mapNameM ::
  (Applicative f, KnownSort sort) =>
  (forall sort. KnownSort sort => name sort -> f (name' sort)) ->
  Tree sort name builtin ann -> f (Tree sort name' builtin ann)
mapNameM fName = mapFieldsM fName pure pure

mapName ::
  (KnownSort sort) =>
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  Tree sort name builtin ann -> Tree sort name' builtin ann
mapName fName = mapFields fName id id

mapBuiltinM ::
  (Applicative f, KnownSort sort) =>
  (forall sort. KnownSort sort => builtin sort -> f (builtin' sort)) ->
  Tree sort name builtin ann -> f (Tree sort name builtin' ann)
mapBuiltinM fBuiltin = mapFieldsM pure fBuiltin pure

mapBuiltin ::
  (KnownSort sort) =>
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  Tree sort name builtin ann -> Tree sort name builtin' ann
mapBuiltin fBuiltin = mapFields id fBuiltin id

mapAnnM ::
  (Applicative f, KnownSort sort) =>
  (forall sort. KnownSort sort => ann sort -> f (ann' sort)) ->
  Tree sort name builtin ann -> f (Tree sort name builtin ann')
mapAnnM fAnn = mapFieldsM pure pure fAnn

mapAnn ::
  (KnownSort sort) =>
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  Tree sort name builtin ann -> Tree sort name builtin ann'
mapAnn fAnn = mapFields id id fAnn


-- -}
-- -}
-- -}
-- -}
-- -}
