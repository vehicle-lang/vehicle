{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module Vehicle.Core.Type.Instance.Trifunctor where

import Control.Monad.Identity
import Data.Functor.Foldable (fold)
import Vehicle.Core.Type.Core
import Vehicle.Core.Type.Instance.Recursive


mapTreeM :: (Applicative f) =>
            (forall sort. SSort sort -> name sort -> f (name' sort)) ->
            (forall sort. SSort sort -> builtin sort -> f (builtin' sort)) ->
            (forall sort. SSort sort -> ann sort -> f (ann' sort)) ->
            SSort sort -> Tree sort name builtin ann -> f (Tree sort name' builtin' ann')
mapTreeM fName fBuiltin fAnn = \case

  -- Kinds
  SKIND -> fold $ \case
    KAppF  ann k1 k2 -> KApp  <$> fAnn SKIND ann <*> k1 <*> k2
    KConF  ann op    -> KCon  <$> fAnn SKIND ann <*> fBuiltin SKIND op
    KMetaF ann i     -> KMeta <$> fAnn SKIND ann <*> pure i

  -- Types
  STYPE -> fold $ \case
    TForallF  ann n t   -> TForall  <$> fAnn STYPE ann <*> mapTreeM fName fBuiltin fAnn STARG n <*> t
    TAppF     ann t1 t2 -> TApp     <$> fAnn STYPE ann <*> t1 <*> t2
    TVarF     ann n     -> TVar     <$> fAnn STYPE ann <*> fName STYPE n
    TConF     ann op    -> TCon     <$> fAnn STYPE ann <*> fBuiltin STYPE op
    TLitDimF  ann d     -> TLitDim  <$> fAnn STYPE ann <*> pure d
    TLitListF ann ts    -> TLitList <$> fAnn STYPE ann <*> sequenceA ts
    TMetaF    ann i     -> TMeta    <$> fAnn STYPE ann <*> pure i

  -- Type arguments
  STARG -> \case
    TArg ann n -> TArg <$> fAnn STARG ann <*> fName STARG n

  -- Expressions
  SEXPR -> fold $ \case
    EAnnF     ann e t     -> EAnn     <$> fAnn SEXPR ann <*> e <*> mapTreeM fName fBuiltin fAnn STYPE t
    ELetF     ann n e1 e2 -> ELet     <$> fAnn SEXPR ann <*> mapTreeM fName fBuiltin fAnn SEARG n <*> e1 <*> e2
    ELamF     ann n e     -> ELam     <$> fAnn SEXPR ann <*> mapTreeM fName fBuiltin fAnn SEARG n <*> e
    EAppF     ann e1 e2   -> EApp     <$> fAnn SEXPR ann <*> e1 <*> e2
    EVarF     ann n       -> EVar     <$> fAnn SEXPR ann <*> fName SEXPR n
    ETyAppF   ann e t     -> ETyApp   <$> fAnn SEXPR ann <*> e <*> mapTreeM fName fBuiltin fAnn STYPE t
    ETyLamF   ann n e     -> ETyLam   <$> fAnn SEXPR ann <*> mapTreeM fName fBuiltin fAnn STARG n <*> e
    EConF     ann op      -> ECon     <$> fAnn SEXPR ann <*> fBuiltin SEXPR op
    ELitIntF  ann z       -> ELitInt  <$> fAnn SEXPR ann <*> pure z
    ELitRealF ann r       -> ELitReal <$> fAnn SEXPR ann <*> pure r
    ELitSeqF  ann es      -> ELitSeq  <$> fAnn SEXPR ann <*> sequenceA es

  -- Expression arguments
  SEARG -> \case
    EArg ann n -> EArg <$> fAnn SEARG ann <*> fName SEARG n

  -- Declarations
  SDECL -> \case
    DeclNetw ann n t    -> DeclNetw <$> fAnn SDECL ann <*>
                           mapTreeM fName fBuiltin fAnn SEARG n <*>
                           mapTreeM fName fBuiltin fAnn STYPE t
    DeclData ann n t    -> DeclData <$> fAnn SDECL ann <*>
                           mapTreeM fName fBuiltin fAnn SEARG n <*>
                           mapTreeM fName fBuiltin fAnn STYPE t
    DefType  ann n ns t -> DefType <$> fAnn SDECL ann <*>
                           mapTreeM fName fBuiltin fAnn STARG n <*>
                           traverse (mapTreeM fName fBuiltin fAnn STARG) ns <*>
                           mapTreeM fName fBuiltin fAnn STYPE t
    DefFun   ann n t e  -> DefFun <$> fAnn SDECL ann <*>
                           mapTreeM fName fBuiltin fAnn SEARG n <*>
                           mapTreeM fName fBuiltin fAnn STYPE t <*>
                           mapTreeM fName fBuiltin fAnn SEXPR e

  -- Programs
  SPROG -> \case
    Main ann ds -> Main <$> fAnn SPROG ann <*> traverse (mapTreeM fName fBuiltin fAnn SDECL) ds


mapTree :: (forall sort. SSort sort -> name sort -> name' sort) ->
           (forall sort. SSort sort -> builtin sort -> builtin' sort) ->
           (forall sort. SSort sort -> ann sort -> ann' sort) ->
           SSort sort -> Tree sort name builtin ann -> Tree sort name' builtin' ann'
mapTree fName fBuiltin fAnn ssort tree =
  runIdentity $ mapTreeM ((pure .) . fName) ((pure .) . fBuiltin) ((pure .) . fAnn) ssort tree

mapNameM :: (Applicative f) =>
            (forall sort. SSort sort -> name sort -> f (name' sort)) ->
            SSort sort -> Tree sort name builtin ann -> f (Tree sort name' builtin ann)
mapNameM fName = mapTreeM fName (const pure) (const pure)

mapName :: (forall sort. SSort sort -> name sort -> name' sort) ->
           SSort sort -> Tree sort name builtin ann -> Tree sort name' builtin ann
mapName fName = mapTree fName (const id) (const id)

mapBuiltinM :: (Applicative f) =>
               (forall sort. SSort sort -> builtin sort -> f (builtin' sort)) ->
               SSort sort -> Tree sort name builtin ann -> f (Tree sort name builtin' ann)
mapBuiltinM fBuiltin = mapTreeM (const pure) fBuiltin (const pure)

mapBuiltin :: (forall sort. SSort sort -> builtin sort -> builtin' sort) ->
              SSort sort -> Tree sort name builtin ann -> Tree sort name builtin' ann
mapBuiltin fBuiltin = mapTree (const id) fBuiltin (const id)

mapAnnM :: (Applicative f) => (forall sort. SSort sort -> ann sort -> f (ann' sort)) ->
           SSort sort -> Tree sort name builtin ann -> f (Tree sort name builtin ann')
mapAnnM fAnn = mapTreeM (const pure) (const pure) fAnn

mapAnn :: (forall sort. SSort sort -> ann sort -> ann' sort) ->
          SSort sort -> Tree sort name builtin ann -> Tree sort name builtin ann'
mapAnn fAnn = mapTree (const id) (const id) fAnn


-- -}
-- -}
-- -}
-- -}
-- -}
