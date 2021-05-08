{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Vehicle.Core.Type.Fields where

import Control.Monad.Identity
import Vehicle.Core.Type.Core
import Vehicle.Core.Type.Recursive


-- * Update fields in one layer

-- |Apply sorted functions to the names, builtins, and annotations in one layer.
updFields ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name' builtin' ann' sort tree)

updFields f g h tree = runIdentity $ updFieldsM (pure . f) (pure . g) (pure . h) tree

-- |Effectful version of |updFields|.
updFieldsM ::
  (Applicative f) =>
  (forall sort. KnownSort sort => name sort -> f (name' sort)) ->
  (forall sort. KnownSort sort => builtin sort -> f (builtin' sort)) ->
  (forall sort. KnownSort sort => ann sort -> f (ann' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name' builtin' ann' sort tree))

updFieldsM f g h (tree :: TreeF name builtin ann sort tree) = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> case tree of
    KAppF  ann k1 k2 -> KAppF  <$> h ann <*> pure k1 <*> pure k2
    KConF  ann op    -> KConF  <$> h ann <*> g op
    KMetaF ann i     -> KMetaF <$> h ann <*> pure i

  -- Types
  STYPE -> case tree of
    TForallF  ann n t   -> TForallF  <$> h ann <*> pure n <*> pure t
    TAppF     ann t1 t2 -> TAppF     <$> h ann <*> pure t1 <*> pure t2
    TVarF     ann n     -> TVarF     <$> h ann <*> f n
    TConF     ann op    -> TConF     <$> h ann <*> g op
    TLitDimF  ann d     -> TLitDimF  <$> h ann <*> pure d
    TLitListF ann ts    -> TLitListF <$> h ann <*> pure ts
    TMetaF    ann i     -> TMetaF    <$> h ann <*> pure i

  -- Type arguments
  STARG -> case tree of
    TArgF ann n -> TArgF <$> h ann <*> f n

  -- Expressions
  SEXPR -> case tree of
    EAnnF     ann e t     -> EAnnF     <$> h ann <*> pure e <*> pure t
    ELetF     ann n e1 e2 -> ELetF     <$> h ann <*> pure n <*> pure e1 <*> pure e2
    ELamF     ann n e     -> ELamF     <$> h ann <*> pure n <*> pure e
    EAppF     ann e1 e2   -> EAppF     <$> h ann <*> pure e1 <*> pure e2
    EVarF     ann n       -> EVarF     <$> h ann <*> f n
    ETyAppF   ann e t     -> ETyAppF   <$> h ann <*> pure e <*> pure t
    ETyLamF   ann n e     -> ETyLamF   <$> h ann <*> pure n <*> pure e
    EConF     ann op      -> EConF     <$> h ann <*> g op
    ELitIntF  ann z       -> ELitIntF  <$> h ann <*> pure z
    ELitRealF ann r       -> ELitRealF <$> h ann <*> pure r
    ELitSeqF  ann es      -> ELitSeqF  <$> h ann <*> pure es

  -- Expression arguments
  SEARG -> case tree of
    EArgF ann n -> EArgF <$> h ann <*> f n

  -- Declarations
  SDECL -> case tree of
    DeclNetwF ann n t    -> DeclNetwF <$> h ann <*> pure n <*> pure t
    DeclDataF ann n t    -> DeclDataF <$> h ann <*> pure n <*> pure t
    DefTypeF  ann n ns t -> DefTypeF  <$> h ann <*> pure n <*> pure ns <*> pure t
    DefFunF   ann n t e  -> DefFunF   <$> h ann <*> pure n <*> pure t <*> pure e

  -- Programs
  SPROG -> case tree of
    MainF ann ds -> MainF <$> h ann <*> pure ds

-- |Apply sorted function to the names in one layer.
updName ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name' builtin ann sort tree)

updName f = updFields f id id

-- |Effectful version |updName|.
updNameM ::
  (Applicative f) =>
  (forall sort. KnownSort sort => name sort -> f (name' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name' builtin ann sort tree))

updNameM f = updFieldsM f pure pure

-- |Apply sorted function to the builtins in one layer.
updBuiltin ::
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name builtin' ann sort tree)

updBuiltin g = updFields id g id

-- |Effectful version |updBuiltin|.
updBuiltinM ::
  (Applicative f) =>
  (forall sort. KnownSort sort => builtin sort -> f (builtin' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name builtin' ann sort tree))

updBuiltinM g = updFieldsM pure g pure

-- |Apply sorted function to the annotation in one layer.
updAnn ::
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> TreeF name builtin ann' sort tree)

updAnn h = updFields id id h

-- |Effectful version |updAnn|.
updAnnM ::
  (Applicative f) =>
  (forall sort. KnownSort sort => ann sort -> f (ann' sort)) ->
  (forall sort. KnownSort sort => TreeF name builtin ann sort tree -> f (TreeF name builtin ann' sort tree))

updAnnM h = updFieldsM pure pure h


-- * Update fields in a tree

-- |Apply sorted functions to all names, builtins, and annotations in a tree.
mapFields ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name' builtin' ann' sort)

mapFields f g h tree = runIdentity $ mapFieldsM (pure . f) (pure . g) (pure . h) tree

-- |Effectful version of |mapFields|.
mapFieldsM ::
  (Monad m) =>
  (forall sort. KnownSort sort => name sort -> m (name' sort)) ->
  (forall sort. KnownSort sort => builtin sort -> m (builtin' sort)) ->
  (forall sort. KnownSort sort => ann sort -> m (ann' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name' builtin' ann' sort))

mapFieldsM f g h = foldTreeM (fmap embed . updFieldsM f g h)

-- |Apply sorted functions to all names in a tree.
mapName ::
  (forall sort. KnownSort sort => name sort -> name' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name' builtin ann sort)
mapName f = mapFields f id id

-- |Effectful version of |mapName|.
mapNameM ::
  (Monad m) =>
  (forall sort. KnownSort sort => name sort -> m (name' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name' builtin ann sort))
mapNameM f = mapFieldsM f pure pure

-- |Apply sorted functions to all builtins in a tree.
mapBuiltin ::
  (forall sort. KnownSort sort => builtin sort -> builtin' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name builtin' ann sort)
mapBuiltin g = mapFields id g id

-- |Effectful version of |mapBuiltin|.
mapBuiltinM ::
  (Monad m) =>
  (forall sort. KnownSort sort => builtin sort -> m (builtin' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name builtin' ann sort))
mapBuiltinM g = mapFieldsM pure g pure

-- |Apply sorted functions to all annotations in a tree.
mapAnn ::
  (forall sort. KnownSort sort => ann sort -> ann' sort) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> Tree name builtin ann' sort)
mapAnn h = mapFields id id h

-- |Effectful version of |mapAnn|.
mapAnnM ::
  (Monad m) =>
  (forall sort. KnownSort sort => ann sort -> m (ann' sort)) ->
  (forall sort. KnownSort sort => Tree name builtin ann sort -> m (Tree name builtin ann' sort))
mapAnnM h = mapFieldsM pure pure h

-- -}
-- -}
-- -}
-- -}
-- -}
