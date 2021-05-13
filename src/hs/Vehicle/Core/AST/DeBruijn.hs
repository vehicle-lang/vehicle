{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.AST.DeBruijn
  ( Index
  , DeBruijn(..)
  , fromIndex
  , toIndex
  , fromSymbol
  , toSymbol
  ) where

import Vehicle.Core.AST.Core (Sort(..), KnownSort(..), SSort(..))
import Vehicle.Prelude

-- * Types

type Index = Int

type family DEBRUIJN (sort :: Sort) where
  DEBRUIJN 'TYPE = Index
  DEBRUIJN 'EXPR = Index
  DEBRUIJN 'TARG = Symbol
  DEBRUIJN 'EARG = Symbol

newtype DeBruijn (sort :: Sort) = DB { unDB :: DEBRUIJN sort }


-- |Wraps an |Index| as a |DeBruijn|.
fromIndex ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  Index -> DeBruijn sort
fromIndex = case sortSing :: SSort sort of
  STYPE -> DB
  SEXPR -> DB

-- |Returns the |Index| of a |DeBruijn|.
toIndex ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  DeBruijn sort -> Index

toIndex = case sortSing :: SSort sort of
  STYPE -> unDB
  SEXPR -> unDB

-- |Wraps a |Symbol| as a |DeBruijn|.
fromSymbol ::
  forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  Symbol -> DeBruijn sort

fromSymbol = case sortSing :: SSort sort of
  STARG -> DB
  SEARG -> DB

-- |Returns the |Symbol| of a |DeBruijn|.
toSymbol ::
  forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  DeBruijn sort -> Symbol

toSymbol = case sortSing :: SSort sort of
  STARG -> unDB
  SEARG -> unDB
