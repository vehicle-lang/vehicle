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

module Vehicle.Core.Type.DeBruijn
  ( Index
  , DeBruijn(..)
  , indexOf
  , symbolOf
  ) where

import Vehicle.Core.Type.Core (Sort(..), KnownSort(..), SSort(..))
import Vehicle.Prelude

-- * Types

type Index = Int

data DeBruijn (sort :: Sort) where
  TIdx :: Index  -> DeBruijn 'TYPE
  EIdx :: Index  -> DeBruijn 'EXPR
  TSym :: Symbol -> DeBruijn 'TARG
  ESym :: Symbol -> DeBruijn 'EARG

-- |Returns the index of a |DeBruijn|.
indexOf :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => DeBruijn sort -> Index
indexOf = case sortSing :: SSort sort of
  STYPE -> \(TIdx i) -> i
  SEXPR -> \(EIdx i) -> i

-- |Returns the name of a |DeBruijn|.
symbolOf :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => DeBruijn sort -> Symbol
symbolOf = case sortSing :: SSort sort of
  STARG -> \(TSym n) -> n
  SEARG -> \(ESym n) -> n
