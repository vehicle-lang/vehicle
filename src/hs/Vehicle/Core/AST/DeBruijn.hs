{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

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

data DeBruijn (sort :: Sort) where
  TIndex  :: Index  -> DeBruijn 'TYPE
  EIndex  :: Index  -> DeBruijn 'EXPR
  TSymbol :: Symbol -> DeBruijn 'TARG
  ESymbol :: Symbol -> DeBruijn 'EARG


-- |Wraps an |Index| as a |DeBruijn|.
fromIndex :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Index -> DeBruijn sort
fromIndex = case sortSing :: SSort sort of
  STYPE -> TIndex
  SEXPR -> EIndex

-- |Returns the |Index| of a |DeBruijn|.
toIndex :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => DeBruijn sort -> Index
toIndex (TIndex index) = index
toIndex (EIndex index) = index

-- |Wraps a |Symbol| as a |DeBruijn|.
fromSymbol :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => Symbol -> DeBruijn sort
fromSymbol = case sortSing :: SSort sort of
  STARG -> TSymbol
  SEARG -> ESymbol

-- |Returns the |Symbol| of a |DeBruijn|.
toSymbol :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => DeBruijn sort -> Symbol
toSymbol (TSymbol symbol) = symbol
toSymbol (ESymbol symbol) = symbol

deriving instance Eq (DeBruijn sort)
