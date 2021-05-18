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
  , Name(..)
  , DeBruijn(..)
  , fromIndex
  , toIndex
  , fromSymbol
  , toSymbol
  ) where

import Vehicle.Core.AST.Core (Sort(..), KnownSort(..), SSort(..))
import Vehicle.Prelude

-- * Types

data Name
  = User Symbol
  | Machine

deriving instance Eq Name

type Index = Int

data DeBruijn (sort :: Sort) where
  TIndex  :: Index -> DeBruijn 'TYPE
  EIndex  :: Index -> DeBruijn 'EXPR
  TSymbol :: Name  -> DeBruijn 'TARG
  ESymbol :: Name  -> DeBruijn 'EARG


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
  STARG -> TSymbol . User
  SEARG -> ESymbol . User

-- |Returns the |Symbol| of a |DeBruijn|.
toSymbol :: forall sort. (KnownSort sort, sort `In` ['TARG, 'EARG]) => DeBruijn sort -> Maybe Symbol
toSymbol (TSymbol (User symbol)) = Just symbol
toSymbol (ESymbol (User symbol)) = Just symbol
toSymbol _                       = Nothing

deriving instance Eq (DeBruijn sort)
