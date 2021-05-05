{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.DeBruijn.Core
  ( DeBruijn
  , SortedDeBruijn(..)
  , Ix(..)
  , ix
  ) where

import Data.Void (Void)
import Vehicle.Core.Type (Sort(..), KnownSort(..), SSort(..))
import Vehicle.Core.Abs (Name(..))
import Vehicle.Prelude (Position)

-- * Types

type Index = Int

-- |A de Bruijn representation of variable references.
-- The `Position` refers to the location of the variable reference in the source file.
-- The `Int` is the number of binders between the use of the variable and it binding site.
newtype Ix = Ix (Position, Index)
  deriving (Eq, Ord, Show, Read)

-- |The type of DeBruijn information stored with each sort.
-- Expressions and types store the pointers to the binding sites (EArg and TArg respectively)
-- Binding sites store the original name of the variable.
-- All other locations store nothing.
type family DeBruijn (sort :: Sort) where
  DeBruijn 'TYPE = Ix
  DeBruijn 'EXPR = Ix
  DeBruijn 'DECL = Void
  DeBruijn 'PROG = Void
  DeBruijn 'TARG = Name
  DeBruijn 'EARG = Name

-- |The de Bruijn representation equipped with sorts
newtype SortedDeBruijn (sort :: Sort) = SortedDeBruijn (DeBruijn sort)

-- |Returns the index value of the SortedDeBruijn index.
ix :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => SortedDeBruijn sort -> Index
ix = case sortSing :: SSort sort of
  STYPE -> \(SortedDeBruijn (Ix (_ , i))) -> i
  SEXPR -> \(SortedDeBruijn (Ix (_ , i))) -> i

-- * Helper functions
--
-- TODO: migrate to their own module

type family DecIn (x :: k) (xs :: [k]) :: Bool where
  DecIn x '[]       = 'False
  DecIn x (x ': xs) = 'True
  DecIn x (y ': xs) = DecIn x xs

type In (x :: k) (xs :: [k]) = DecIn x xs ~ 'True
