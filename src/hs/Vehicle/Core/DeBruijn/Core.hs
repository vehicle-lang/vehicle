{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vehicle.Core.DeBruijn.Core
  ( DeBruijn
  , DeBruijnIndex(..)
  , SortedDeBruijn(..)
  , dbValue
  ) where

import Vehicle.Core.Type (Sort(..))
import Vehicle.Core.Abs (Name(..))
import Vehicle.Prelude (Position)

-- * Types

-- |The empty type
data Empty

-- |A de Bruijn representation of variable references.
-- The `Position` refers to the location of the variable reference in the source file.
-- The `Int` is the number of binders between the use of the variable and it binding site.
newtype DeBruijnIndex = DeBruijnIndex (Position, Int)
  deriving (Eq, Ord, Show, Read)

-- |The type of DeBruijn information stored with each sort.
-- Expressions and types store the pointers to the binding sites (EArg and TArg respectively)
-- Binding sites store the original name of the variable.
-- All other locations store nothing.
type family DeBruijn (sort :: Sort) where
  DeBruijn 'TYPE = DeBruijnIndex
  DeBruijn 'EXPR = DeBruijnIndex
  DeBruijn 'DECL = Empty
  DeBruijn 'PROG = Empty
  DeBruijn 'TARG = Name 
  DeBruijn 'EARG = Name 

-- |The de Bruijn representation equipped with sorts
newtype SortedDeBruijn (sort :: Sort) = SortedDeBruijn (DeBruijn sort)

-- |Returns the index value of the SortedDeBruijn index.
dbValue :: SortedDeBruijn 'EXPR -> Int
dbValue (SortedDeBruijn (DeBruijnIndex (_ , l))) = l