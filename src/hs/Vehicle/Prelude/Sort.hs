{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Vehicle.Prelude.Sort
  ( Sort(..)
  , SSort(..)
  , KnownSort(..)
  , toSort
  , sort
  ) where

import Data.Hashable (Hashable(..), hashUsing)


-- | Syntactic sorts used in Vehicle Core syntax.
data Sort = EXPR | DECL | PROG
  deriving (Eq, Ord, Enum, Show)

instance Hashable Sort where
  hashWithSalt = hashUsing fromEnum

-- | Singleton type for 'Sort'.
data SSort (sort :: Sort) where
  SEXPR :: SSort 'EXPR
  SDECL :: SSort 'DECL
  SPROG :: SSort 'PROG

deriving instance (Eq (SSort sort))
deriving instance (Ord (SSort sort))
deriving instance (Show (SSort sort))


class KnownSort (sort :: Sort) where
  sortSing :: SSort sort

instance KnownSort 'EXPR where sortSing = SEXPR
instance KnownSort 'DECL where sortSing = SDECL
instance KnownSort 'PROG where sortSing = SPROG

toSort :: SSort sort -> Sort
toSort SEXPR = EXPR
toSort SDECL = DECL
toSort SPROG = PROG

sort :: forall sort. KnownSort sort => Sort
sort = toSort (sortSing @sort)