{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Prelude.Sort
  ( Sort(..)
  , SSort(..)
  , KnownSort(..)
  , toSort
  , sort
  ) where

import Data.Hashable (Hashable(..), hashUsing)


-- | Syntactic sorts used in Vehicle Core syntax.
data Sort = KIND | TYPE | EXPR | DECL | PROG | TARG | EARG
  deriving (Eq, Ord, Enum, Show)

instance Hashable Sort where
  hashWithSalt = hashUsing fromEnum

-- | Singleton type for 'Sort'.
data SSort (sort :: Sort) where
  SKIND :: SSort 'KIND
  STYPE :: SSort 'TYPE
  SEXPR :: SSort 'EXPR
  SDECL :: SSort 'DECL
  SPROG :: SSort 'PROG
  STARG :: SSort 'TARG
  SEARG :: SSort 'EARG

deriving instance (Eq (SSort sort))
deriving instance (Ord (SSort sort))
deriving instance (Show (SSort sort))

class KnownSort (sort :: Sort) where
  sortSing :: SSort sort

instance KnownSort 'KIND where sortSing = SKIND
instance KnownSort 'TYPE where sortSing = STYPE
instance KnownSort 'EXPR where sortSing = SEXPR
instance KnownSort 'DECL where sortSing = SDECL
instance KnownSort 'PROG where sortSing = SPROG
instance KnownSort 'TARG where sortSing = STARG
instance KnownSort 'EARG where sortSing = SEARG

toSort :: SSort sort -> Sort
toSort SKIND = KIND
toSort STYPE = TYPE
toSort SEXPR = EXPR
toSort SDECL = DECL
toSort SPROG = PROG
toSort STARG = TARG
toSort SEARG = EARG

sort :: forall sort. KnownSort sort => Sort
sort = toSort (sortSing @sort)