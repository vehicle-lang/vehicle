{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE PolyKinds       #-}

module Vehicle.Prelude
  ( module X
  , (|->)
  , (:*:)(..)
  , type DecIn
  , type In
  , K(..)
  , O(..)
  ) where

import Vehicle.Prelude.Token as X

infix 1 |->

-- | Useful for writing association lists.
(|->) :: a -> b -> (a, b)
(|->) = (,)

-- | Indexed product.
data (:*:) (f :: k -> *) (g :: k -> *) (x :: k) = (:*:) { sortedFst :: f x, sortedSnd :: g x }
  deriving (Eq, Ord, Show)

-- |
type family DecIn (x :: k) (xs :: [k]) :: Bool where
  DecIn x '[]       = 'False
  DecIn x (x ': xs) = 'True
  DecIn x (y ': xs) = DecIn x xs

-- |
type In (x :: k) (xs :: [k]) = DecIn x xs ~ 'True

-- |Type-level constant function.
newtype K (a :: *) (b :: k) = K { unK :: a }
  deriving (Eq, Ord, Show)

-- |Type-level function composition.
newtype O (g :: * -> *) (f :: k -> *) (a :: k) = O { unO :: g (f a) }
