{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Vehicle.Prelude
  ( module X
  , (|->)
  , (:*:)(..)
  ) where

import Vehicle.Prelude.Token as X

infix 1 |->

-- | Useful for writing association lists.
(|->) :: a -> b -> (a, b)
(|->) = (,)

-- | Indexed product.
data (:*:) (f :: k -> *) (g :: k -> *) (x :: k) = (:*:) { fst :: f x, snd :: g x }
  deriving (Eq, Ord, Show)
