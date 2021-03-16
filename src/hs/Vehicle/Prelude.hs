module Vehicle.Prelude
  ( module X
  , (|->)
  ) where

import Vehicle.Prelude.Token as X

infix 1 |->

(|->) :: a -> b -> (a, b)
(|->) = (,)
