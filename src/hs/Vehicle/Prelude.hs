
module Vehicle.Prelude
  ( module X
  , (|->)
  , (!?)
  , rangeStart
  ) where

import Data.Range

import Vehicle.Prelude.Token as X
import Vehicle.Prelude.Sort as X
import Vehicle.Prelude.Provenance as X
import Vehicle.Prelude.Language as X
import Vehicle.Prelude.Error as X
import Vehicle.Prelude.Prettyprinter as X

infix 1 |->

-- | Useful for writing association lists.
(|->) :: a -> b -> (a, b)
(|->) = (,)

-- |Attempts to extract the first element from a bound
boundStart :: Bound a -> Maybe a
boundStart (Bound v Inclusive)= Just v
boundStart (Bound _v Exclusive)= Nothing

-- |Attempts to extract the first element in a range
rangeStart :: Range a -> Maybe a
rangeStart (SingletonRange a)          = Just a
rangeStart (SpanRange lb _ub)          = boundStart lb
rangeStart (LowerBoundRange b)         = boundStart b
rangeStart (UpperBoundRange _)         = Nothing
rangeStart InfiniteRange               = Nothing

(!?) :: Eq a => [(a,b)] -> a -> Maybe b
[] !? _ = Nothing
((k , v) : xs) !? k'
  | k == k'   = Just v
  | otherwise = xs !? k'