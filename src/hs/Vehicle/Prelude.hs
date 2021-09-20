
module Vehicle.Prelude
  ( module X
  , (|->)
  , (!?)
  , (!!?)
  , rangeStart
  , repeatN
  , duplicate
  ) where

import Data.Range

import Vehicle.Prelude.Token as X
import Vehicle.Prelude.Sort as X
import Vehicle.Prelude.Provenance as X
import Vehicle.Prelude.Language as X
import Vehicle.Prelude.Error as X
import Vehicle.Prelude.Prettyprinter as X
import Vehicle.Prelude.Visibility as X
import Vehicle.Prelude.Logging as X
import Vehicle.Prelude.Supply as X

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

(!!?) :: [a] -> Int -> Maybe a
[] !!? _       = Nothing
(x : _)  !!? 0 = Just x
(_ : xs) !!? i = xs !!? (i - 1)

repeatN :: (a -> a) -> Int -> a -> a
repeatN _ 0 = id
repeatN f n = f . repeatN f (n-1)

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string