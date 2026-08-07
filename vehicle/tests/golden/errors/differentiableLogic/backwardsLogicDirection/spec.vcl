@network
f : Real -> Real

-- Does not represent a loss as `falseElement <= trueElement` and therefore
-- logic is backwards.
CustomLoss : DifferentiableTensorLogic
CustomLoss =
  { trueElement                = 1
  , falseElement               = 0
  , pointwiseNegation          = \x -> -x
  , pointwiseConjunction       = \x y -> max x y
  , pointwiseDisjunction       = \x y -> min x y
  , pointwiseLessThan          = \x y -> x - y
  , pointwiseLessEqualThan     = \x y -> x - y
  , pointwiseGreaterThan       = \x y -> y - x
  , pointwiseGreaterEqualThan  = \x y -> y - x
  , pointwiseEqual             = \x y -> min (x - y) (y - x)
  , pointwiseNotEqual          = \x y -> max (x - y) (y - x)
  , reduceConjunction          = \xs -> reduceMax xs
  , reduceDisjunction          = \xs -> reduceMin xs
  }
