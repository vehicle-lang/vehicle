@network
f : Real -> Real

-- Cannot reduce `trueElement <= falseElement` and therefore cannot
-- calculate the logic direction.
CustomLoss : DifferentiableTensorLogic
CustomLoss =
  { trueElement                = f 0
  , falseElement               = 1000000
  , pointwiseNegation          = \x -> -x
  , pointwiseConjunction       = \x y -> max x y
  , pointwiseDisjunction       = \x y -> min x y
  , pointwiseLessThan          = \x y -> x - y
  , pointwiseLessEqualThan     = \x y -> x - y
  , pointwiseGreaterThan       = \x y -> y - x
  , pointwiseGreaterEqualThan  = \x y -> y - x
  , pointwiseEqual             = \x y -> min (x - y) (y - x)
  , pointwiseNotEqual          = \x y -> max (x - y) (y - x)
  , reduceConjunction          = \e xs -> reduceMax e xs
  , reduceDisjunction          = \e xs -> reduceMin e xs
  }
