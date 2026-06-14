@network
f : Real -> Real


-- Cannot reduce the logic to a single concrete record.
CustomLoss : DifferentiableTensorLogic
CustomLoss = if f 0 > 0 then
  { trueElement                = f 0
  , falseElement               = 30
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
  } else
  { trueElement                = 0
  , falseElement               = 20
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
