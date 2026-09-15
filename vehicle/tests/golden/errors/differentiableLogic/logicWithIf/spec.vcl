@network
f : Real -> Real

-- Cannot compile as it contains an `if` statement.
CustomLoss : DifferentiableTensorLogic
CustomLoss =
  { trueElement                = 1
  , falseElement               = 30
  , pointwiseNegation          = \x -> if f 0 > 0 then x else -x
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

@property
p : Bool
p = forall x. not (f x > 0)
