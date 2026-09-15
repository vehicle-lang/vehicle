@parameter
p : Real

CustomLogic : DifferentiableTensorLogic
CustomLogic =
  { trueElement                = -infinity
  , falseElement               = infinity
  , pointwiseNegation          = \x -> x
  , pointwiseConjunction       = \{dims} x y -> (const (1/p) dims) * (x + y)
  , pointwiseDisjunction       = \x y -> x + y
  , pointwiseLessThan          = \x y -> x + y
  , pointwiseLessEqualThan     = \x y -> x + y
  , pointwiseGreaterThan       = \x y -> x + y
  , pointwiseGreaterEqualThan  = \x y -> x + y
  , pointwiseEqual             = \x y -> x + y
  , pointwiseNotEqual          = \x y -> x + y
  , reduceConjunction          = \xs -> reduceAdd xs
  , reduceDisjunction          = \xs -> reduceAdd xs
  }

@network
f : Real -> Real

@property
property : Bool
property = forall x . 0 < x < 1 => 0 < f x < 1
