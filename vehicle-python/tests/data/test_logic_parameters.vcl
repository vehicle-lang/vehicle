@parameter
p : Real

capucciAdditive : DifferentiableTensorLogic
capucciAdditive =
  { trueElement                = -infinity
  , falseElement               = infinity
  , pointwiseNegation          = \x -> -x
  , pointwiseConjunction       = \{dims} x y -> (const (1/p) dims) * log(exp(const p dims * x) + exp(const p dims * y))
  , pointwiseDisjunction       = \{dims} x y -> -(const (1/p) dims) * log(exp(const (-p) dims * x) + exp(const (-p) dims * y))
  , pointwiseLessThan          = \x y -> x - y
  , pointwiseLessEqualThan     = \x y -> x - y
  , pointwiseGreaterThan       = \x y -> y - x
  , pointwiseGreaterEqualThan  = \x y -> y - x
  , pointwiseEqual             = \x y -> max (x - y) (y - x)
  , pointwiseNotEqual          = \x y -> - max (x - y) (y - x)
  , reduceConjunction          = \{dims} xs -> (1/p) * log(reduceAdd (exp (const p dims * xs)))
  , reduceDisjunction          = \{dims} xs -> (1/p) * log(reduceAdd (exp (const (-p) dims * xs)))
  }

@network
f : Real -> Real

@property
property : Bool
property = forall x . 0 < x < 1 => 0 < f x < 1
