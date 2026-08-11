@network
f : Tensor Real [2] -> Tensor Real [2]

sharpness : Real
sharpness = 5.0

eulerE : Real
eulerE = 2.718281828459045

SmoothLoss : DifferentiableTensorLogic
SmoothLoss =
  { trueElement                = -1000000
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
  , reduceConjunction          = \{dims} e xs ->
      let m = reduceMax (sharpness * e) (const sharpness dims * xs) in
      (m + log eulerE
             (exp (sharpness * e - m)
              + reduceAdd 0 (exp (const sharpness dims * xs - const m dims))))
        / sharpness
  , reduceDisjunction          = \{dims} e xs ->
      let m = reduceMax (-sharpness * e) (const (-sharpness) dims * xs) in
      -(m + log eulerE
              (exp (-sharpness * e - m)
               + reduceAdd 0 (exp (const (-sharpness) dims * xs - const m dims))))
        / sharpness
  }

input : Tensor Real [2]
input = [0.5, 0.5]

@property
bounded : Bool
bounded = forall i . f input ! i <= 10
