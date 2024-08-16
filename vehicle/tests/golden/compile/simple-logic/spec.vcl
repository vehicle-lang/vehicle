@lossLogic
myLogic : DifferentiableLogic
myLogic =
  { bool        = Rat
  , true        = -100000
  , false       = 100000
  , conjunction = \x y -> max x y
  , disjunction = \x y -> min x y
  , negation    = \x -> - x
  , implication = \x y -> max (- x) y
  , lt          = \x y -> x - y
  , le          = \x y -> x - y
  , gt          = \x y -> y - x
  , ge          = \x y -> y - x
  , equals      = \x y -> max (x - y) (y - x)
  , notEquals   = \x y -> - (max (x - y) (y - x))
  }

@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = forall x . f x ! 0 >= 0
