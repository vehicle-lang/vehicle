record DifferentiableLogic where
  { true        : Real
  , false       : Real
  , conjunction : Real -> Real -> Real
  , disjunction : Real -> Real -> Real
  , negation    : Real -> Real
  , implication : Real -> Real -> Real
  , lt          : Real -> Real -> Real
  , le          : Real -> Real -> Real
  , gt          : Real -> Real -> Real
  , ge          : Real -> Real -> Real
  , equals      : Real -> Real -> Real
  , notEquals   : Real -> Real -> Real
  }

myLogic : DifferentiableLogic
myLogic =
  { true        = -100000
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
f : Real -> Real

@property
p : Bool
p = forall x . f x >= myLogic.true
