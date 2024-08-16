record DifferentiableLogic
  { bool :: Type
  , true :: self.bool
  , false :: self.bool
  , conjunction :: self.bool -> self.bool -> self.bool
  , disjunction :: self.bool -> self.bool -> self.bool
  , negation :: self.bool -> self.bool
  , implication :: self.bool -> self.bool -> self.bool
  , lt :: Rat -> Rat -> self.bool
  , le :: Rat -> Rat -> self.bool
  , gt :: Rat -> Rat -> self.bool
  , ge :: Rat -> Rat -> self.bool
  , equals :: Rat -> Rat -> self.bool
  , notEquals :: Rat -> Rat -> self.bool
  }

@differentiableLogic
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
  , le          = self.lt
  , gt          = \x y -> y - x
  , ge          = self.gt
  , equals      = \x y -> self.conjunction (le x y) (ge x y)
  , notEquals   = \x y -> self.negation (self.notEquals x y)
  }

@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = forall x . f x ! 0 >= 0
