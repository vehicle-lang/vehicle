@tensor
record Pair where
  { a : Real
  , b : Real
  }

type Input = Tensor Real [2]
type Output = Tensor Real [2]
-- unused : Bool
-- unused = forall (x : Real) . True

@network
f : Pair -> Pair

-- @network
-- f : Input -> Output

-- pair : Pair
-- pair = { a = 1, b = 1 }


@property
p : Bool
p = (forall x . (f x).a > x.a)
-- @property
-- p : Bool
-- p = (forall x . (f x) ! 0 > x ! 0)